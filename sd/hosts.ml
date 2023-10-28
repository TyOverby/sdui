open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Host = String
module Form = Bonsai_web_ui_form

module Work = struct
  type t =
    { host : Host.t
    ; f : 'a. (Host.t -> 'a Or_error.t Effect.t) -> 'a Or_error.t Effect.t
    }

  let sexp_of_t { host; _ } = Sexp.Atom host
end

type request_host = Work.t Effect.t

type t =
  { view : Vdom.Node.t
  ; request : request_host
  }

let health_check host =
  let%map.Effect response = Samplers_request.dispatch host in
  match response with
  | Ok (_ : string list) -> `Good
  | Error _ -> `Bad
;;

let find_permutation map permutations =
  List.map permutations ~f:(fun permutation ->
    Option.map (Map.find map permutation) ~f:(fun result -> permutation, result))
  |> List.reduce ~f:Option.first_some
  |> Option.join
;;

let component =
  let%sub hosts, textbox = Custom_form_elements.textarea ~label:"hosts" () in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map hosts = hosts in
         Form.set hosts "localhost")
      ()
  in
  let%sub hosts =
    let%arr hosts = hosts in
    hosts
    |> Form.value_or_default ~default:""
    |> String.split_lines
    |> List.map ~f:String.strip
    |> List.filter ~f:(function
      | "" -> false
      | _ -> true)
    |> List.map ~f:(fun s ->
      match String.split s ~on:':' with
      | [ ("http" | "https"); _name; _port ] -> s
      | [ ("http" | "https"); _name ] -> s ^ ":7860"
      | [ _name; _port ] -> "http://" ^ s
      | [ _name ] -> "http://" ^ s ^ ":7860"
      | _ -> s)
  in
  let%sub host_status, inject_host_status =
    Bonsai.state_machine1
      ~default_model:String.Map.empty
      ~apply_action:(fun ctx input model -> function
        | `Report_health (host, `Pending) ->
          Map.change model host ~f:(function
            | None | Some (`Bad | `Pending) -> Some `Pending
            | Some (`Good | `Good_pending) -> Some `Good_pending)
        | `Report_health (host, health) -> Map.set model ~key:host ~data:health
        | `Check_host host ->
          Bonsai.Apply_action_context.schedule_event
            ctx
            (let%bind.Effect () =
               Bonsai.Apply_action_context.inject ctx (`Report_health (host, `Pending))
             in
             let%bind.Effect health = health_check host in
             Bonsai.Apply_action_context.inject ctx (`Report_health (host, health)));
          model
        | `Remove host -> Map.remove model host
        | `Check_all ->
          let effect =
            match input with
            | Active hosts ->
              let check_health_effect =
                hosts
                |> List.map ~f:(fun host ->
                  Bonsai.Apply_action_context.inject ctx (`Check_host host))
                |> Effect.Many
              in
              let clear_effect =
                Set.diff (Map.key_set model) (String.Set.of_list hosts)
                |> Set.to_list
                |> List.map ~f:(fun host ->
                  Bonsai.Apply_action_context.inject ctx (`Remove host))
                |> Effect.Many
              in
              Effect.Many [ check_health_effect; clear_effect ]
            | Inactive -> Effect.Ignore
          in
          Bonsai.Apply_action_context.schedule_event ctx effect;
          model)
      hosts
  in
  let%sub () =
    let%sub callback =
      let%arr inject_host_status = inject_host_status in
      inject_host_status `Check_all
    in
    let%sub () =
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Wait_period_after_previous_effect_finishes_blocking
        ~trigger_on_activate:true
        (Time_ns.Span.of_sec 5.0)
        callback
    in
    Bonsai.Edge.on_change
      hosts
      ~equal:[%equal: string list]
      ~callback:
        (let%map callback = callback in
         fun _ -> callback)
  in
  let%sub available =
    let%sub good_hosts =
      Bonsai.Map.filter_map host_status ~f:(function
        | `Good | `Good_pending -> Some ()
        | `Bad | `Pending -> None)
    in
    return good_hosts
  in
  let%sub write, read = Bonsai_extra.pipe (module Work) in
  let%sub workers =
    Bonsai.assoc
      (module String)
      available
      ~f:(fun host _data ->
        let%sub working, set_working = Bonsai.state false in
        let%sub register_self =
          let%arr write = write
          and host = host
          and set_working = set_working in
          write
            { Work.host
            ; f =
                (fun cb ->
                  let%bind.Effect () = set_working true in
                  let%bind.Effect the_result = cb host in
                  let%bind.Effect () = set_working false in
                  Effect.return the_result)
            }
        in
        let%sub () =
          Bonsai.Edge.on_change
            working
            ~equal:equal_bool
            ~callback:
              (let%map register_self = register_self in
               function
               | true -> Effect.Ignore
               | false -> register_self)
        in
        return working)
  in
  let%sub view =
    let%sub theme = View.Theme.current in
    let%arr textbox = textbox
    and host_status = host_status
    and workers = workers
    and theme = theme in
    let highlight s =
      String.split_lines s
      |> List.map ~f:(fun line ->
        let color, busy =
          let stripped = String.strip line in
          let lookup =
            find_permutation
              host_status
              [ stripped
              ; "http://" ^ stripped
              ; "http://" ^ stripped ^ ":7860"
              ; stripped ^ ":7860"
              ]
          in
          match lookup with
          | None -> (View.extreme_colors theme).foreground, false
          | Some (_host, (`Bad | `Pending)) ->
            let color = (View.intent_colors theme Warning).background in
            color, false
          | Some (host, (`Good | `Good_pending)) ->
            let color = (View.intent_colors theme Success).background in
            let busy = Map.find workers host |> Option.value ~default:true in
            color, busy
        in
        Vdom.Node.span
          ~attrs:
            [ Vdom.Attr.style (Css_gen.color color)
            ; (if busy then Vdom.Attr.style (Css_gen.opacity 0.5) else Vdom.Attr.empty)
            ]
          [ Vdom.Node.text line ])
      |> List.intersperse ~sep:(Vdom.Node.text "\n")
    in
    textbox ~colorize:highlight ()
  in
  let%arr view = view
  and read = read in
  { view; request = read }
;;
