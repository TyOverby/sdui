open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Host = struct
  include String
end

module Form = Bonsai_web_ui_form.With_manual_view

type t =
  { view : Vdom.Node.t
  ; available_hosts : Host.Set.t
  ; set_worker_in_use : Host.t -> bool -> unit Effect.t
  }

let random_healthy_host =
  Effect.of_sync_fun (fun { available_hosts; _ } ->
    List.random_element (Set.to_list available_hosts))
;;

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

let component graph =
  let hosts_form = Custom_form_elements.textarea ~label:"hosts" graph in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%map hosts_form = hosts_form in
       Form.set hosts_form "localhost")
    graph;
  let%sub hosts =
    let%arr hosts = hosts_form in
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
  let host_status, inject_host_status =
    Bonsai.state_machine1
      ~default_model:String.Map.empty
      ~apply_action:(fun ctx input model ->
        function
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
      graph
  in
  let%sub callback =
    let%arr inject_host_status = inject_host_status in
    inject_host_status `Check_all
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Wait_period_after_previous_effect_finishes_blocking
    ~trigger_on_activate:true
    (Time_ns.Span.of_sec 5.0)
    callback
    graph;
  Bonsai.Edge.on_change
    hosts
    ~equal:[%equal: string list]
    ~callback:
      (let%map callback = callback in
       fun _ -> callback)
    graph;
  let available =
    let good_hosts =
      Bonsai.Map.filter_map host_status graph ~f:(function
        | `Good | `Good_pending -> Some ()
        | `Bad | `Pending -> None)
    in
    good_hosts
  in
  let worker_state, inject_worker_state =
    Bonsai.state_machine0
      graph
      ~default_model:Host.Map.empty
      ~apply_action:(fun _ctx model (key, data) -> Map.set model ~key ~data)
  in
  let view =
    let%arr textbox = hosts_form
    and host_status = host_status
    and worker_state = worker_state
    and theme = View.Theme.current graph in
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
          | None | Some (_, (`Bad | `Pending)) ->
            let color = (View.intent_colors theme Warning).background in
            color, false
          | Some (host, (`Good | `Good_pending)) ->
            let color = (View.intent_colors theme Success).background in
            let busy = Map.find worker_state host |> Option.value ~default:true in
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
    (Form.view textbox) ~colorize:highlight ()
  in
  let%arr view = view
  and available = available
  and inject_worker_state = inject_worker_state in
  let set_worker_in_use k v = inject_worker_state (k, v) in
  { view; set_worker_in_use; available_hosts = Map.key_set available }
;;
