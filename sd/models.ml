open! Core
open! Bonsai_web
open Async_kernel
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

type t = string option [@@deriving sexp, yojson, compare]

module Current_model = struct
  module T = struct
    type t = { sd_model_checkpoint : string }
    [@@yojson.allow_extra_fields] [@@deriving yojson, sexp_of]
  end

  let dispatch_set (host_and_port, query) =
    Deferred.Or_error.try_with_join (fun () ->
      let body : Async_js.Http.Method_with_args.t =
        query |> T.yojson_of_t |> Shared.Yojson_safe.to_string |> String |> Some |> Post
      in
      let%map.Deferred.Or_error _response =
        Async_js.Http.request
          ~response_type:Default
          ~headers:[ "Content-Type", "application/json" ]
          ~url:(sprintf "%s/sdapi/v1/options" host_and_port)
          body
      in
      ())
  ;;

  let dispatch_set = Effect.of_deferred_fun dispatch_set

  let dispatch_get host_and_port =
    Deferred.Or_error.try_with_join (fun () ->
      let%bind.Deferred.Or_error response =
        Async_js.Http.get (sprintf "%s/sdapi/v1/options" host_and_port)
      in
      Deferred.Or_error.try_with (fun () ->
        response
        |> Yojson.Safe.from_string
        |> T.t_of_yojson
        |> fun { T.sd_model_checkpoint } ->
        String.split_on_chars ~on:[ ' '; '.' ] sd_model_checkpoint
        |> List.hd_exn
        |> Deferred.return))
  ;;

  let dispatch_get = Effect.of_deferred_fun dispatch_get

  let current ~(request_host : Hosts.request_host Value.t) =
    let%sub r, refresh =
      Bonsai.Edge.Poll.manual_refresh
        (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
        ~effect:
          (let%map request_host = request_host in
           let%bind.Effect work = request_host in
           work.f (fun host -> dispatch_get host))
    in
    let%sub () =
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Every_multiple_of_period_blocking
        ~trigger_on_activate:true
        (Time_ns.Span.of_sec 1.0)
        refresh
    in
    match%arr r with
    | Ok m -> Some m
    | Error e ->
      print_s [%message (e : Error.t)];
      None
  ;;
end

module Model_list = struct
  module Api_response = struct
    type model = { model_name : string }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

    type t = model list [@@deriving of_yojson, sexp_of]
  end

  let dispatch host_and_port =
    let%bind.Deferred.Or_error response =
      Async_js.Http.get (sprintf "%s/sdapi/v1/sd-models" host_and_port)
    in
    Deferred.Or_error.try_with (fun () ->
      response
      |> Yojson.Safe.from_string
      |> Api_response.t_of_yojson
      |> List.map ~f:(fun { model_name } -> model_name)
      |> Deferred.return)
  ;;

  let dispatch_get = Effect.of_deferred_fun dispatch

  let all ~(request_host : Hosts.request_host Value.t) =
    let%sub r, refresh =
      Bonsai.Edge.Poll.manual_refresh
        (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
        ~effect:
          (let%map request_host = request_host in
           let%bind.Effect work = request_host in
           work.f (fun host -> dispatch_get host))
    in
    let%sub () =
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Every_multiple_of_period_blocking
        ~trigger_on_activate:true
        (Time_ns.Span.of_min 1.0)
        refresh
    in
    match%arr r with
    | Ok l ->
      List.filter l ~f:(fun model -> not (String.is_substring model ~substring:"inpaint"))
    | Error _ -> []
  ;;
end

let form ~request_host ~available_hosts =
  let%sub all = Model_list.all ~request_host in
  let%sub state, set_state = Bonsai.state_opt () in
  let%sub current = Current_model.current ~request_host in
  let%sub in_progress, set_in_progress = Bonsai.state false in
  let%sub () =
    Bonsai.Edge.on_change
      ~equal:[%equal: String.Set.t * string option * string option]
      (let%map available_hosts = available_hosts
       and state = state
       and current = current in
       available_hosts, state, current)
      ~callback:
        (let%map set_in_progress = set_in_progress in
         fun (available_hosts, state, _current) ->
           match state with
           | None -> Effect.Ignore
           | Some sd_model_checkpoint ->
             let%bind.Effect () = set_in_progress true in
             let%bind.Effect response =
               available_hosts
               |> Set.to_list
               |> List.map ~f:(fun host ->
                 Current_model.dispatch_set (host, { sd_model_checkpoint }))
               |> Effect.all
             in
             let%bind.Effect () = set_in_progress false in
             (match Or_error.all response with
              | Error e ->
                Effect.print_s [%message "setting the model failed " ~_:(e : Error.t)]
              | Ok _l -> Effect.return ()))
  in
  let%sub theme = View.Theme.current in
  let%sub id = Bonsai.path_id in
  let%arr theme = theme
  and all = all
  and state = state
  and set_state = set_state
  and in_progress = in_progress
  and id = id
  and current = current in
  let current = Option.first_some current state in
  let options =
    print_s [%message (all : string list) (current : string option)];
    List.map all ~f:(fun model_name ->
      let view = Vdom.Node.text model_name in
      let picked =
        match current with
        | None -> false
        | Some state -> String.equal model_name state
      in
      model_name, picked, view)
  in
  let view =
    Kado.Unstable.Input.dropdown
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:
        (Vdom.Attr.many
           [ Custom_form_elements.Label_modifications.muted_label
           ; Vdom.Attr.style (Css_gen.create ~field:"height" ~value:"fit-content")
           ; Custom_form_elements.Label_modifications.Variables.set_all
               ~border:
                 (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
               ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
           ])
      ~title:(Some ("model" ^ if in_progress then " - setting" else ""))
      ~on_change:(fun v -> set_state (Some v))
      ~options
  in
  let form =
    Form.Expert.create
      ~value:(Ok current)
      ~set:set_state
      ~view:(Form.View.of_vdom ~id view)
  in
  form, view
;;
