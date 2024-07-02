open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
.container {
    position: fixed;
    bottom: 0;
    right: 0;
    max-height:50%;
    overflow-y: scroll;
    overflow-x: hidden;
    border-radius: 9px;
    border: 1px solid rgb(49, 57, 67);
    padding-left: 20px;
  }

.queued_item {
  border-bottom: 1px solid rgb(49, 57, 67);
}

.remove_queued_button {
  margin-left: 20px;
}
|}]

type t =
  { view : Vdom.Node.t
  ; preview_view : Vdom.Node.t option
  ; queue_request : Txt2img.Query.t -> unit Effect.t
  }

let component ~host_and_port ~add_images =
  let%sub ongoing, inject_ongoing =
    Bonsai.state_machine0
      ~default_model:`Uninitialized
      ~apply_action:(fun _ctx state action ->
        match state, action with
        | `Uninitialized, `In_progress -> `Preexisting
        | s, `In_progress -> s
        | _, `Not_running -> `Idle
        | `Idle, `Dispatched params -> `Running params
        | _, `Dispatched _ ->
          raise_s [%message "New requests can only be dispatched while idle."])
      ()
  in
  let%sub (_, requests), modify_requests =
    (* TODO: allow reordering requests by reinserting the key as the average of the new neighbor keys. *)
    Bonsai.state_machine0
      ~default_model:(0., Map.empty (module Float))
      ~apply_action:(fun _ctx (next_idx, map) ->
        function
        | `Queue params -> next_idx +. 100., Map.add_exn map ~key:next_idx ~data:params
        | `Pop idx -> next_idx, Map.remove map idx)
      ()
  in
  let%sub next_request =
    let%arr requests = requests in
    Map.min_elt requests
  in
  let%sub progress = Progress.state ~host_and_port in
  let%sub handle_queue_effect =
    let%arr ongoing = ongoing
    and inject_ongoing = inject_ongoing
    and next_request = next_request
    and modify_requests = modify_requests
    and host_and_port = host_and_port
    and add_images = add_images
    and progress = progress in
    match ongoing, next_request with
    | `Uninitialized, _ | `Preexisting, _ ->
      (match progress with
       | Error _ -> Effect.Ignore
       | Ok { Progress.progress; _ } when Float.equal progress 0. ->
         inject_ongoing `Not_running
       | Ok _ -> inject_ongoing `In_progress)
    | `Running _, _ | `Idle, None -> Effect.Ignore
    | `Idle, Some (idx, params) ->
      let open Effect.Let_syntax in
      let%bind () = inject_ongoing (`Dispatched params) in
      let%bind () = modify_requests (`Pop idx) in
      let%bind result =
        match%bind Txt2img.dispatch ~host_and_port params with
        | Ok images -> add_images ~params ~images:(List.map images ~f:Result.return)
        | Error e -> add_images ~params ~images:[ Error e ]
      in
      let%bind () = inject_ongoing `Not_running in
      return result
  in
  let%sub () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:true
      (Time_ns.Span.of_sec 0.1)
      handle_queue_effect
  in
  let%sub requests =
    Bonsai.assoc
      (module Float)
      requests
      ~f:(fun idx request ->
        let%arr idx = idx
        and request = request
        and modify_requests = modify_requests in
        View.hbox
          [ Vdom.Node.div
              ~attrs:[ Style.queued_item ]
              [ Vdom.Node.sexp_for_debugging ([%sexp_of: Txt2img.Query.t] request) ]
          ; Vdom.Node.button
              ~attrs:
                [ Style.remove_queued_button
                ; Vdom.Attr.on_click (fun _ -> modify_requests (`Pop idx))
                ]
              [ Vdom.Node.text "X" ]
          ])
  in
  let%sub view =
    let%arr requests = requests in
    if Map.is_empty requests
    then Vdom.Node.None
    else
      View.vbox
        ~attrs:[ Style.container ]
        [ Vdom.Node.h2 [ Vdom.Node.text "Request Queue" ]
        ; Vdom_node_with_map_children.make ~tag:"div" requests
        ]
  in
  let%sub preview_view =
    match%sub ongoing with
    | `Uninitialized | `Idle -> Bonsai.const None
    | `Preexisting -> Preview.component progress
    | `Running params -> Preview.component ~params progress
  in
  let%sub queue_request =
    let%arr modify_requests = modify_requests in
    fun params -> modify_requests (`Queue params)
  in
  let%arr view = view
  and preview_view = preview_view
  and queue_request = queue_request in
  { view; preview_view; queue_request }
;;
