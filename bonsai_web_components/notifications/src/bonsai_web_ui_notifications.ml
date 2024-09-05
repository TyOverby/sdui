open! Core

module type Model = Bonsai_web.Proc.Model

open! Bonsai_web
open! Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
      .notification_container {
        position: fixed;
        bottom: 20px;
        right: 20px;
      }

      .notification_container .notification {
        color: white;
        cursor: pointer;
        text-align: right;
      }

      .notification_container .notification:not(:last-child) {
        margin-bottom: 20px;
      }

      .notification_body {
        padding: 15px;
      }

      .notification_body > pre {
        background-color: unset;
        font-size: x-small;
        margin-bottom: 0;
      }

      @keyframes fadeOut {
        0% {
          opacity: 1;
        }

        100% {
          opacity: 0;
        }
      }
      |}]

module Notification_id = Bonsai_extra.Id_gen (Int) ()

module Id = struct
  type _ t = Notification_id.t
end

module Notification = struct
  type 'a t =
    { content : 'a
    ; opened_at : Time_ns.Alternate_sexp.t
    ; close_after : Time_ns.Span.t option
    }
  [@@deriving equal, sexp]
end

module Action = struct
  type 'a t =
    | Add of
        { id : Notification_id.t
        ; notification : 'a Notification.t
        }
    | Remove of Notification_id.t
    | Remove_all
    | Remove_oldest
    | Remove_newest
  [@@deriving equal, sexp]
end

type 'a t =
  { notifications : 'a Notification.t Map.M(Notification_id).t
  ; inject : 'a Action.t -> unit Effect.t
  ; send_notification : ?close_after:Time_ns.Span.t -> 'a -> Notification_id.t Effect.t
  ; modify_notification :
      ?close_after:Time_ns.Span.t -> 'a Id.t -> 'a -> Notification_id.t Effect.t
  }

let component (type a) (module M : Model with type t = a) ~equal graph =
  let id_generator = Notification_id.component graph in
  let notifications, inject =
    let module Model = struct
      type a = M.t

      let equal_a = equal
      let sexp_of_a = M.sexp_of_t

      type t = a Notification.t Map.M(Notification_id).t [@@deriving equal, sexp_of]
    end
    in
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:(Action.sexp_of_t M.sexp_of_t)
      ~default_model:(Map.empty (module Notification_id))
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) notifications action ->
        match action with
        | Add { id; notification } -> Map.set notifications ~key:id ~data:notification
        | Remove notification_id -> Map.remove notifications notification_id
        | Remove_all -> Notification_id.Map.empty
        | Remove_oldest ->
          (match Map.keys notifications with
           | oldest_id :: _ -> Map.remove notifications oldest_id
           | [] -> notifications)
        | Remove_newest ->
          (match Map.keys notifications |> List.last with
           | Some newest_id -> Map.remove notifications newest_id
           | None -> notifications))
  in
  let%sub () =
    let%sub (_ : (Notification_id.t, unit, _) Map.t) =
      Bonsai.assoc
        (module Notification_id)
        notifications
        ~f:(fun notification_id notification graph ->
          let%sub { opened_at; close_after; content = _ } = notification in
          match%sub close_after with
          | None -> Bonsai.return ()
          | Some close_after ->
            let span =
              let%arr close_after = close_after
              and opened_at = opened_at in
              Time_ns.add opened_at close_after
            in
            let at = Bonsai.Clock.at span graph in
            let callback =
              let%arr notification_id = notification_id
              and inject = inject in
              function
              | Bonsai.Clock.Before_or_after.Before -> Effect.Ignore
              | After -> inject (Action.Remove notification_id)
            in
            Bonsai.Edge.on_change
              ~sexp_of_model:[%sexp_of: Bonsai.Clock.Before_or_after.t]
              ~equal:[%equal: Bonsai.Clock.Before_or_after.t]
              at
              ~callback
              graph;
            Bonsai.return ())
        graph
    in
    Bonsai.return ()
  in
  let send_and_modify_notifications =
    let now = Bonsai.Clock.get_current_time graph in
    let%arr inject = inject
    and id_generator = id_generator
    and now = now in
    let modify_notification ?close_after id content =
      let open Effect.Let_syntax in
      let%bind now = now in
      let notification = { Notification.content; opened_at = now; close_after } in
      let%bind () = inject (Add { id; notification }) in
      return id
    in
    let send_notification ?close_after content =
      let%bind.Effect id = id_generator in
      modify_notification ?close_after id content
    in
    send_notification, modify_notification
  in
  let%arr notifications = notifications
  and inject = inject
  and send_notification, modify_notification = send_and_modify_notifications in
  { notifications; inject; send_notification; modify_notification }
;;

(* [render_with_access_to_entire_notification] is just like render, but the [f] rendering
   function also has access to the internally meaningful id of the notificaition and also
   the data that is known about the notificaiton like when we expect it to closed an also
   when it was sent. *)
let render_with_access_to_entire_notification
  ?(notification_container_extra_attr = Bonsai.return Vdom.Attr.empty)
  t
  ~f
  graph
  =
  let%sub { notifications; inject; send_notification = _; modify_notification = _ } = t in
  let rendered =
    Bonsai.assoc
      (module Notification_id)
      notifications
      ~f:(fun notification_id notification graph ->
        let close_effect =
          let%arr notification_id = notification_id
          and inject = inject in
          inject (Action.Remove notification_id)
        in
        let rendered = f ~close:close_effect ~id:notification_id notification graph in
        let%arr rendered = rendered
        and data = notification in
        rendered, data.opened_at)
      graph
  in
  let%arr rendered = rendered
  and notification_container_extra_attr = notification_container_extra_attr in
  Map.to_alist rendered
  |> List.map ~f:(fun (notification_id, (rendered, _)) ->
    Vdom.Node.div
      ~key:(Notification_id.to_string notification_id)
      ~attrs:[ Style.notification ]
      [ rendered ])
  |> Vdom.Node.div
       ~attrs:[ Style.notification_container; notification_container_extra_attr ]
;;

let render t ~f graph =
  render_with_access_to_entire_notification
    t
    ~f:(fun ~close ~id:_ notification graph ->
      let content =
        let%arr notification = notification in
        notification.Notification.content
      in
      f ~close content graph)
    graph
;;

let send_notification
  ?close_after
  { send_notification; notifications = _; inject = _; modify_notification = _ }
  =
  send_notification ?close_after
;;

let close_notification
  { inject; notifications = _; send_notification = _; modify_notification = _ }
  id
  =
  inject (Remove id)
;;

let close_all_notifications
  { inject; notifications = _; send_notification = _; modify_notification = _ }
  =
  inject Remove_all
;;

let close_oldest_notification
  { inject; notifications = _; send_notification = _; modify_notification = _ }
  =
  inject Remove_oldest
;;

let close_newest_notification
  { inject; notifications = _; send_notification = _; modify_notification = _ }
  =
  inject Remove_newest
;;

let modify_notification
  ?close_after
  { modify_notification; inject = _; notifications = _; send_notification = _ }
  id
  content
  =
  Effect.ignore_m (modify_notification ?close_after id content)
;;

module Basic = struct
  module Level = struct
    type t =
      | Success
      | Error of Error.t option
    [@@deriving equal, sexp]
  end

  module Basic_notification = struct
    type t =
      { text : string
      ; level : Level.t
      }
    [@@deriving equal, sexp]

    let success text = { text; level = Success }
    let error ?error text = { text; level = Error error }
  end

  type basic_t =
    { notifications : Basic_notification.t t
    ; dismiss_notifications_after : Time_ns.Span.t
    ; dismiss_errors_automatically : bool
    }

  module Notification_style =
    [%css
    stylesheet
      {|
        .success {
          background-color: #00ab66;
        }

        .error {
          background-color: #c70039;
        }
        |}]

  let create
    ?(dismiss_notifications_after : Time_ns.Span.t Bonsai.t =
      Bonsai.return (Time_ns.Span.of_sec 15.0))
    ?(dismiss_errors_automatically : bool Bonsai.t = Bonsai.return false)
    ()
    graph
    =
    let notifications =
      component (module Basic_notification) ~equal:[%equal: Basic_notification.t] graph
    in
    let%arr notifications = notifications
    and dismiss_notifications_after = dismiss_notifications_after
    and dismiss_errors_automatically = dismiss_errors_automatically in
    { notifications; dismiss_notifications_after; dismiss_errors_automatically }
  ;;

  let add_error ?error (t : basic_t) ~text =
    let { notifications; dismiss_notifications_after; dismiss_errors_automatically } =
      t
    in
    let close_after =
      match dismiss_errors_automatically with
      | false -> None
      | true -> Some dismiss_notifications_after
    in
    Ui_effect.ignore_m
      (send_notification
         ?close_after
         notifications
         (Basic_notification.error ?error text))
  ;;

  let add_success (t : basic_t) ~text =
    let { notifications; dismiss_notifications_after; dismiss_errors_automatically = _ } =
      t
    in
    let close_after = Some dismiss_notifications_after in
    Ui_effect.ignore_m
      (send_notification ?close_after notifications (Basic_notification.success text))
  ;;

  module type Style = Notification_style.S

  let default_module : (module Style) = (module Notification_style)

  let render
    ?(notification_style = default_module)
    ?(notification_extra_attr = Bonsai.return Vdom.Attr.empty)
    ?notification_container_extra_attr
    (t : basic_t Bonsai.t)
    graph
    =
    let module Notification_style = (val notification_style) in
    let%sub { notifications
            ; dismiss_notifications_after = _
            ; dismiss_errors_automatically = _
            }
      =
      t
    in
    render_with_access_to_entire_notification
      notifications
      ?notification_container_extra_attr
      ~f:(fun ~close ~id:notification_id notification _graph ->
        let%arr { Notification.content = { text; level }; opened_at = _; close_after } =
          notification
        and close = close
        and notification_id = notification_id
        and notification_extra_attr = notification_extra_attr in
        let level_class =
          match level with
          | Success -> Notification_style.success
          | Error _ -> Notification_style.error
        in
        Vdom.Node.div
          ~key:(Notification_id.to_string notification_id)
          ~attrs:
            [ Vdom.Attr.(
                Style.notification
                @ on_click (fun _ -> close)
                @ notification_extra_attr
                @ create
                    "data-notification-id"
                    (Notification_id.to_string notification_id))
            ]
          [ Vdom.Node.div
              ~attrs:
                [ Vdom.Attr.(
                    many [ Style.notification_body; level_class ]
                    @
                    match close_after with
                    | None -> Vdom.Attr.empty
                    | Some close_after ->
                      style
                        (let open Css_gen in
                         animation
                           ~name:"fadeOut"
                           ~duration:close_after
                           ~timing_function:"ease-in"
                           ()))
                ]
              [ Vdom.Node.text text
              ; (match level with
                 | Success | Error None -> Vdom.Node.None
                 | Error (Some error) ->
                   Vdom.Node.pre [ Vdom.Node.text (Error.to_string_hum error) ])
              ]
          ])
      graph
  ;;

  type t = basic_t
end
