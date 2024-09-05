open! Core
open Virtual_dom
open Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
      .connected {
        display: none;
      }

      .warning {
        font-size: 1.5rem;
        font-weight: bold;
      }
      |}]

let message_for_async_durable time_span =
  sprintf
    "You've been disconnected from the server for %s. There is no need to refresh the \
     page, since the web client will reconnect automatically when the server becomes \
     available again."
    (Time_ns.Span.to_string_hum ~decimals:0 time_span)
;;

let custom ~create_message is_connected graph =
  if%sub is_connected
  then Bonsai.return (Vdom.Node.div ~attrs:[ Style.connected ] [])
  else (
    let activation_time, set_activation_time =
      Bonsai.state_opt
        graph
        ~sexp_of_model:[%sexp_of: Time_ns.Alternate_sexp.t]
        ~equal:[%equal: Time_ns.Alternate_sexp.t]
    in
    let now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.0) graph in
    let () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr set_activation_time = set_activation_time
           and now = now >>| Option.some in
           set_activation_time now)
        graph
    in
    let%arr now = now
    and activation_time = activation_time
    and create_message = create_message in
    let duration_of_visibility =
      Time_ns.diff now (Option.value ~default:now activation_time)
    in
    create_message duration_of_visibility)
;;

let component ?(styles = Vdom.Attr.empty) ~create_message is_connected =
  custom
    is_connected
    ~create_message:
      (Bonsai.return (fun duration_of_visibility ->
         Vdom.Node.div
           ~attrs:[ styles ]
           [ Vdom.Node.div ~attrs:[ Style.warning ] [ Vdom.Node.text "Warning!" ]
           ; Vdom.Node.div [ Vdom.Node.text (create_message duration_of_visibility) ]
           ]))
;;
