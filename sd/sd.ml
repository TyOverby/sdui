open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

module Style = [%css stylesheet {|
  body {
    padding: 0;
    margin: 0;
  }
|}]

let blurry_transparent_background =
  View.Theme.override_constants_for_computation ~f:(fun constants ->
    let make_transparent pct c =
      `Name
        (sprintf
           "color-mix(in oklab, transparent %d%%, %s)"
           pct
           (Css_gen.Color.to_string_css c))
    in
    { constants with
      primary =
        { constants.primary with
          background = make_transparent 0 constants.primary.background
        }
    ; extreme =
        { constants.primary with
          background = make_transparent 0 constants.extreme.background
        }
    })
;;

let component graph =
  let%sub { view = hosts_view; request = request_host; available_hosts } =
    Hosts.component graph
  in
  let%sub { form; form_view } =
    blurry_transparent_background
      (Parameters.component ~request_host ~available_hosts)
      graph
  in
  let%sub { queue_request; view = gallery } =
    Gallery2.component ~request_host ~set_params:(form >>| Form.set) graph
  in
  let%sub submit_effect =
    let%arr form = Bonsai.peek form graph
    and queue_request = queue_request in
    Some
      (match%bind.Effect form with
       | Inactive -> Effect.Ignore
       | Active form ->
         (match Form.value form with
          | Error e -> Effect.print_s [%sexp (e : Error.t)]
          | Ok query -> queue_request query))
  in
  let%arr form_view = form_view
  and submit_effect = submit_effect
  and gallery = gallery
  and hosts_view = hosts_view in
  let on_submit = Option.value submit_effect ~default:Effect.Ignore in
  Vdom.Node.div [ form_view ~on_submit ~hosts_panel:hosts_view; gallery ]
;;
