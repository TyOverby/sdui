open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module _ = Problem

module Style = [%css stylesheet {|
  body {
    padding: 0;
    margin: 0;
  }
|}]

let component graph =
  let%sub { view = hosts_view; request = request_host; available_hosts } =
    Sd.Hosts.component graph
  in
  let%sub worker_pool = Worker_pool.component ~hosts:available_hosts graph in
  let parameters, model_form =
    Sd.Parameters.component ~request_host ~available_hosts graph
  in
  let%sub submit_effect =
    let%arr parameters_form = Bonsai.peek (parameters >>| Form.value) graph
    and model_form = Bonsai.peek (model_form >>| Form.value) graph
    and sleep = Bonsai.Clock.sleep graph
    and worker_pool = worker_pool in
    Some
      (match%bind.Effect Effect.Let_syntax.Let_syntax.both parameters_form model_form with
       | Active (Ok _parameters), Active (Ok model) ->
         Worker_pool.enqueue worker_pool ~spec:model () ~f:(fun host () ->
           let%bind.Effect () =
             Effect.print_s [%message "got a host, sleeping" (host : Sd.Hosts.Host.t)]
           in
           let%bind.Effect () = sleep (Time_ns.Span.of_sec 2.0) in
           Effect.print_s [%message "done!" (host : Sd.Hosts.Host.t)])
       | _ -> Effect.print_s [%message "BUG" [%here]])
  in
  let%arr parameters = parameters
  and submit_effect = submit_effect
  and worker_pool = worker_pool
  and hosts_view = hosts_view in
  let on_submit = Option.value submit_effect ~default:Effect.Ignore in
  Vdom.Node.div
    [ (Form.view parameters) ~on_submit ~hosts_panel:hosts_view
    ; Worker_pool.debug worker_pool
    ]
;;
