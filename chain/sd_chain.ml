open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

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
  let hosts = Bonsai.Map.of_set available_hosts graph in
  let lease_pool = Lease_pool.create (module Sd.Hosts.Host) hosts graph in
  let parameters, model_form =
    Sd.Parameters.component ~request_host ~available_hosts graph
  in
  let submit_effect =
    let%arr parameters_form = Bonsai.peek (parameters >>| Form.value) graph
    and model_form = Bonsai.peek (model_form >>| Form.value) graph
    and sleep = Bonsai.Clock.sleep graph
    and dispatcher = Lease_pool.dispatcher lease_pool in
    Some
      (match%bind.Effect Effect.Let_syntax.Let_syntax.both parameters_form model_form with
       | Active (Ok _parameters), Active (Ok _model) ->
         dispatcher (function
           | Error e -> Effect.alert (Error.to_string_hum e)
           | Ok (host, ()) ->
             let%bind.Effect () =
               Effect.print_s [%message "got a host, sleeping" (host : Sd.Hosts.Host.t)]
             in
             let%bind.Effect () = sleep (Time_ns.Span.of_sec 2.0) in
             Effect.print_s [%message "done!" (host : Sd.Hosts.Host.t)])
       | parameters, model ->
         Effect.print_s
           [%message
             "BUG"
               [%here]
               (parameters : Sd.Txt2img.Query.t Or_error.t Bonsai.Computation_status.t)
               (model : Sd.Models.t Or_error.t Bonsai.Computation_status.t)])
  in
  let%sub view = Pair.component ~pool:lease_pool graph in
  let%arr parameters = parameters
  and submit_effect = submit_effect
  and lease_pool_debug = Lease_pool.debug lease_pool
  and hosts_view = hosts_view
  and view = view
  and theme = View.Theme.current graph
  and clear_all = Lease_pool.clear_all lease_pool
  and get_leased = Bonsai.peek (Lease_pool.leased_out lease_pool) graph in
  let on_submit = Option.value submit_effect ~default:Effect.Ignore in
  Vdom.Node.div
    [ (Form.view parameters) ~on_submit ~hosts_panel:hosts_view
    ; Vdom.Node.div
        ~attrs:
          [ {%css| position: fixed; top:1em; left: 1em; background: black; padding:0.25em; border-radius:0.25em; z-index:1; |}
          ]
        [ Vdom.Node.sexp_for_debugging lease_pool_debug
        ; View.button
            theme
            "clear queue"
            ~on_click:
              (let%bind.Effect () = clear_all in
               match%bind.Effect get_leased with
               | Inactive -> Effect.Ignore
               | Active leased ->
                 let%map.Effect skip_result =
                   Set.to_list leased
                   |> List.map ~f:(fun host -> Skip.dispatch_effect ~host)
                   |> Effect.all
                 in
                 (match Or_error.all_unit skip_result with
                  | Ok () -> ()
                  | Error e -> print_s [%message (e : Error.t)]))
        ]
    ; view
    ]
;;
