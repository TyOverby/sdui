open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module Style =
  [%css
  stylesheet
    {|
  body {
    padding: 0;
    margin: 0;
    overflow:clip;
  }

  .app-wrapper {
    width: 100%;
    height:100vh;
    overflow:clip;
    display:flex;
    flex-direction:column;
  }

  .workspace-wrapper {
    overflow-y: scroll;
    /* scroll-snap-type: y proximity; */
  }
|}]

let component graph =
  let%sub { view = hosts_view; available_hosts; set_worker_in_use } =
    Sd.Hosts.component graph
  in
  let hosts = Bonsai.Map.of_set available_hosts graph in
  let lease_pool =
    let on_take =
      let%arr set_worker_in_use = set_worker_in_use in
      fun k _ -> set_worker_in_use k true
    and on_return =
      let%arr set_worker_in_use = set_worker_in_use in
      fun k -> set_worker_in_use k false
    in
    Lease_pool.create (module Sd.Hosts.Host) hosts graph
    |> Lease_pool.advise ~on_take ~on_return
  in
  let%sub view = Pair.component ~pool:lease_pool graph in
  let%arr lease_pool_debug = Lease_pool.debug lease_pool
  and hosts_view = hosts_view
  and view = view
  and theme = View.Theme.current graph
  and clear_all = Lease_pool.clear_all lease_pool
  and get_leased = Bonsai.peek (Lease_pool.leased_out lease_pool) graph in
  Vdom.Node.div
    ~attrs:[ Style.app_wrapper ]
    [ hosts_view
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
    ; View.vbox ~attrs:[ Style.workspace_wrapper ] view
    ]
;;
