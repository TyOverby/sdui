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
  let%arr hosts_view = hosts_view
  and view = view
  and lease_pool_view = Lease_pool_small_viz.component ~pool:lease_pool
  and theme = View.Theme.current graph
  and clear_all = Lease_pool.clear_all lease_pool
  and get_leased = Bonsai.peek (Lease_pool.leased_out lease_pool) graph in
  let clear_queue =
    let%bind.Effect () = clear_all in
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
       | Error e -> print_s [%message (e : Error.t)])
  in
  Vdom.Node.div
    ~attrs:[ Style.app_wrapper ]
    [ View.hbox
        [ hosts_view
        ; lease_pool_view
        ; View.button theme "clear queue" ~on_click:clear_queue
        ]
    ; View.vbox ~attrs:[ Style.workspace_wrapper ] view
    ]
;;
