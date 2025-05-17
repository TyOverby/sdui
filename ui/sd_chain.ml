open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Parameters = Parameters
module Form = Bonsai_web_ui_form.With_manual_view
module Lease_pool = Lease_pool
module Paint = Paint
module Toplayer = Bonsai_web_ui_toplayer

module _ =
  [%css
  stylesheet
    ~don't_hash:[ "popover_dom__inline_class_hash_2a0c95f160" ]
    {|
body {
  overflow-y: clip;
}

.popover_dom__inline_class_hash_2a0c95f160 {
  height: unset !important;
}
|}]

let hosts_and_queue (local_ graph) =
  let%sub { view = hosts_view; available_hosts; set_worker_in_use } =
    Sd.Hosts.component graph
  in
  let hosts_popover, hosts_popover_controls =
    Toplayer.Popover.create
      ~overflow_auto_wrapper:(Bonsai.return true)
      ~match_anchor_side_length:(Bonsai.return None)
        (* ~alignment:(Bonsai.return Toplayer.Alignment.Start) *)
      ~position:(Bonsai.return Toplayer.Position.Bottom)
      ~content:(fun ~close:_ _graph -> hosts_view)
      graph
  in
  let hosts_view =
    let%arr hosts_popover
    and open_ = hosts_popover_controls.open_ in
    Vdom.Node.div
      ~attrs:
        [ hosts_popover
        ; Vdom.Attr.on_click (fun _ -> open_)
        ; {%css| margin: 0.5em; cursor:pointer; |}
        ]
      [ Feather_icon.svg ~stroke_width:(`Px 1) ~size:(`Px 20) Edit ]
  in
  let lease_pool =
    let on_take =
      let%arr set_worker_in_use in
      fun k _ -> set_worker_in_use k true
    and on_return =
      let%arr set_worker_in_use in
      fun k -> set_worker_in_use k false
    in
    Lease_pool.create (module Sd.Hosts.Host) available_hosts graph
    |> Lease_pool.advise ~on_take ~on_return
  in
  let clear_queue =
    let%arr clear_all = Lease_pool.clear_all lease_pool
    and get_leased = Bonsai.peek (Lease_pool.leased_out lease_pool) graph in
    let%bind.Effect () = clear_all in
    match%bind.Effect get_leased with
    | Inactive -> Effect.Ignore
    | Active leased ->
      let%map.Effect skip_result =
        Map.keys leased
        |> List.map ~f:(fun host -> Skip.dispatch_effect ~host)
        |> Effect.all
      in
      (match Or_error.all_unit skip_result with
       | Ok () -> ()
       | Error e -> print_s [%message (e : Error.t)])
  in
  let queue_view =
    Lease_pool_small_viz.component
      ~data_to_string:Sd.Hosts.Current_model.to_string
      ~pool:lease_pool
  in
  lease_pool, hosts_view, queue_view, clear_queue
;;

let component (local_ graph) =
  let lease_pool, hosts_view, queue_view, clear_queue = hosts_and_queue graph in
  let lease_pool_view =
    let%arr clear_queue
    and queue_view
    and theme = View.Theme.current graph in
    View.hbox [ queue_view; View.button theme "clear" ~on_click:clear_queue ]
  in
  Navigation.component ~pool:lease_pool ~hosts_view ~lease_pool_view graph
;;
