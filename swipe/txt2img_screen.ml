open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool

let component
  ~(lease_pool :
      ( Sd.Hosts.Host.t
        , Sd.Hosts.Current_model.t
        , Sd.Hosts.Host.comparator_witness )
        Lease_pool.t)
  ~(inject : (Sd.Image.t Or_error.t -> Sd_chain.Parameters.t -> unit Effect.t) Bonsai.t)
  (local_ graph)
  =
  let models =
    let%arr hosts = Lease_pool.all lease_pool in
    hosts |> Map.data |> Sd.Hosts.Current_model.Set.of_list
  in
  let samplers =
    Sd.Samplers.all ~hosts:(Lease_pool.all lease_pool) graph
    >>| Or_error.ok
    >>| Option.value ~default:[]
  in
  let parameters = Sd_chain.Parameters.component ~samplers ~models graph in
  let%arr parameters
  and theme = View.Theme.current graph
  and dispatcher = Lease_pool.dispatcher lease_pool
  and inject in
  let generate_action =
    match Form.value parameters with
    | Error _ -> Effect.Ignore
    | Ok parameters ->
      let parameters = { parameters with Sd_chain.Parameters.seed = Int63.of_int (-1) } in
      let pred =
        Option.map parameters.specific_model ~f:(fun specific_model _host current_model ->
          Sd.Hosts.Current_model.equal current_model specific_model)
      in
      let%bind.Effect image =
        dispatcher ?pred (fun get_host ->
          match get_host with
          | Error e -> Effect.return (Error e)
          | Ok (host, _current_model) ->
            (match%map.Effect
               Sd.Txt2img.dispatch
                 ~host_and_port:(host :> string)
                 (Sd_chain.Parameters.for_txt2img parameters)
             with
             | Ok (img, _) -> Ok img
             | Error e -> Error e))
      in
      inject image parameters
  in
  let view ~host_monitor =
    View.vbox
      [ host_monitor
      ; View.vbox
          [ Vdom.Node.div
              [ Form.view parameters ~direction:`Horizontal ~theme ~reset:Effect.Ignore ]
          ]
      ]
  in
  ~view, ~generate_action
;;
