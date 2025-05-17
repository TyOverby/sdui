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
  let parameters = Sd_chain.Parameters.basic_component graph in
  let%arr parameters
  and theme = View.Theme.current graph
  and dispatcher = Lease_pool.dispatcher lease_pool
  and get_now = Bonsai.Clock.get_current_time graph
  and inject in
  let generate_action =
    match Form.value parameters with
    | Error _ -> Effect.return None
    | Ok parameters ->
      let parameters = { parameters with Sd_chain.Parameters.seed = Int63.of_int (-1) } in
      let pred =
        Option.map parameters.specific_model ~f:(fun specific_model _host current_model ->
          Sd.Hosts.Current_model.equal current_model specific_model)
      in
      let%bind.Effect image_and_duration =
        dispatcher ?pred (fun get_host ->
          match get_host with
          | Error e -> Effect.return (Error e)
          | Ok (host, _current_model) ->
            let%bind.Effect before = get_now in
            let%bind.Effect result =
              Sd.Txt2img.dispatch
                ~host_and_port:(host :> string)
                (Sd_chain.Parameters.for_txt2img parameters)
            in
            let%bind.Effect after = get_now in
            let duration = Time_ns.abs_diff before after in
            Effect.return
              (match result with
               | Ok (img, _) -> Ok (img, duration)
               | Error e -> Error e))
      in
      let image, duration =
        match image_and_duration with
        | Ok (image, duration) -> Ok image, Some duration
        | Error e -> Error e, None
      in
      let%bind.Effect () = inject image parameters in
      Effect.return duration
  in
  let view ~host_monitor =
    View.vbox
      ~attrs:[ {%css| font-size:20px; |} ]
      [ host_monitor
      ; View.vbox [ Vdom.Node.div [ Form.view parameters ~theme ~reset:Effect.Ignore ] ]
      ]
  in
  ~view, ~generate_action
;;
