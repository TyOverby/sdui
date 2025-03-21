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
  ~inject
  ~id
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
  and id
  and dispatcher = Lease_pool.dispatcher lease_pool
  and inject in
  let generate_button, generate_action =
    match Form.value parameters with
    | Error e -> Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e), Effect.Ignore
    | Ok parameters ->
      let generate =
        let dispatch ~id ~on_started =
          let parameters =
            { parameters with
              Sd_chain.Parameters.seed = Int63.of_int (Image_tree.Unique_id.to_int_exn id)
            }
          in
          let pred =
            Option.map
              parameters.specific_model
              ~f:(fun specific_model _host current_model ->
                Sd.Hosts.Current_model.equal current_model specific_model)
          in
          dispatcher ?pred (fun get_host ->
            match get_host with
            | Error e -> Effect.return (Error e)
            | Ok (host, _current_model) ->
              let%bind.Effect () = on_started in
              (match%map.Effect
                 Sd.Txt2img.dispatch
                   ~host_and_port:(host :> string)
                   (Sd_chain.Parameters.for_txt2img parameters)
               with
               | Ok (img, _) -> Ok img
               | Error e -> Error e))
        in
        let on_complete image =
          Image_tree.Stage.State.Finished { image; parent_image = Some image; parameters }
        in
        inject
          (Image_tree.Action.Add
             { parent_id = id
             ; stage = { desc = Txt2img; state = Enqueued }
             ; dispatch
             ; on_complete
             })
      in
      ( View.button
          theme
          ~attrs:[ Vdom.Attr.create "data-kind" "generate" ]
          ~on_click:generate
          "generate"
      , generate )
  in
  let view ~state_tree =
    View.hbox
      [ View.vbox
          [ Vdom.Node.div
              [ Form.view parameters ~direction:`Horizontal ~theme ~reset:Effect.Ignore ]
          ; generate_button
          ]
      ; state_tree
      ]
  in
  let key_commands =
    Vdom_keyboard.Keyboard_event_handler.of_command_list_exn
      [ { keys = [ Vdom_keyboard.Keystroke.create' Enter ]
        ; description = "generate"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ -> generate_action)
        }
      ]
  in
  view, key_commands
;;
