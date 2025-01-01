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
  ~img
  ~parameters
  ~add_seen
  ~add_seen_after_active
  (local_ graph)
  =
  add_seen_after_active ~add_seen ~id graph;
  let%arr parameters
  and theme = View.Theme.current graph
  and models = Lease_pool.all lease_pool >>| Map.data
  and id
  and dispatcher = Lease_pool.dispatcher lease_pool
  and inject
  and img in
  let generate_button ~button_text ~stack_text ~modify_parameters =
    let generate =
      let%bind.Effect parameters = modify_parameters parameters in
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
               Sd.Img2img.dispatch
                 ~host_and_port:(host :> string)
                 { (Sd_chain.Parameters.for_img2img parameters) with
                   init_images = [ img ]
                 }
             with
             | Ok ((img, _) :: _) -> Ok img
             | Ok [] -> Error (Error.of_string "no images")
             | Error e -> Error e))
      in
      let on_complete image =
        Image_tree.Stage.State.Finished { image; parent_image = Some img; parameters }
      in
      inject
        (Image_tree.Action.Add
           { parent_id = id
           ; stage = { desc = stack_text; state = Enqueued }
           ; dispatch
           ; on_complete
           })
    in
    View.button theme ~on_click:generate button_text, generate
  in
  let upscale_button, upscale =
    generate_button
      ~button_text:"[u]pscale"
      ~stack_text:"upscaled"
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        Effect.return
          { params with
            width = Int63.(params.width * of_int 2)
          ; height = Int63.(params.height * of_int 2)
          ; denoise = Int63.of_int 30
          ; steps = Int63.of_int 50
          })
  in
  let refine_button, refine =
    generate_button
      ~button_text:"ref[y]ne"
      ~stack_text:"refined"
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        Effect.return
          { params with
            denoise = Int63.of_int 50
          ; steps = Int63.(params.steps * of_int 3 / of_int 2)
          ; cfg = Int63.of_int 5
          })
  in
  let reimagine_button, reimagine =
    generate_button
      ~button_text:"re[i]magine"
      ~stack_text:"reimagined"
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        Effect.return
          { params with
            denoise = Int63.of_int 70
          ; steps = Int63.of_int 25
          ; cfg = Int63.of_int 10
          })
  in
  let switch_model_button, switch_model =
    generate_button
      ~button_text:"[o]ther model"
      ~stack_text:"model"
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        let%bind.Effect new_model =
          Effect.of_thunk (fun () ->
            match params.specific_model with
            | None -> List.random_element models
            | Some prev_model ->
              models
              |> List.filter ~f:(fun model ->
                not (Sd.Hosts.Current_model.equal model prev_model))
              |> List.random_element)
        in
        Effect.return
          { params with denoise = Int63.of_int 40; specific_model = new_model })
  in
  let view =
    View.hbox
      [ Sd.Image.to_vdom img
      ; View.vbox [ upscale_button; refine_button; reimagine_button; switch_model_button ]
      ]
  in
  let key_commands =
    Vdom_keyboard.Keyboard_event_handler.of_command_list_exn
      [ { keys = [ Vdom_keyboard.Keystroke.create' KeyY ]
        ; description = "refine"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ -> refine)
        }
      ; { keys = [ Vdom_keyboard.Keystroke.create' KeyU ]
        ; description = "upscale"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ -> upscale)
        }
      ; { keys = [ Vdom_keyboard.Keystroke.create' KeyI ]
        ; description = "reimagine"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ -> reimagine)
        }
      ; { keys = [ Vdom_keyboard.Keystroke.create' KeyO ]
        ; description = "switch model"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ -> switch_model)
        }
      ]
  in
  view, key_commands
;;
