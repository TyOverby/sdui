open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool
module Images = Sd_chain.Paint.Images
module P = Sd.Parameters.Individual

let editor_view ~parameters (view : Sd_chain.Paint.View.t Bonsai.t) (local_ graph) =
  let pos_prompt =
    P.prompt_form
      ~default:"score_9, score_8_up, score_7_up,\n"
      ~container_attrs:[ {%css| flex-grow: 2 |} ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "prompt" ]
      ~label:"prompt"
      graph
  in
  let override_prompt =
    let%arr { Form.value; _ } = pos_prompt in
    fun (params : Sd_chain.Parameters.t) ->
      match value with
      | Ok pos_prompt -> { params with pos_prompt }
      | Error _ -> params
  in
  let _ : unit Bonsai.t =
    Bonsai_extra.exactly_once
      (let%arr { Form.set = set_prompt; _ } = pos_prompt
       and prompt =
         parameters >>| fun { Sd_chain.Parameters.pos_prompt; _ } -> pos_prompt
       in
       set_prompt prompt)
      graph
  in
  let view =
    let%arr { widget
            ; color_picker
            ; pen_size_slider
            ; layer_panel
            ; clear_button
            ; forward_button = _
            ; padding = _
            }
      =
      view
    and pos_prompt in
    View.vbox
      [ pos_prompt.view
      ; View.hbox
          [ layer_panel
          ; widget
          ; View.vbox [ color_picker; pen_size_slider; clear_button ]
          ]
      ]
  in
  override_prompt, view
;;

let component
  ~(lease_pool :
      ( Sd.Hosts.Host.t
        , Sd.Hosts.Current_model.t
        , Sd.Hosts.Host.comparator_witness )
        Lease_pool.t)
  ~inject
  ~id
  ~img
  ~parent_img
  ~is_image_editor
  ~parameters
  ~add_seen
  ~add_seen_after_active
  ~refine_card
  ~reimagine_card
  ~upscale_card
  ~other_model_card
  (local_ graph)
  =
  add_seen_after_active ~add_seen ~id graph;
  let get_images, override_prompt, img_view =
    if is_image_editor
    then (
      let editor = Sd_chain.Paint.component ~prev:parent_img graph in
      let override_prompt, view = editor_view editor.view ~parameters graph in
      editor.get_images, override_prompt, view)
    else (
      let get_images =
        let%arr img in
        Effect.return { Images.image = img; mask = None }
      in
      get_images, Bonsai.return Fn.id, img >>| Sd.Image.to_vdom)
  in
  let%arr parameters
  and theme = View.Theme.current graph
  and models = Lease_pool.all lease_pool >>| Map.data
  and id
  and dispatcher = Lease_pool.dispatcher lease_pool
  and inject
  and get_images
  and img_view
  and ~modifier:modify_refine, ~view:refine_view = refine_card
  and ~modifier:modify_reimagine, ~view:reimagine_view = reimagine_card
  and ~modifier:modify_upscale, ~view:upscale_view = upscale_card
  and ~modifier:modify_other_model, ~view:other_model_view = other_model_card
  and override_prompt in
  let generate_button ~button_text ~kind ~modify_parameters =
    let generate =
      let%bind.Effect parameters = modify_parameters parameters in
      let parameters = override_prompt parameters in
      let%bind.Effect { image = img; mask } = get_images in
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
                 ; mask
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
           ; stage = { desc = kind; state = Enqueued }
           ; dispatch
           ; on_complete
           })
    in
    View.button theme ~on_click:generate button_text, generate
  in
  let upscale_button, upscale =
    generate_button
      ~button_text:"[u]pscale"
      ~kind:(Img2img "upscaled")
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        let params : Sd_chain.Parameters.t = modify_upscale params in
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
      ~kind:(Img2img "refined")
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        Effect.return (modify_refine params))
  in
  let reimagine_button, reimagine =
    generate_button
      ~button_text:"re[i]magine"
      ~kind:(Img2img "reimagined")
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        Effect.return (modify_reimagine params))
  in
  let switch_model_button, switch_model =
    generate_button
      ~button_text:"[o]ther model"
      ~kind:(Img2img "model")
      ~modify_parameters:(fun (params : Sd_chain.Parameters.t) ->
        let params : Sd_chain.Parameters.t = modify_other_model params in
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
  let edit_button, edit =
    let effect =
      let dispatch ~id:_ ~on_started:_ =
        let%bind.Effect { image = img; mask = _ } = get_images in
        Effect.return (Ok img)
      in
      let on_complete image =
        Image_tree.Stage.State.Finished { image; parent_image = Some image; parameters }
      in
      inject
        (Image_tree.Action.Add
           { parent_id = id
           ; stage = { desc = Edit; state = Enqueued }
           ; dispatch
           ; on_complete
           })
    in
    let view = View.button theme "[p]aint" ~on_click:effect in
    view, effect
  in
  let view =
    View.hbox
      ~attrs:[ {%css|margin: 0.5em|} ]
      ~gap:(`Em_float 0.5)
      [ img_view
      ; Vdom.Node.div ~attrs:[ {%css|flex-grow:1;|} ] []
      ; View.hbox
          ~gap:(`Em_float 0.5)
          [ upscale_view ~button:upscale_button
          ; refine_view ~button:refine_button
          ; reimagine_view ~button:reimagine_button
          ; other_model_view ~button:switch_model_button
          ; edit_button
          ]
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
      ; { keys = [ Vdom_keyboard.Keystroke.create' KeyP ]
        ; description = "edit"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ -> edit)
        }
      ]
  in
  view, key_commands
;;
