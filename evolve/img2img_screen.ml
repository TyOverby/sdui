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
      ~default:"! score_9, score_8_up, score_7_up,\n"
      ~container_attrs:[ {%css| flex-grow: 2 |} ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "prompt" ]
      ~label:"prompt"
      graph
  in
  let neg_prompt =
    P.prompt_form
      ~default:"! score_1, score_2, score_3,\n"
      ~container_attrs:[ {%css| flex-grow: 2 |} ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "neg-prompt" ]
      ~label:"neg-prompt"
      graph
  in
  let override_prompt =
    let%arr { Form.value = pos_prompt; _ } = pos_prompt
    and { Form.value = neg_prompt; _ } = neg_prompt in
    fun (params : Sd_chain.Parameters.t) ->
      match pos_prompt, neg_prompt with
      | Ok pos_prompt, Error _ -> { params with pos_prompt }
      | Error _, Ok neg_prompt -> { params with neg_prompt }
      | Ok pos_prompt, Ok neg_prompt -> { params with pos_prompt; neg_prompt }
      | Error _, Error _ -> params
  in
  let _ : unit Bonsai.t =
    Bonsai_extra.exactly_once
      (let%arr { Form.set = set_pos_prompt; _ } = pos_prompt
       and { Form.set = set_neg_prompt; _ } = neg_prompt
       and { Sd_chain.Parameters.pos_prompt; neg_prompt; _ } = parameters in
       Effect.Many [ set_pos_prompt pos_prompt; set_neg_prompt neg_prompt ])
      graph
  in
  let view =
    let%arr { widget
            ; color_picker
            ; pen_size_slider
            ; blur_radius_slider
            ; layer_panel
            ; clear_button
            ; forward_button = _
            ; alt_panel
            ; flip_button
            ; clone_button
            ; blur_button
            }
      =
      view
    and pos_prompt
    and neg_prompt in
    View.hbox
      [ layer_panel
      ; widget
      ; View.vbox
          [ color_picker
          ; pen_size_slider
          ; alt_panel
          ; flip_button
          ; clone_button
          ; blur_radius_slider
          ; blur_button
          ; clear_button
          ]
      ; View.vbox [ Form.view pos_prompt; Form.view neg_prompt ]
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
  ~resize_card
  ~controlnet_fix_card
  (local_ graph)
  =
  add_seen_after_active ~add_seen ~id graph;
  let get_images, override_prompt, img_view, set_paint_image =
    if is_image_editor
    then (
      let editor = Sd_chain.Paint.component ~prev:parent_img graph in
      let override_prompt, view = editor_view editor.view ~parameters graph in
      editor.get_images, override_prompt, view, editor.set_paint_image)
    else (
      let get_images =
        let%arr img in
        Effect.return { Images.image = img; mask = None; blur_mask = None }
      in
      get_images, Bonsai.return Fn.id, img >>| Sd.Image.to_vdom, Bonsai.return None)
  in
  let%arr parameters
  and theme = View.Theme.current graph
  and models = Lease_pool.all lease_pool >>| Map.data
  and id
  and dispatcher = Lease_pool.dispatcher lease_pool
  and inject
  and get_images
  and img_view
  and ~params:controlnet_params, ~view:controlnet_view = controlnet_fix_card
  and ~modifier:modify_refine, ~view:refine_view = refine_card
  and ~modifier:modify_reimagine, ~view:reimagine_view = reimagine_card
  and ~modifier:modify_upscale, ~view:upscale_view = upscale_card
  and ~modifier:modify_other_model, ~view:other_model_view = other_model_card
  and resize_card, _resize_effect = resize_card
  and override_prompt
  and set_paint_image in
  let ctrlnet_detect, _ctrlnet_detect_effect =
    let generate =
      let%bind.Effect { image = img; mask = _; blur_mask = _ } = get_images in
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
            let module_ =
              match controlnet_params with
              | ( ~module_:(Ok (Some module_))
                , ~model:_
                , ~weight:_
                , ~start_point:_
                , ~end_point:_ ) -> Sd.Controlnet_modules.to_string module_
              | _ -> "none"
            in
            (match%map.Effect
               Sd.Controlnet_detect.dispatch
                 ~host_and_port:(host :> string)
                 { image = img; module_ }
             with
             | Ok img -> Ok img
             | Error e -> Error e))
      in
      let on_complete image =
        Image_tree.Stage.State.Finished { image; parent_image = Some img; parameters }
      in
      inject
        (Image_tree.Action.Add
           { parent_id = id
           ; stage = { desc = Ctrlnet; state = Enqueued }
           ; dispatch
           ; on_complete
           })
    in
    View.button theme ~on_click:generate "detect", generate
  in
  let generate_button ~button_text ~kind ~modify_parameters =
    let generate =
      let%bind.Effect { image = img; mask; blur_mask = _ } = get_images in
      let%bind.Effect parameters = modify_parameters ~parameters ~image:img in
      let parameters = override_prompt parameters in
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
                 { (Sd_chain.Parameters.for_img2img parameters) with image = img; mask }
             with
             | Ok (img, _) -> Ok img
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
      ~modify_parameters:(fun ~(parameters : Sd_chain.Parameters.t) ~image ->
        let params : Sd_chain.Parameters.t = modify_upscale ~parameters ~image in
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
      ~modify_parameters:(fun ~parameters ~image ->
        Effect.return (modify_refine ~parameters ~image))
  in
  let reimagine_button, reimagine =
    generate_button
      ~button_text:"re[i]magine"
      ~kind:(Img2img "reimagined")
      ~modify_parameters:(fun ~parameters ~image ->
        Effect.return (modify_reimagine ~parameters ~image))
  in
  let switch_model_button, switch_model =
    generate_button
      ~button_text:"[o]ther model"
      ~kind:(Img2img "model")
      ~modify_parameters:(fun ~parameters ~image ->
        let params : Sd_chain.Parameters.t = modify_other_model ~parameters ~image in
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
        let%bind.Effect { image = img; mask = _; blur_mask = _ } = get_images in
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
  let resize_card =
    let set_result ~new_width ~new_height img =
      let parameters = { parameters with width = new_width; height = new_height } in
      let on_complete image =
        Image_tree.Stage.State.Finished { image; parent_image = Some img; parameters }
      in
      inject
        (Image_tree.Action.Add
           { parent_id = id
           ; stage = { desc = Resize; state = Enqueued }
           ; dispatch = (fun ~id:_ ~on_started:_ -> Effect.return (Ok img))
           ; on_complete
           })
    in
    resize_card ~get_images ~set_result
  in
  let view =
    View.hbox
      ~attrs:[ {%css|margin: 0.5em; align-items: flex-start;|} ]
      ~gap:(`Em_float 0.5)
      [ img_view
      ; Vdom.Node.div ~attrs:[ {%css|flex-grow:1;|} ] []
      ; View.hbox_wrap
          ~row_gap:(`Em_float 0.5)
          ~column_gap:(`Em_float 0.5)
          ~cross_axis_alignment:Start
          [ upscale_view ~button:upscale_button
          ; refine_view ~button:refine_button
          ; reimagine_view ~button:reimagine_button
          ; other_model_view ~button:switch_model_button
          ; controlnet_view ~button:ctrlnet_detect
          ; resize_card
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
  view, key_commands, set_paint_image
;;
