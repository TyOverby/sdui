open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool
module Images = Sd_chain.Paint.Images
module P = Sd.Parameters.Individual
module Size_tracker = Bonsai_web_ui_element_size_hooks.Size_tracker
module Toplayer = Bonsai_web_ui_toplayer

let editor_view_for_ctrlnet_image
  ~remove_button
  ~clone_current_button
  ~clone_parent_button
  (view : Sd_chain.Paint.View.t Bonsai.t)
  =
  let%arr { widget
          ; color_picker
          ; pen_size_slider
          ; blur_radius_slider = _
          ; layer_panel = _
          ; clear_button = _
          ; forward_button = _
          ; alt_panel
          ; flip_button
          ; clone_button
          ; blur_button = _
          }
    =
    view
  and clone_current_button
  and clone_parent_button
  and remove_button in
  View.hbox
    [ widget
    ; View.vbox
        [ color_picker
        ; pen_size_slider
        ; alt_panel
        ; flip_button
        ; clone_current_button
        ; clone_parent_button
        ; clone_button
        ; remove_button
        ]
    ]
;;

let prompt_boxes ?(textarea_attrs = []) ?(container_attrs = []) ~parameters (local_ graph)
  =
  let make_prompt ~default ~label =
    P.prompt_form
      ~default
      ~container_attrs:({%css| flex-shrink: 0; |} :: container_attrs)
      ~textarea_attrs:(Vdom.Attr.create "data-kind" label :: textarea_attrs)
      ~label
      graph
  in
  let pos_prompt =
    make_prompt ~label:"prompt" ~default:(parameters >>| Sd_chain.Parameters.pos_prompt)
  in
  let neg_prompt =
    make_prompt
      ~label:"neg-prompt"
      ~default:(parameters >>| Sd_chain.Parameters.neg_prompt)
  in
  ignore
    (Bonsai_extra.exactly_once
       (let%arr { Form.set = set_pos_prompt; _ } = pos_prompt
        and { Form.set = set_neg_prompt; _ } = neg_prompt
        and { Sd_chain.Parameters.pos_prompt; neg_prompt; _ } = parameters in
        Effect.Many [ set_pos_prompt pos_prompt; set_neg_prompt neg_prompt ])
       graph);
  ~pos_prompt, ~neg_prompt
;;

let editor_view_with_parameters
  ~parameters
  (view : Sd_chain.Paint.View.t Bonsai.t)
  (local_ graph)
  =
  let ~pos_prompt, ~neg_prompt = prompt_boxes ~parameters graph in
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
  let view =
    let%arr { widget
            ; color_picker
            ; pen_size_slider
            ; blur_radius_slider
            ; layer_panel
            ; clear_button
            ; forward_button
            ; alt_panel
            ; flip_button
            ; clone_button
            ; blur_button
            }
      =
      view
    and pos_prompt
    and neg_prompt in
    ( ~pos_prompt
    , ~neg_prompt
    , ~widget
    , ~color_picker
    , ~pen_size_slider
    , ~blur_radius_slider
    , ~layer_panel
    , ~clear_button
    , ~forward_button
    , ~alt_panel
    , ~flip_button
    , ~clone_button
    , ~blur_button )
  in
  override_prompt, view
;;

let extend_ctrlnet_setter ~dispatcher ~controlnet_params ~f image =
  dispatcher (fun get_host ->
    match get_host with
    | Error _e -> Effect.return ()
    | Ok (host, _current_model) ->
      let module_ =
        match controlnet_params with
        | ~module_:(Ok (Some module_)), ~model:_, ~weight:_, ~start_point:_, ~end_point:_
          -> Sd.Controlnet_modules.to_string module_
        | _ -> "none"
      in
      (match%bind.Effect
         Sd.Controlnet_detect.dispatch
           ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
           { image; module_ }
       with
       | Ok image -> f image
       | Error _ -> Effect.return ()))
;;

let image_editor_specialization
  ~parent_img
  ~parameters
  ~lease_pool
  ~controlnet_fix_card
  graph
  =
  let editor = Sd_chain.Paint.component ~prev:parent_img graph in
  let ctrlnet_image, set_ctrlnet_image = Bonsai.state_opt graph in
  let ctrlnet_editor =
    Sd_chain.Paint.component
      ~prev:(ctrlnet_image >>| Option.value ~default:Sd.Image.empty)
      graph
  in
  let override_prompt, view = editor_view_with_parameters editor.view ~parameters graph in
  let view =
    let generic_clone_button ~label ~get_image =
      let%arr theme = View.Theme.current graph
      and set_ctrlnet_image
      and dispatcher = Lease_pool.dispatcher lease_pool
      and get_image
      and ~params:controlnet_params, ~view:_ = controlnet_fix_card in
      let on_click =
        let%bind.Effect image = get_image in
        extend_ctrlnet_setter
          ~dispatcher:(fun f -> dispatcher f)
          ~controlnet_params
          ~f:(fun image -> set_ctrlnet_image (Some image))
          image
      in
      View.button theme label ~on_click
    in
    let clone_parent_button =
      generic_clone_button ~label:"clone parent" ~get_image:(parent_img >>| Effect.return)
    in
    let clone_current_button =
      generic_clone_button
        ~label:"clone parent"
        ~get_image:
          (let%arr get_images = editor.get_images in
           let%map.Effect { image; mask = _; blur_mask = _ } = get_images in
           image)
    in
    let remove_button =
      let%arr theme = View.Theme.current graph
      and set_ctrlnet_image in
      View.button theme "remove" ~on_click:(set_ctrlnet_image None)
    in
    let%arr ctrlnet_editor_view =
      editor_view_for_ctrlnet_image
        ~remove_button
        ~clone_current_button
        ~clone_parent_button
        ctrlnet_editor.view
    and view
    and ctrlnet_image in
    match ctrlnet_image with
    | None -> `Editor_view (view, None)
    | Some _ -> `Editor_view (view, Some ctrlnet_editor_view)
  in
  let set_paint_image =
    let%arr editor_setter = editor.requesting_set_paint_image
    and ctrlnet_setter = ctrlnet_editor.requesting_set_paint_image
    and ~params:controlnet_params, ~view:_ = controlnet_fix_card
    and set_ctrlnet_image
    and dispatcher = Lease_pool.dispatcher lease_pool in
    let set_controlnet image ~f =
      extend_ctrlnet_setter
        ~dispatcher:(fun f -> dispatcher f)
        ~controlnet_params
        ~f:(fun image ->
          let%bind.Effect () = set_ctrlnet_image (Some image) in
          f image)
        image
    in
    match editor_setter with
    | Some f -> Some f
    | None ->
      (match ctrlnet_setter with
       | None -> None
       | Some f -> Some (set_controlnet ~f))
  in
  let get_ctrlnet =
    let%arr get_ctrlnet = ctrlnet_editor.get_images
    and ctrlnet_image in
    if Option.is_none ctrlnet_image
    then Effect.return None
    else (
      let%map.Effect { image; mask = _; blur_mask = _ } = get_ctrlnet in
      if Sd.Image.is_empty image then None else Some image)
  in
  ( ~get_images:editor.get_images
  , ~get_ctrlnet
  , ~override_prompt
  , ~view
  , ~set_ctrlnet_image:(Some set_ctrlnet_image)
  , ~set_paint_image )
;;

let ctrlnet_detect
  ~get_images
  ~parameters
  ~id
  ~inject
  ~lease_pool
  ~controlnet_fix_card
  ~set_ctrlnet_image
  graph
  =
  let%sub ctrlnet_detect, _ctrlnet_detect_effect =
    let%arr get_images
    and parameters
    and id
    and inject
    and dispatcher = Lease_pool.dispatcher lease_pool
    and ~params:controlnet_params, ~view:_ = controlnet_fix_card
    and set_ctrlnet_image = Bonsai.transpose_opt set_ctrlnet_image
    and theme = View.Theme.current graph in
    let generate =
      let%bind.Effect { Images.image = img; mask = _; blur_mask = _ } = get_images in
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
                 ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
                 { image = img; module_ }
             with
             | Ok img -> Ok img
             | Error e -> Error e))
      in
      match set_ctrlnet_image with
      | Some setter ->
        let%bind.Effect result = dispatch ~id ~on_started:Effect.Ignore in
        (match result with
         | Ok image -> setter (Some image)
         | Error _e -> Effect.Ignore)
      | None ->
        let on_complete image =
          Image_tree.Stage.State.Finished { image; parent_image = Some img; parameters }
        in
        inject
          (Image_tree.Action.Add
             { parent_id = id
             ; stage = { desc = Ctrlnet; state = Enqueued { parameters } }
             ; dispatch
             ; on_complete
             ; parameters
             })
    in
    View.button theme ~on_click:generate "detect", generate
  in
  ctrlnet_detect
;;

let prev_inner_size_var = Bonsai.Expert.Var.create None
let prev_outer_size_var = Bonsai.Expert.Var.create None

let component
  ~(lease_pool :
      ( Sd.Hosts.Host.t
        , Sd.Hosts.Current_model.t
        , Sd.Hosts.Host.comparator_witness )
        Lease_pool.t)
  ~(inject : _ Bonsai.t)
  ~(id : _ Bonsai.t)
  ~img
  ~parent_img
  ~is_image_editor
  ~parameters
  ~refine_card
  ~reimagine_card
  ~upscale_card
  ~other_model_card
  ~(resize_card : _ Bonsai.t)
  ~controlnet_fix_card
  ~zoom
  (local_ graph)
  =
  let ( ~get_images
      , ~get_ctrlnet
      , ~override_prompt
      , ~view:img_view
      , ~set_ctrlnet_image
      , ~set_paint_image )
    =
    if is_image_editor
    then
      image_editor_specialization
        ~parent_img
        ~parameters
        ~lease_pool
        ~controlnet_fix_card
        graph
    else (
      let ~pos_prompt, ~neg_prompt =
        prompt_boxes
          ~parameters
          ~container_attrs:[ {%css| opacity: 0.7; |} ]
          ~textarea_attrs:[ Vdom.Attr.disabled ]
          graph
      in
      let get_images =
        let%arr img in
        Effect.return { Images.image = img; mask = None; blur_mask = None }
      in
      let view =
        let%arr img and pos_prompt and neg_prompt in
        let img = Sd.Image.to_vdom img in
        `Image_view (~pos_prompt, ~neg_prompt, ~img)
      in
      ( ~get_images
      , ~get_ctrlnet:(Bonsai.return (Effect.return None))
      , ~override_prompt:(Bonsai.return Fn.id)
      , ~view
      , ~set_ctrlnet_image:None
      , ~set_paint_image:(Bonsai.return None) ))
  in
  let size_state prev_var =
    let size, set_size = Bonsai.state_opt graph in
    let size =
      let%arr size
      and prev_size = Bonsai.Expert.Var.value prev_var in
      Option.first_some size prev_size
    in
    let set_size =
      let%arr set_size in
      fun new_size ->
        Effect.Many
          [ Effect.of_thunk (fun () -> Bonsai.Expert.Var.set prev_var new_size)
          ; set_size new_size
          ]
    in
    size, set_size
  in
  let inner_size, set_inner_size = size_state prev_inner_size_var in
  let outer_size, set_outer_size = size_state prev_outer_size_var in
  let ctrlnet_detect =
    ctrlnet_detect
      ~get_images
      ~parameters
      ~id
      ~inject
      ~lease_pool
      ~controlnet_fix_card
      ~set_ctrlnet_image
      graph
  in
  let generate_button =
    let%arr theme = View.Theme.current graph
    and get_images
    and get_ctrlnet
    and override_prompt
    and id
    and inject
    and dispatcher = Lease_pool.dispatcher lease_pool
    and parameters in
    fun ~button_text ~kind ~modify_parameters ->
      let generate =
        let%bind.Effect { image = img; mask; blur_mask = _ } = get_images in
        let%bind.Effect ctrlnet_image = get_ctrlnet in
        let%bind.Effect parameters =
          modify_parameters ~parameters ~image:img ~ctrlnet_image
        in
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
             ; stage = { desc = kind; state = Enqueued { parameters } }
             ; dispatch
             ; on_complete
             ; parameters
             })
      in
      View.button theme ~on_click:generate button_text, generate
  in
  let%sub upscale_button, upscale =
    let%arr generate_button
    and ~modifier:modify_upscale, ~view:_ = upscale_card in
    generate_button
      ~button_text:"[u]pscale"
      ~kind:(Img2img "upscaled")
      ~modify_parameters:
        (fun
          ~(parameters : Sd_chain.Parameters.t) ~image ~ctrlnet_image ->
        let params : Sd_chain.Parameters.t =
          modify_upscale ~parameters ~image ~ctrlnet_image
        in
        Effect.return
          { params with
            width = Int63.(params.width * of_int 2)
          ; height = Int63.(params.height * of_int 2)
          ; denoise = Int63.of_int 30
          ; steps = Int63.of_int 50
          })
  in
  let%sub refine_button, refine =
    let%arr generate_button
    and ~modifier:modify_refine, ~view:_ = refine_card in
    generate_button
      ~button_text:"ref[y]ne"
      ~kind:(Img2img "refined")
      ~modify_parameters:(fun ~parameters ~image ~ctrlnet_image ->
        Effect.return (modify_refine ~parameters ~image ~ctrlnet_image))
  in
  let%sub reimagine_button, reimagine =
    let%arr generate_button
    and ~modifier:modify_reimagine, ~view:_ = reimagine_card in
    generate_button
      ~button_text:"re[i]magine"
      ~kind:(Img2img "reimagined")
      ~modify_parameters:(fun ~parameters ~image ~ctrlnet_image ->
        Effect.return (modify_reimagine ~parameters ~image ~ctrlnet_image))
  in
  let%sub switch_model_button, switch_model =
    let%arr generate_button
    and ~modifier:modify_other_model, ~view:_ = other_model_card
    and models =
      Lease_pool.all lease_pool
      >>| Map.data
      |> Bonsai.cutoff ~equal:[%equal: Sd.Hosts.Current_model.t list]
    in
    generate_button
      ~button_text:"[o]ther model"
      ~kind:(Img2img "model")
      ~modify_parameters:(fun ~parameters ~image ~ctrlnet_image ->
        let params : Sd_chain.Parameters.t =
          modify_other_model ~parameters ~image ~ctrlnet_image
        in
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
  let%sub edit_button, edit =
    let%arr get_images
    and parameters
    and theme = View.Theme.current graph
    and id
    and inject in
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
           ; stage = { desc = Edit; state = Enqueued { parameters } }
           ; dispatch
           ; on_complete
           ; parameters
           })
    in
    let view = View.button theme "[p]aint" ~on_click:effect in
    view, effect
  in
  let resize_card =
    let%arr parameters and id and inject and resize_card and get_images in
    let set_result ~new_width ~new_height img =
      let parameters = { parameters with width = new_width; height = new_height } in
      let on_complete image =
        Image_tree.Stage.State.Finished { image; parent_image = Some img; parameters }
      in
      inject
        (Image_tree.Action.Add
           { parent_id = id
           ; stage = { desc = Resize; state = Enqueued { parameters } }
           ; dispatch = (fun ~id:_ ~on_started:_ -> Effect.return (Ok img))
           ; on_complete
           ; parameters
           })
    in
    resize_card ~get_images ~set_result
  in
  let settings_modal =
    let content ~close:close_modal graph =
      let%arr ~modifier:_, ~view:reimagine_view = reimagine_card
      and ~modifier:_, ~view:refine_view = refine_card
      and ~modifier:_, ~view:upscale_view = upscale_card
      and ~modifier:_, ~view:other_model_view = other_model_card
      and ~params:_, ~view:controlnet_view = controlnet_fix_card
      and upscale_button
      and resize_card
      and edit_button
      and ctrlnet_detect
      and switch_model_button
      and reimagine_button
      and refine_button
      and theme = View.Theme.current graph
      and close_modal in
      let title =
        [ Vdom.Node.div
            [ Vdom.Node.text "Generation Settings"
            ; Vdom.Node.button
                ~attrs:[ Vdom.Attr.on_click (fun _ -> close_modal) ]
                [ Vdom.Node.text "x" ]
            ]
        ]
      in
      let content =
        View.vbox
          ~gap:(`Em_float 0.5)
          [ View.hbox
              ~gap:(`Em_float 0.5)
              [ upscale_view ~button:upscale_button
              ; refine_view ~button:refine_button
              ; reimagine_view ~button:reimagine_button
              ; other_model_view ~button:switch_model_button
              ]
          ; controlnet_view ~button:ctrlnet_detect
          ; resize_card
          ; edit_button
          ]
      in
      View.card' theme ~title [ content ]
    in
    Toplayer.Modal.create ~overflow_auto_wrapper:(Bonsai.return true) ~content graph
  in
  let%arr img_view
  and inner_size =
    Bonsai.cutoff inner_size ~equal:[%equal: Size_tracker.Dimensions.t option]
  and set_inner_size
  and outer_size =
    Bonsai.cutoff outer_size ~equal:[%equal: Size_tracker.Dimensions.t option]
  and set_outer_size
  and zoom, zoom_slider = zoom
  and refine
  and upscale
  and reimagine
  and switch_model
  and edit
  and set_paint_image
  and open_settings_modal = settings_modal.open_ in
  let view ~state_tree ~host_monitor =
    let container_css =
      {%css|
      width:100%;
      height:100%;

      display: grid; 
      grid-template-columns: 1fr auto auto; 
      grid-template-rows: auto auto 1fr auto; 
      gap: 0px 0px; 

      & > * {
        contain: paint;
      }
    |}
    in
    let header_css =
      {%css|
      grid-area: 1 / 1 / 2 / 4;
      display:flex;
      justify-content: space-between;
      align-items: center;
      background: rgb(0 0 0 / 18%);
      backdrop-filter: blur(10px);
      border-bottom: 1px solid #ffffff73;
    |}
    in
    let sidebar_css =
      {%css|
      grid-area: 2 / 3 / 5 / 4;

      max-height:100%;
      display:flex;
      flex-direction:column;
    |}
    in
    let paint_opts_css =
      {%css| 
      grid-area: 2 / 2 / 3 / 3; 
      padding: 3px;
    |}
    in
    let zoom_css = {%css| grid-area: 4 / 1 / 5 / 2; |} in
    let canvas_css =
      {%css|
      grid-area: 1 / 1 / 5 / 4;
      display:flex; 
      align-items: center ;
      justify-content: center; 
      overflow:clip;
      position:relative;
    |}
    in
    let canvas =
      let css =
        let zoom =
          match inner_size, outer_size with
          | ( Some { border_box = { width = inner_width; height = inner_height }; _ }
            , Some { border_box = { width = outer_width; height = outer_height }; _ } ) ->
            let rat_w = inner_width /. outer_width in
            let rat_h = inner_height /. outer_height in
            let biggest_rat = Float.max rat_w rat_h in
            1.0 /. biggest_rat *. zoom
          | _ -> 1.0
        in
        {%css| transform: scale(%{ Virtual_dom.Dom_float.to_string zoom}); |}
      in
      let contained =
        match img_view with
        | `Editor_view ((~widget, ..), _ctrlnet) -> widget
        | `Image_view (~img, ..) -> img
      in
      Vdom.Node.div
        ~attrs:[ Size_tracker.on_change (fun dims -> set_inner_size (Some dims)) ]
        [ Vdom.Node.div ~attrs:[ css ] [ contained ] ]
    in
    let paint_opts =
      let color_picker =
        match img_view with
        | `Editor_view
            ( ( ~color_picker
              , ~layer_panel
              , ~flip_button
              , ~pen_size_slider
              , ~blur_button
              , ~blur_radius_slider
              , .. )
            , _ctrlnet ) ->
          View.vbox
            [ View.hbox [ layer_panel; color_picker ]
            ; View.hbox [ pen_size_slider; Vdom.Node.text "pen" ]
            ; View.hbox [ blur_radius_slider; blur_button ]
            ; flip_button
            ]
        | `Image_view _ -> Vdom.Node.none
      in
      let fancy_box =
        Shared.Fancy_box.test_box
          ~inner:(~color:"black", ~radius:3, ~border:"1px solid black")
          ~outer:(~color:"white", ~radius:5, ~border:"1px solid rgba(255,255,255, 0.75)")
          ~opacity:0.5
          ~gap:3
          ~blur:10
      in
      {%html| <%{fancy_box}> %{color_picker} </> |}
    in
    let settings_button =
      {%html|
        <div style="margin-right: 1em;" on_click=%{fun _ -> open_settings_modal}> %{Feather_icon.svg ~stroke_width:(`Px 1) ~size:(`Em 2) Settings} </div>
      |}
    in
    let sidebar =
      match img_view with
      | `Editor_view ((~pos_prompt, ~neg_prompt, ..), _)
      | `Image_view (~pos_prompt, ~neg_prompt, ..) ->
        {%html| 
        <> 
          %{Form.view pos_prompt} 
          %{Form.view neg_prompt} 
          %{state_tree} 
        </> 
        |}
    in
    let zoom =
      let size =
        match inner_size with
        | None -> Vdom.Node.none
        | Some { border_box = { width; height }; content_box = _ } ->
          Vdom.Node.textf "%d x %d" (Float.to_int width) (Float.to_int height)
      in
      View.hbox ~gap:(`Em 1) [ zoom_slider; size ]
    in
    let view =
      {%html|
    <div %{container_css}> 
      <div %{canvas_css} %{Size_tracker.on_change (fun dims -> set_outer_size (Some dims))}>%{canvas}</div>
      <div %{header_css}> %{host_monitor} %{settings_button} </div>
      <div %{sidebar_css}> %{sidebar} </div>
      <div %{paint_opts_css}> %{paint_opts} </div>
      <div %{zoom_css}> %{zoom} </div>
    </div>
    |}
    in
    view
  in
  let key_commands =
    let open Vdom_keyboard in
    let module Kh = Keyboard_event_handler in
    Kh.of_command_list_exn
      [ { keys = [ Keystroke.create' KeyY ]
        ; description = "refine"
        ; group = None
        ; handler =
            Kh.Handler.only_handle_if
              Kh.Condition.(not_ has_text_input_target)
              (fun _ -> refine)
        }
      ; { keys = [ Keystroke.create' KeyU ]
        ; description = "upscale"
        ; group = None
        ; handler =
            Kh.Handler.only_handle_if
              Kh.Condition.(not_ has_text_input_target)
              (fun _ -> upscale)
        }
      ; { keys = [ Keystroke.create' KeyI ]
        ; description = "reimagine"
        ; group = None
        ; handler =
            Kh.Handler.only_handle_if
              Kh.Condition.(not_ has_text_input_target)
              (fun _ -> reimagine)
        }
      ; { keys = [ Keystroke.create' KeyO ]
        ; description = "switch model"
        ; group = None
        ; handler =
            Kh.Handler.only_handle_if
              Kh.Condition.(not_ has_text_input_target)
              (fun _ -> switch_model)
        }
      ; { keys = [ Keystroke.create' KeyP ]
        ; description = "edit"
        ; group = None
        ; handler =
            Kh.Handler.only_handle_if
              Kh.Condition.(not_ has_text_input_target)
              (fun _ -> edit)
        }
      ]
  in
  view, key_commands, set_paint_image
;;
