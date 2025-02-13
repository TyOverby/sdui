open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool
module P = Sd.Parameters.Individual

module Seen = struct
  type t = Image_tree.Unique_id.Set.t

  let state graph =
    Bonsai.state_machine0
      graph
      ~default_model:Image_tree.Unique_id.Set.empty
      ~apply_action:(fun _ctx -> Set.add)
  ;;
end

let none_screen ~inject graph =
  let%arr theme = View.Theme.current graph
  and inject in
  let view =
    Vdom.Node.div
      [ Vdom.Node.text "none_screen"
      ; View.button theme "new series" ~on_click:(inject Image_tree.Action.Add_root)
      ]
  in
  view, Vdom_keyboard.Keyboard_event_handler.of_command_list_exn [], None
;;

let children_of_current ~current_id ~state ~set_current_id graph =
  let children_state =
    let%arr current_id
    and { Image_tree.Model.children; images; _ } = state in
    match current_id with
    | None -> Image_tree.Unique_id.Map.empty
    | Some current_id ->
      (match Map.find children current_id with
       | None -> Image_tree.Unique_id.Map.empty
       | Some children ->
         children
         |> Set.to_map ~f:(fun id -> Map.find images id)
         |> Map.filter_map ~f:Fn.id)
  in
  let children_state =
    Bonsai.cutoff
      children_state
      ~equal:[%equal: Image_tree.Stage.t Image_tree.Unique_id.Map.t]
  in
  let children_imgs =
    Bonsai.assoc
      (module Image_tree.Unique_id)
      children_state
      graph
      ~f:(fun id stage _graph ->
        let%arr id and set_current_id and stage in
        let image =
          match stage.state with
          | Initial -> None
          | Enqueued -> None
          | In_progress -> None
          | Finished { image; _ } ->
            Some
              (Sd.Image.to_vdom
                 ~drop_size:true
                 ~attrs:[ {%css| max-height:100%; |} ]
                 image)
          | Error e -> Some (Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e))
        in
        match image with
        | None -> None
        | Some image ->
          Some
            (Vdom.Node.div
               ~attrs:[ Vdom.Attr.on_click (fun _ -> set_current_id id) ]
               [ image ]))
  in
  let children_imgs = Bonsai.Map.filter_map children_imgs graph ~f:Fn.id in
  let%arr children_imgs in
  if Map.is_empty children_imgs
  then None
  else
    Some
      (Vdom.Node.Map_children.make
         ~attr:{%css| display: flex; gap:1em; margin: 1em; height: 100%;|}
         ~tag:"div"
         children_imgs)
;;

let controlnet_fix_card ~hosts (local_ graph) =
  let%arr module_form = Sd.Controlnet_modules.form ~hosts graph
  and model_form = Sd.Controlnet_models.form ~hosts graph
  and weight =
    Form.Elements.Range.float
      ~min:(Bonsai.return 0.0)
      ~max:(Bonsai.return 2.0)
      ~default:(Bonsai.return 1.0)
      ~step:(Bonsai.return 0.01)
      ()
      graph
  and start_point =
    Form.Elements.Range.float
      ~min:(Bonsai.return 0.0)
      ~max:(Bonsai.return 1.0)
      ~default:(Bonsai.return 0.0)
      ~step:(Bonsai.return 0.01)
      ()
      graph
  and end_point =
    Form.Elements.Range.float
      ~min:(Bonsai.return 0.0)
      ~default:(Bonsai.return 1.0)
      ~max:(Bonsai.return 0.8)
      ~step:(Bonsai.return 0.01)
      ()
      graph
  and theme = View.Theme.current graph in
  let view ~button =
    View.card'
      theme
      ~title_kind:View.Constants.Card_title_kind.Discreet
      ~title:[ Vdom.Node.Text "Control-Net" ]
      [ View.vbox
          [ module_form.view
          ; model_form.view
          ; Form.view weight
          ; Form.view start_point
          ; Form.view end_point
          ; button
          ]
      ]
  in
  let weight = Form.value_or_default weight ~default:0.0 in
  let start_point = Form.value_or_default start_point ~default:0.0 in
  let end_point = Form.value_or_default end_point ~default:0.0 in
  let params =
    ~module_:module_form.value, ~model:model_form.value, ~weight, ~start_point, ~end_point
  in
  ~params, ~view
;;

let generic_card
  ~default_cfg_enabled
  ~default_denoise_enabled
  ~default_steps_enabled
  ~default_ctrlnet_enabled
  ~default_cfg
  ~default_denoise
  ~default_steps
  ~controlnet_fix_card
  (local_ graph)
  =
  let enabled_checkbox ~default (local_ graph) =
    let%sub { value; view; _ } = Form.Elements.Checkbox.bool ~default () graph in
    let value = value >>| Or_error.ok >>| Option.value ~default in
    let disable_attr =
      match%arr value with
      | true -> Vdom.Attr.empty
      | false -> Vdom.Attr.disabled
    in
    ~view, ~disable_attr, ~value
  in
  let ~view:ctrlnet_enabled_view, ~disable_attr:_, ~value:ctrlnet_enabled =
    enabled_checkbox ~default:default_ctrlnet_enabled graph
  in
  let ~view:cfg_enabled_view, ~disable_attr:cfg_disabled_attr, ~value:cfg_enabled =
    enabled_checkbox ~default:default_cfg_enabled graph
  in
  let ( ~view:denoise_enabled_view
      , ~disable_attr:denoise_disabled_attr
      , ~value:denoise_enabled )
    =
    enabled_checkbox ~default:default_denoise_enabled graph
  in
  let ~view:steps_enabled_view, ~disable_attr:steps_disabled_attr, ~value:steps_enabled =
    enabled_checkbox ~default:default_steps_enabled graph
  in
  let cfg =
    P.min_1_form
      ~input_attrs:(cfg_disabled_attr >>| List.return)
      ~default:default_cfg
      ~max:30
      ~label:"cfg"
      graph
  in
  let denoise =
    P.min_1_form
      ~step:5
      ~input_attrs:(denoise_disabled_attr >>| List.return)
      ~default:default_denoise
      ~max:100
      ~label:"deno"
      graph
  in
  let steps =
    P.min_1_form
      ~step:5
      ~input_attrs:(steps_disabled_attr >>| List.return)
      ~default:default_steps
      ~max:150
      ~label:"steps"
      graph
  in
  let modifier =
    let%arr { value = new_cfg; _ } = cfg
    and { value = new_denoise; _ } = denoise
    and { value = new_steps; _ } = steps
    and cfg_enabled
    and denoise_enabled
    and ctrlnet_enabled
    and steps_enabled
    and ~params:controlnet_params, ~view:_ = controlnet_fix_card in
    let or_default ~default = function
      | Ok v -> v
      | Error _ -> default
    in
    fun ~(parameters : Sd_chain.Parameters.t) ~image ->
      let ctrlnet =
        match controlnet_params with
        | ~module_:(Ok module_), ~model:(Ok model), ~weight, ~start_point, ~end_point
          when ctrlnet_enabled ->
          let module_ = Option.map module_ ~f:Sd.Controlnet_modules.to_string in
          let model = Sd.Controlnet_models.to_string model in
          Some
            { Sd.Alwayson_scripts.Ctrlnet.Query.image
            ; module_
            ; model
            ; weight
            ; guidance_start = start_point
            ; guidance_end = end_point
            }
        | _ -> None
      in
      { parameters with
        cfg =
          (if cfg_enabled
           then new_cfg |> or_default ~default:default_cfg
           else parameters.cfg)
      ; denoise =
          (if denoise_enabled
           then new_denoise |> or_default ~default:default_denoise
           else parameters.denoise)
      ; steps =
          (if steps_enabled
           then new_steps |> or_default ~default:default_steps
           else parameters.steps)
      ; ctrlnet
      }
  in
  let view =
    let%arr { view = cfg_view; _ } = cfg
    and { view = denoise_view; _ } = denoise
    and { view = steps_view; _ } = steps
    and cfg_enabled_view
    and denoise_enabled_view
    and ctrlnet_enabled_view
    and steps_enabled_view in
    [ View.hbox
        ~cross_axis_alignment:Baseline
        ~attrs:[ {%css|user-select:none|} ]
        [ Vdom.Node.label [ ctrlnet_enabled_view; Vdom.Node.text "ctrlnet" ] ]
    ; View.hbox ~cross_axis_alignment:Baseline [ cfg_enabled_view; cfg_view ]
    ; View.hbox ~cross_axis_alignment:Baseline [ denoise_enabled_view; denoise_view ]
    ; View.hbox ~cross_axis_alignment:Baseline [ steps_enabled_view; steps_view ]
    ]
  in
  ~modifier, ~view
;;

let refine_card ~controlnet_fix_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:true
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_ctrlnet_enabled:true
      ~default_cfg:(Int63.of_int 10)
      ~default_denoise:(Int63.of_int 55)
      ~default_steps:(Int63.of_int 25)
      ~controlnet_fix_card
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button ->
      View.card'
        theme
        ~title_kind:View.Constants.Card_title_kind.Discreet
        ~title:[ Vdom.Node.text "Refinement" ]
        (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let reimagine_card ~controlnet_fix_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:true
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_ctrlnet_enabled:false
      ~default_cfg:(Int63.of_int 10)
      ~default_denoise:(Int63.of_int 70)
      ~default_steps:(Int63.of_int 25)
      ~controlnet_fix_card
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button ->
      View.card'
        theme
        ~title_kind:View.Constants.Card_title_kind.Discreet
        ~title:[ Vdom.Node.text "Reimagine" ]
        (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let upscale_card ~controlnet_fix_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:false
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_ctrlnet_enabled:true
      ~default_cfg:(Int63.of_int 7)
      ~default_denoise:(Int63.of_int 30)
      ~default_steps:(Int63.of_int 50)
      ~controlnet_fix_card
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button ->
      View.card'
        theme
        ~title_kind:View.Constants.Card_title_kind.Discreet
        ~title:[ Vdom.Node.text "Upscale" ]
        (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let other_model_card ~controlnet_fix_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:false
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_ctrlnet_enabled:true
      ~default_cfg:(Int63.of_int 7)
      ~default_denoise:(Int63.of_int 40)
      ~default_steps:(Int63.of_int 40)
      ~controlnet_fix_card
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button ->
      View.card'
        theme
        ~title_kind:View.Constants.Card_title_kind.Discreet
        ~title:[ Vdom.Node.text "Change model" ]
        (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let trigger_after_active ~for_duration ~effect graph =
  let module Seqnum = Unique_id.Int63 () in
  let (_ : unit Bonsai.t) =
    Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
      let inputs =
        let%arr for_duration
        and effect
        and sleep = Bonsai.Clock.sleep graph in
        effect, sleep for_duration
      in
      let _state, inject =
        Bonsai.state_machine1
          inputs
          graph
          ~default_model:None
          ~apply_action:(fun ctx input model action ->
            match input, model, action with
            | Inactive, _, _ -> None
            | Active _, None, `Ping _ -> None
            | Active _, Some current_seqnum, `Activate -> Some current_seqnum
            | Active (effect, _), Some current_seqnum, `Ping ping_seqnum ->
              if Seqnum.equal current_seqnum ping_seqnum
              then Bonsai.Apply_action_context.schedule_event ctx effect;
              model
            | Active (_, sleep), None, `Activate ->
              let id = Seqnum.create () in
              Bonsai.Apply_action_context.schedule_event
                ctx
                (let%bind.Effect () = sleep in
                 Bonsai.Apply_action_context.inject ctx (`Ping id));
              Some id)
      in
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr inject in
           inject `Activate)
        ~on_deactivate:reset
        graph;
      Bonsai.return ())
  in
  ()
;;

let add_seen_after_active ~add_seen ~id graph =
  trigger_after_active
    ~for_duration:(Bonsai.return (Time_ns.Span.of_sec 0.5))
    ~effect:
      (let%arr add_seen and id in
       add_seen id)
    graph
;;

let component (local_ graph) =
  let lease_pool, hosts_view, queue_view, _kill_all = Sd_chain.hosts_and_queue graph in
  let state, inject = Image_tree.state graph in
  let seen = state >>| fun { seen; _ } -> seen in
  let current_id, set_current_id =
    Bonsai.state_opt ~default_model:Image_tree.Model.first_id graph
  in
  let add_seen =
    let%arr inject in
    fun id -> inject (Set_seen id)
  in
  let set_current_id =
    let%arr set_current_id
    and _add_seen = add_seen in
    fun id ->
      Effect.Many
        [ set_current_id (Some id)
        ; Effect.of_thunk (fun () ->
            let element =
              Js_of_ocaml.Dom_html.window##.document##querySelector
                (Js_of_ocaml.Js.string ("#" ^ Image_tree.Unique_id.to_dom_id id))
            in
            Js_of_ocaml.Js.Opt.iter element (fun element ->
              (Obj.magic element)##scrollIntoViewIfNeeded))
        ]
  in
  let current_id =
    let%arr current_id
    and { images; _ } = state in
    let%bind.Option current_id in
    if Map.mem images current_id then Some current_id else None
  in
  let current =
    let%arr current_id
    and { images; _ } = state in
    let%bind.Option current_id in
    let%bind.Option { Image_tree.Stage.desc; state } = Map.find images current_id in
    Some (current_id, desc, state)
  in
  let children_of_current = children_of_current in
  let controlnet_fix_card =
    controlnet_fix_card ~hosts:(Lease_pool.all lease_pool) graph
  in
  let refine_card = refine_card ~controlnet_fix_card graph in
  let reimagine_card = reimagine_card ~controlnet_fix_card graph in
  let upscale_card = upscale_card ~controlnet_fix_card graph in
  let other_model_card = other_model_card ~controlnet_fix_card graph in
  let resize_card = Resize_screen.component graph in
  let main_viewport =
    Bonsai.scope_model
      (module Image_tree.Optional_unique_id)
      ~on:current_id
      graph
      ~for_:(fun graph ->
        match%sub current with
        | None -> none_screen ~inject graph
        | Some (id, _desc, Initial) ->
          let%sub view, kbd =
            Txt2img_screen.component
              ~lease_pool
              ~add_seen
              ~id
              ~inject
              ~add_seen_after_active
              graph
          in
          let%arr view and kbd in
          view, kbd, None
        | Some (_, _, Finished { parent_image = None; _ }) ->
          Bonsai.return
            ( Vdom.Node.text "somehow no parent image"
            , Vdom_keyboard.Keyboard_event_handler.of_command_list_exn []
            , None )
        | Some (id, Edit, Finished { image; parent_image = Some parent_image; parameters })
          ->
          Img2img_screen.component
            ~lease_pool
            ~id
            ~inject
            ~img:image
            ~parent_img:parent_image
            ~parameters
            ~add_seen
            ~add_seen_after_active
            ~refine_card
            ~reimagine_card
            ~upscale_card
            ~other_model_card
            ~resize_card
            ~is_image_editor:true
            ~controlnet_fix_card
            graph
        | Some
            (id, _desc, Finished { image; parent_image = Some parent_image; parameters })
          ->
          Img2img_screen.component
            ~lease_pool
            ~id
            ~inject
            ~img:image
            ~parent_img:parent_image
            ~parameters
            ~add_seen
            ~add_seen_after_active
            ~refine_card
            ~reimagine_card
            ~upscale_card
            ~other_model_card
            ~resize_card
            ~is_image_editor:false
            ~controlnet_fix_card
            graph
        | _ ->
          Bonsai.return
            ( Vdom.Node.none
            , Vdom_keyboard.Keyboard_event_handler.of_command_list_exn []
            , None ))
  in
  let state_tree =
    Image_tree.render
      ~state
      ~current_id
      ~inject
      ~seen
      ~set_current_id
      ~override_on_click:(main_viewport >>| Tuple3.get3)
      graph
  in
  let global_keyboard_handlers =
    let%arr current_id and state and set_current_id and inject in
    let up_and_down_handler ~f _ =
      let open Image_tree.Model in
      match current_id with
      | None -> Effect.Ignore
      | Some current_id ->
        (match f (tree_structure state) ~looking_for:current_id with
         | None -> Effect.Ignore
         | Some succ -> set_current_id succ)
    in
    Vdom_keyboard.Keyboard_event_handler.of_command_list_exn
      [ { keys = [ Vdom_keyboard.Keystroke.create' KeyJ ]
        ; description = "down"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (up_and_down_handler
                 ~f:
                   (Image_tree.Model.Tree_structure.succeeding
                      ~drop_children_of_target:false))
        }
      ; { keys = [ Vdom_keyboard.Keystroke.create' KeyK ]
        ; description = "up"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (up_and_down_handler
                 ~f:
                   (Image_tree.Model.Tree_structure.preceding
                      ~drop_children_of_target:false))
        }
      ; { keys = [ Vdom_keyboard.Keystroke.create' KeyL ]
        ; description = "delete"
        ; group = None
        ; handler =
            Vdom_keyboard.Keyboard_event_handler.Handler.only_handle_if
              Vdom_keyboard.Keyboard_event_handler.Condition.(not_ has_text_input_target)
              (fun _ ->
                match current_id with
                | None -> Effect.Ignore
                | Some current_id ->
                  Effect.Many
                    [ (match
                         ( Image_tree.Model.Tree_structure.succeeding
                             (Image_tree.Model.tree_structure state)
                             ~drop_children_of_target:true
                             ~looking_for:current_id
                         , Image_tree.Model.Tree_structure.preceding
                             (Image_tree.Model.tree_structure state)
                             ~drop_children_of_target:true
                             ~looking_for:current_id )
                       with
                       | None, None -> Effect.Ignore
                       | Some succ, _ -> set_current_id succ
                       | None, Some pred -> set_current_id pred)
                    ; inject
                        (Image_tree.Action.Remove { id = current_id; from_kbd = true })
                    ])
        }
      ]
  in
  let%arr state_tree
  and main_viewport, subview_handlers, _set_paint_image = main_viewport
  and hosts_view
  and queue_view
  and global_keyboard_handlers
  and children_of_current =
    children_of_current ~current_id ~state ~set_current_id graph
  in
  let open Snips.Infix in
  let handler =
    Virtual_dom.Vdom.Attr.Global_listeners.keydown ~phase:Bubbling ~f:(fun event ->
      Vdom_keyboard.Keyboard_event_handler.handle_or_ignore_event
        (Vdom_keyboard.Keyboard_event_handler.merge
           ~on_dup:`Override_with_right
           global_keyboard_handlers
           subview_handlers)
        event)
  in
  Snips.right ~scroll:(Snips.Scroll_config.only_on_primary ~gutter:`Stable ()) state_tree
  |+| Snips.top (View.hbox [ hosts_view; queue_view ])
  |+| Option.value_map
        children_of_current
        ~f:(Snips.bottom ~attr:{%css| max-height: 30vh; |})
        ~default:(Snips.bottom Vdom.Node.none)
  |+| Snips.body ~attr:handler main_viewport
  |> Snips.render
;;
