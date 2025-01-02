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
  view, Vdom_keyboard.Keyboard_event_handler.of_command_list_exn []
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

let state_tree =
  let ul_styles =
    {%css| margin-right: 1em; list-style-type:none; padding-left:20px; user-select:none; |}
  and li_styles = {%css| margin:0; padding:0; |} in
  fun ~state ~current_id ~inject ~seen ~set_current_id graph ->
    let%arr state
    and current_id
    and inject
    and theme = View.Theme.current graph
    and seen
    and set_current_id in
    let tree_structure = Image_tree.Model.tree_structure state in
    let set_on_click id = Vdom.Attr.on_click (fun _ -> set_current_id id) in
    let label_attr =
      {%css| 
        display: inline-flex; 
        align-items: flex-end; 
        cursor: pointer; 
        line-height: 1em; 
        padding: 3px 5px; 
        border:2px solid transparent;
        border-radius: 5px;
        |}
    in
    let maybe_highlight ~state ~id =
      let selected_attrs =
        if [%equal: Image_tree.Unique_id.t option] (Some id) current_id
        then {%css| border-color: #1ba1f2; |}
        else Vdom.Attr.empty
      in
      let seen_attrs =
        if not (Set.mem seen id)
        then (
          match state with
          | Image_tree.Stage.State.Finished _ | Error _ ->
            {%css| background: #1bf2372e; |}
          | _ -> Vdom.Attr.empty)
        else Vdom.Attr.empty
      in
      Vdom.Attr.many [ selected_attrs; seen_attrs ]
    in
    let remove_button_attrs ~id =
      [ {%css| 
        cursor:pointer; 
        opacity: 0.6;

        &:hover {
          opacity: 1;
          border: 1px solid currentColor;
          border-radius: 3px;
        }
      |}
      ; Vdom.Attr.on_click (fun _ ->
          print_s
            [%message (state.parents : Image_tree.Unique_id.t Image_tree.Unique_id.Map.t)];
          Effect.Many
            [ inject (Image_tree.Action.Remove { id; from_kbd = false })
            ; (if [%equal: Image_tree.Unique_id.t option] (Some id) current_id
               then
                 Option.value_map
                   (Map.find state.parents id)
                   ~f:set_current_id
                   ~default:Effect.Ignore
               else Effect.Ignore)
            ; Effect.Stop_propagation
            ])
      ]
    in
    let rec loop { Image_tree.Model.Tree_structure.id; stage = { desc; state }; children }
      =
      let icon =
        match state with
        | Initial -> Feather.File_text
        | Enqueued -> Feather.Upload
        | In_progress -> Feather.Loader
        | Finished _ -> Feather.Image
        | Error _ -> Feather.Alert_triangle
      in
      Vdom.Node.li
        ~attrs:[ li_styles; Vdom.Attr.id (Image_tree.Unique_id.to_dom_id id) ]
        [ Vdom.Node.span
            ~attrs:[ set_on_click id; maybe_highlight ~state ~id; label_attr ]
            [ Feather.svg
                ~extra_attrs:[ {%css| margin-right: 0.5em; |} ]
                ~size:(`Em 1)
                icon
            ; Vdom.Node.text desc
            ; Feather.svg
                ~extra_attrs:({%css| margin-left: 0.5em; |} :: remove_button_attrs ~id)
                ~size:(`Em 1)
                X
            ]
        ; Vdom.Node.ul ~attrs:[ ul_styles ] (List.map children ~f:loop)
        ]
    in
    Vdom.Node.div
      [ Vdom.Node.ul ~attrs:[ ul_styles ] (List.map tree_structure ~f:loop)
      ; View.button
          theme
          "New Prompt"
          ~attrs:[ {%css| width: 100%; text-align: center; |} ]
          ~on_click:(inject Image_tree.Action.Add_root)
      ]
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

let generic_card
  ~default_cfg_enabled
  ~default_denoise_enabled
  ~default_steps_enabled
  ~default_cfg
  ~default_denoise
  ~default_steps
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
      ~input_attrs:(denoise_disabled_attr >>| List.return)
      ~default:default_denoise
      ~max:100
      ~label:"deno"
      graph
  in
  let steps =
    P.min_1_form
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
    and steps_enabled in
    let or_default ~default = function
      | Ok v -> v
      | Error _ -> default
    in
    fun (params : Sd_chain.Parameters.t) ->
      { params with
        cfg =
          (if cfg_enabled then new_cfg |> or_default ~default:default_cfg else params.cfg)
      ; denoise =
          (if denoise_enabled
           then new_denoise |> or_default ~default:default_denoise
           else params.denoise)
      ; steps =
          (if steps_enabled
           then new_steps |> or_default ~default:default_steps
           else params.steps)
      }
  in
  let view =
    let%arr { view = cfg_view; _ } = cfg
    and { view = denoise_view; _ } = denoise
    and { view = steps_view; _ } = steps
    and cfg_enabled_view
    and denoise_enabled_view
    and steps_enabled_view in
    [ View.hbox ~cross_axis_alignment:Baseline [ cfg_enabled_view; cfg_view ]
    ; View.hbox ~cross_axis_alignment:Baseline [ denoise_enabled_view; denoise_view ]
    ; View.hbox ~cross_axis_alignment:Baseline [ steps_enabled_view; steps_view ]
    ]
  in
  ~modifier, ~view
;;

let refine_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:true
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_cfg:(Int63.of_int 5)
      ~default_denoise:(Int63.of_int 50)
      ~default_steps:(Int63.of_int 30)
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button -> View.card' theme (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let reimagine_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:true
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_cfg:(Int63.of_int 10)
      ~default_denoise:(Int63.of_int 70)
      ~default_steps:(Int63.of_int 25)
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button -> View.card' theme (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let upscale_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:false
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_cfg:(Int63.of_int 7)
      ~default_denoise:(Int63.of_int 30)
      ~default_steps:(Int63.of_int 50)
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button -> View.card' theme (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
;;

let other_model_card (local_ graph) =
  let ~modifier, ~view =
    generic_card
      ~default_cfg_enabled:false
      ~default_denoise_enabled:true
      ~default_steps_enabled:true
      ~default_cfg:(Int63.of_int 7)
      ~default_denoise:(Int63.of_int 40)
      ~default_steps:(Int63.of_int 40)
      graph
  in
  let view =
    let%arr view
    and theme = View.Theme.current graph in
    fun ~button -> View.card' theme (view @ [ button ])
  in
  let%arr modifier and view in
  ~modifier, ~view
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
  let state_tree = state_tree ~state ~current_id ~inject ~seen ~set_current_id graph in
  let refine_card = refine_card graph in
  let reimagine_card = reimagine_card graph in
  let upscale_card = upscale_card graph in
  let other_model_card = other_model_card graph in
  let main_viewport =
    Bonsai.scope_model
      (module Image_tree.Optional_unique_id)
      ~on:current_id
      graph
      ~for_:(fun graph ->
        match%sub current with
        | None -> none_screen ~inject graph
        | Some (id, _desc, Initial) ->
          Txt2img_screen.component
            ~lease_pool
            ~add_seen
            ~id
            ~inject
            ~add_seen_after_active
            graph
        | Some (_, _, Finished { parent_image = None; _ }) ->
          Bonsai.return
            ( Vdom.Node.text "somehow no parent image"
            , Vdom_keyboard.Keyboard_event_handler.of_command_list_exn [] )
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
            ~is_image_editor:true
            graph
        | _ ->
          Bonsai.return
            (Vdom.Node.none, Vdom_keyboard.Keyboard_event_handler.of_command_list_exn []))
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
  and main_viewport, subview_handlers = main_viewport
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
