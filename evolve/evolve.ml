open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool

module Image_tree = struct
  module Unique_id = struct
    include Core.Unique_id.Int ()

    let to_dom_id id = sprintf "tree_node_%s" (to_string id)
  end

  module Optional_unique_id = struct
    module T = struct
      type t = Unique_id.t option [@@deriving sexp, compare]
    end

    include T
    include Comparator.Make (T)
  end

  module Stage = struct
    module State = struct
      type t =
        | Initial
        | Enqueued
        | In_progress
        | Finished of
            { parent_image : Sd.Image.t option
            ; image : Sd.Image.t
            ; parameters : Sd_chain.Parameters.t
            }
        | Error of Error.t
      [@@deriving equal]
    end

    module Kind = struct
      type t =
        | Txt2Img
        | Txt2Img_result
        | Upscale
    end

    type t =
      { desc : string
      ; state : State.t
      }
    [@@deriving equal]
  end

  module Model = struct
    type t =
      { images : Stage.t Unique_id.Map.t
      ; children : Unique_id.Set.t Unique_id.Map.t
      ; parents : Unique_id.t Unique_id.Map.t
      ; roots : Unique_id.Set.t
      ; seen : Unique_id.Set.t
      }

    let first_id = Unique_id.create ()

    let empty =
      { images =
          Unique_id.Map.singleton first_id { Stage.desc = "Prompt"; state = Initial }
      ; children = Unique_id.Map.empty
      ; parents = Unique_id.Map.empty
      ; roots = Unique_id.Set.singleton first_id
      ; seen = Unique_id.Set.empty
      }
    ;;

    module Tree_structure = struct
      type t =
        { stage : Stage.t
        ; id : Unique_id.t
        ; children : t list
        }

      let rec linearize ~drop_children_of t_list =
        let loop t =
          if drop_children_of t.id
          then [ t.id ]
          else t.id :: linearize ~drop_children_of t.children
        in
        List.concat_map t_list ~f:loop
      ;;

      let preceding t_list ~drop_children_of_target ~looking_for =
        let rec loop = function
          | [] -> None
          | x :: found :: _ when Unique_id.equal found looking_for -> Some x
          | _ :: rest -> loop rest
        in
        let drop_children_of =
          if drop_children_of_target
          then fun x -> Unique_id.equal x looking_for
          else Fn.const false
        in
        loop (linearize ~drop_children_of t_list)
      ;;

      let succeeding t_list ~drop_children_of_target ~looking_for =
        let rec loop = function
          | [] -> None
          | found :: x :: _ when Unique_id.equal found looking_for -> Some x
          | _ :: rest -> loop rest
        in
        let drop_children_of =
          if drop_children_of_target
          then fun x -> Unique_id.equal x looking_for
          else Fn.const false
        in
        loop (linearize ~drop_children_of t_list)
      ;;
    end

    let tree_structure { images; children; roots; _ } =
      let rec loop id =
        match Map.find images id, Map.find children id with
        | None, _ -> None
        | Some stage, None -> Some { Tree_structure.id; stage; children = [] }
        | Some stage, Some children ->
          let children = List.filter_map (Set.to_list children) ~f:loop in
          Some { Tree_structure.id; stage; children }
      in
      List.filter_map (Set.to_list roots) ~f:loop
    ;;

    let add_root t =
      let id = Unique_id.create () in
      let images =
        Map.add_exn t.images ~key:id ~data:{ Stage.desc = "Prompt"; state = Initial }
      in
      { t with roots = Set.add t.roots id; images }
    ;;

    let add ~parent_id ~desc ~state t =
      let id = Unique_id.create () in
      let children =
        Map.update t.children parent_id ~f:(function
          | None -> Unique_id.Set.singleton id
          | Some set -> Set.add set id)
      in
      let parents = Map.set t.parents ~key:id ~data:parent_id in
      let images = Map.set t.images ~key:id ~data:{ Stage.desc; state } in
      let new_state = { t with images; children; parents } in
      new_state, id
    ;;

    let set ~id ~desc ~state t =
      { t with images = Map.set t.images ~key:id ~data:{ Stage.desc; state } }
    ;;

    let remove ~id t =
      let children =
        match Map.find t.parents id with
        | None -> t.children
        | Some parent_id ->
          Map.change t.children parent_id ~f:(function
            | None -> None
            | Some children ->
              let c_set = Set.remove children id in
              if Set.is_empty c_set then None else Some c_set)
      in
      let roots = Set.remove t.roots id in
      let parents = Map.remove t.parents id in
      let images = Map.remove t.images id in
      { t with children; roots; parents; images }
    ;;
  end

  module Action = struct
    type t =
      | Add_root
      | Remove of
          { id : Unique_id.t
          ; from_kbd : bool
          }
      | Add of
          { parent_id : Unique_id.t
          ; stage : Stage.t
          ; dispatch :
              id:Unique_id.t -> on_started:unit Effect.t -> Sd.Image.t Or_error.t Effect.t
          ; on_complete : Sd.Image.t -> Stage.State.t
          }
      | Set of
          { id : Unique_id.t
          ; stage : Stage.t
          }
      | Set_seen of Unique_id.t
  end

  let state graph =
    Bonsai.state_machine0
      graph
      ~default_model:Model.empty
      ~apply_action:(fun ctx model action ->
        match action with
        | Action.Add_root -> Model.add_root model
        | Remove { id; from_kbd } ->
          if from_kbd && Set.mem model.roots id then model else Model.remove ~id model
        | Set { id; stage = { desc; state } } -> Model.set model ~id ~desc ~state
        | Set_seen id -> { model with seen = Set.add model.seen id }
        | Add { parent_id; stage = { desc; state }; dispatch; on_complete } ->
          let state, id = Model.add model ~parent_id ~desc ~state in
          Bonsai.Apply_action_context.schedule_event
            ctx
            (match%bind.Effect
               dispatch
                 ~id
                 ~on_started:
                   (Bonsai.Apply_action_context.inject
                      ctx
                      (Set { id; stage = { desc; state = In_progress } }))
             with
             | Ok image ->
               Bonsai.Apply_action_context.inject
                 ctx
                 (Set { id; stage = { desc; state = on_complete image } })
             | Error e ->
               Bonsai.Apply_action_context.inject
                 ctx
                 (Set { id; stage = { desc; state = Error e } }));
          state)
  ;;
end

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

let txt2img_screen
  ~(lease_pool :
      ( Sd.Hosts.Host.t
        , Sd.Hosts.Current_model.t
        , Sd.Hosts.Host.comparator_witness )
        Lease_pool.t)
  ~inject
  ~id
  ~add_seen
  graph
  =
  add_seen_after_active ~add_seen ~id graph;
  let models =
    let%arr hosts = Lease_pool.all lease_pool in
    hosts |> Map.data |> Sd.Hosts.Current_model.Set.of_list
  in
  let parameters = Sd_chain.Parameters.component ~models graph in
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
               | Ok ((img, _) :: _) -> Ok img
               | Ok [] -> Error (Error.of_string "no images")
               | Error e -> Error e))
        in
        let on_complete image =
          Image_tree.Stage.State.Finished { image; parent_image = None; parameters }
        in
        inject
          (Image_tree.Action.Add
             { parent_id = id
             ; stage = { desc = "txt2img"; state = Enqueued }
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
  let view =
    View.vbox
      [ Vdom.Node.div
          [ Form.view parameters ~direction:`Horizontal ~theme ~reset:Effect.Ignore ]
      ; generate_button
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

let img2img_screen
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
  graph
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
          ; steps = Int63.of_int 30
          ; cfg = Int63.of_int 7
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
        [ set_current_id (Some id) (* ; add_seen id *)
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
  let main_viewport =
    Bonsai.scope_model
      (module Image_tree.Optional_unique_id)
      ~on:current_id
      graph
      ~for_:(fun graph ->
        match%sub current with
        | None -> none_screen ~inject graph
        | Some (id, _desc, Initial) ->
          txt2img_screen ~lease_pool ~add_seen ~id ~inject graph
        | Some (id, _desc, Finished { image; parent_image = _; parameters }) ->
          img2img_screen ~lease_pool ~id ~inject ~img:image ~parameters ~add_seen graph
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
        ~default:Snips.none
  |+| Snips.body ~attr:handler main_viewport
  |> Snips.render
;;
