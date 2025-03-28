open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Feather = Feather_icon

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
    [@@deriving equal, sexp_of]
  end

  module Kind = struct
    type t =
      | Prompt
      | Txt2img
      | Img2img of string
      | Resize
      | Ctrlnet
      | Edit
    [@@deriving equal, sexp_of]

    let to_string = function
      | Prompt -> "prompt"
      | Txt2img -> "txt2img"
      | Edit -> "edit"
      | Resize -> "resize"
      | Ctrlnet -> "control net"
      | Img2img s -> s
    ;;
  end

  type t =
    { desc : Kind.t
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
    ; starred : Unique_id.Set.t
    }

  let first_id = Unique_id.create ()

  let empty =
    { images = Unique_id.Map.singleton first_id { Stage.desc = Prompt; state = Initial }
    ; children = Unique_id.Map.empty
    ; parents = Unique_id.Map.empty
    ; roots = Unique_id.Set.singleton first_id
    ; seen = Unique_id.Set.empty
    ; starred = Unique_id.Set.empty
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
      Map.add_exn t.images ~key:id ~data:{ Stage.desc = Prompt; state = Initial }
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
    | Toggle_starred of Unique_id.t
    | Set_seen of Unique_id.t
end

let state (local_ graph) =
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
      | Toggle_starred id ->
        let starred =
          if Set.mem model.starred id
          then Set.remove model.starred id
          else Set.add model.starred id
        in
        { model with starred }
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

let ul_styles = {%css| list-style-type:none; padding-left:20px; user-select:none; |}
let li_styles = {%css| margin:0; padding:0; |}

let render ~state ~current_id ~inject ~seen ~set_current_id ~override_on_click =
  let%arr state
  and current_id
  and inject
  and seen
  and set_current_id
  and override_on_click in
  let tree_structure = Model.tree_structure state in
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
      if [%equal: Unique_id.t option] (Some id) current_id
      then {%css| border-color: #1ba1f2; |}
      else Vdom.Attr.empty
    in
    let seen_attrs =
      if not (Set.mem seen id)
      then (
        match state with
        | Stage.State.Finished _ | Error _ -> {%css| background: #1bf2372e; |}
        | _ -> Vdom.Attr.empty)
      else Vdom.Attr.empty
    in
    Vdom.Attr.many [ selected_attrs; seen_attrs ]
  in
  let is_starred id = Set.mem state.starred id in
  let star_button_attrs ~child_is_starred ~id =
    [ {%css| 
        cursor:pointer; 

        &:hover {
          opacity: 1;
        }
      |}
    ; (if is_starred id
       then
         {%css|
      fill: gold;
      stroke: gold;

      &:hover {
        opacity: 0.5;
      }
        |}
       else {%css| |})
    ; (if child_is_starred
       then {%css|
        stroke: gold;
       |}
       else Vdom.Attr.empty)
    ; Vdom.Attr.on_click (fun _ ->
        Effect.Many [ inject (Action.Toggle_starred id); Effect.Stop_propagation ])
    ]
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
        Effect.Many
          [ inject (Action.Remove { id; from_kbd = false })
          ; (if [%equal: Unique_id.t option] (Some id) current_id
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
  let rec loop
    ~only_child
    ~deindented
    { Model.Tree_structure.id; stage = { desc; state }; children }
    =
    let icon =
      match state with
      | Initial -> Feather.File_text
      | Enqueued -> Feather.Upload
      | In_progress -> Feather.Loader
      | Finished _ -> Feather.Image
      | Error _ -> Feather.Alert_triangle
    in
    let child_is_starred = List.exists children ~f:(fun { id; _ } -> is_starred id) in
    let children =
      let i_am_selected = [%equal: Unique_id.t option] (Some id) current_id in
      let child_of_mine_is_selected =
        List.exists children ~f:(fun { id; _ } ->
          [%equal: Unique_id.t option] (Some id) current_id)
      in
      let only_have_one_child = List.length children = 1 in
      let children =
        if only_have_one_child || i_am_selected || child_of_mine_is_selected
        then children
        else List.filter children ~f:(fun { id; _ } -> not (is_starred id))
      in
      match children with
      | [] -> Vdom.Node.none
      | [ child ] when only_child -> loop ~only_child:true ~deindented:true child
      | [ _ ] ->
        Vdom.Node.ul
          ~attrs:[ ul_styles ]
          (List.map children ~f:(fun c ->
             Vdom.Node.li [ loop ~only_child:true ~deindented:false c ]))
      | children ->
        Vdom.Node.ul
          ~attrs:[ ul_styles ]
          (List.map children ~f:(fun c ->
             Vdom.Node.li [ loop ~only_child:false ~deindented:false c ]))
    in
    let on_click, highlight =
      match override_on_click, state with
      | Some f, Finished { image; _ } ->
        Vdom.Attr.on_click (fun _ -> f image), {%css| background: yellow; color: black|}
      | _ -> set_on_click id, {%css||}
    in
    Vdom.Node.li
      ~attrs:[ li_styles; Vdom.Attr.id (Unique_id.to_dom_id id) ]
      [ Vdom.Node.span
          ~attrs:[ on_click; maybe_highlight ~state ~id; highlight; label_attr ]
          [ (if deindented
             then
               Feather.svg
                 ~extra_attrs:[ {%css| margin-right: 0.5em; |} ]
                 ~size:(`Em 1)
                 Corner_down_right
             else Vdom.Node.none)
          ; Feather.svg ~extra_attrs:[ {%css| margin-right: 0.5em; |} ] ~size:(`Em 1) icon
          ; Vdom.Node.text (Stage.Kind.to_string desc)
          ; Feather.svg
              ~extra_attrs:
                ({%css| margin-left: 0.5em; |} :: star_button_attrs ~child_is_starred ~id)
              ~size:(`Em 1)
              Star
          ; Feather.svg
              ~extra_attrs:({%css| margin-left: 0.5em; |} :: remove_button_attrs ~id)
              ~size:(`Em 1)
              X
          ]
      ; children
      ]
  in
  Vdom.Node.div
    [ Vdom.Node.ul
        ~attrs:[ ul_styles ]
        (List.map tree_structure ~f:(fun child ->
           Vdom.Node.li [ loop ~only_child:false ~deindented:false child ]))
    ]
;;
