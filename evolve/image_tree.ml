open! Core
open! Bonsai_web

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
      | Edit
    [@@deriving equal, sexp_of]

    let to_string = function
      | Prompt -> "prompt"
      | Txt2img -> "txt2img"
      | Edit -> "edit"
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
    }

  let first_id = Unique_id.create ()

  let empty =
    { images = Unique_id.Map.singleton first_id { Stage.desc = Prompt; state = Initial }
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
