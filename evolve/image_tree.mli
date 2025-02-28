open! Core
open! Bonsai_web

module Unique_id : sig
  type t [@@deriving sexp, compare]

  include Comparator.S with type t := t
  include Comparable.S with type t := t

  val to_int_exn : t -> int
  val to_dom_id : t -> string
end

module Optional_unique_id : sig
  type t = Unique_id.t option [@@deriving sexp, compare]

  include Comparator.S with type t := t
end

module Stage : sig
  module State : sig
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
    [@@deriving sexp_of, equal]
  end

  module Kind : sig
    type t =
      | Prompt
      | Txt2img
      | Img2img of string
      | Resize
      | Ctrlnet
      | Edit
    [@@deriving sexp_of, equal]
  end

  type t =
    { desc : Kind.t
    ; state : State.t
    }
  [@@deriving equal]
end

module Model : sig
  type t = private
    { images : Stage.t Unique_id.Map.t
    ; children : Unique_id.Set.t Unique_id.Map.t
    ; parents : Unique_id.t Unique_id.Map.t
    ; roots : Unique_id.Set.t
    ; seen : Unique_id.Set.t
    }

  val first_id : Unique_id.t

  module Tree_structure : sig
    type t = private
      { stage : Stage.t
      ; id : Unique_id.t
      ; children : t list
      }

    val preceding
      :  t list
      -> drop_children_of_target:bool
      -> looking_for:Unique_id.t
      -> Unique_id.t option

    val succeeding
      :  t list
      -> drop_children_of_target:bool
      -> looking_for:Unique_id.t
      -> Unique_id.t option
  end

  val tree_structure : t -> Tree_structure.t list
end

module Action : sig
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
            id:Unique_id.t
            -> on_started:unit Effect.t
            -> Sd.Image.t Core.Or_error.t Effect.t
        ; on_complete : Sd.Image.t -> Stage.State.t
        }
    | Set of
        { id : Unique_id.t
        ; stage : Stage.t
        }
    | Set_seen of Unique_id.t
end

val state : local_ Bonsai.graph -> Model.t Bonsai.t * (Action.t -> unit Effect.t) Bonsai.t

val render
  :  state:Model.t Bonsai.t
  -> current_id:Unique_id.t option Bonsai.t
  -> inject:(Action.t -> unit Effect.t) Bonsai.t
  -> seen:(Unique_id.t, 'a) Set.t Bonsai.t
  -> set_current_id:(Unique_id.t -> unit Effect.t) Bonsai.t
  -> override_on_click:(Sd.Image.t -> unit Effect.t) option Bonsai.t
  -> local_ Bonsai.graph
  -> Virtual_dom.Vdom.Node.t Bonsai.t
