open! Core
open! Bonsai_web.Cont

module Item : sig
  type 'resource t =
    { on_start : unit Effect.t
    ; on_done : unit Effect.t
    ; resource : 'resource
    }
  [@@deriving sexp_of]
end

module Status : sig
  type t =
    | Loitering
    | In_queue of Job_queue.Item_id.t
    | Working
  [@@deriving sexp_of]
end

val component
  :  queue:('resource Item.t, 'spec) Job_queue.t Bonsai.t
  -> item:'resource Bonsai.t
  -> spec:'spec option Bonsai.t
  -> spec_compare:('spec -> 'spec -> int)
  -> Bonsai.graph
  -> Status.t Bonsai.t
