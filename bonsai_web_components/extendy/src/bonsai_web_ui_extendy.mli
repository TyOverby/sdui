open! Core
open Bonsai_web

module Id : sig
  type t [@@deriving equal, sexp]

  include Comparable.S with type t := t
end

type 'a t =
  { contents : 'a Id.Map.t
  ; append : unit Effect.t
  ; set_length : int -> unit Effect.t
  ; remove : Id.t -> unit Effect.t
  }

(** Given a computation, builds a new computation that can hold
    a dynamic number of the wrapped computation. *)
val component : (Bonsai.graph -> 'a Bonsai.t) -> Bonsai.graph -> 'a t Bonsai.t

(** Like [component], but with the power to extend the result of the
    input component with an event that removes itself. *)
val component'
  :  (Bonsai.graph -> 'a Bonsai.t)
  -> wrap_remove:('a -> unit Effect.t -> 'b)
  -> Bonsai.graph
  -> 'b t Bonsai.t
