open! Core
open! Bonsai_web.Cont

module Host : sig
  type t = private string [@@deriving sexp_of]

  include Comparable.S with type t := t
end

module Work : sig
  type t =
    { host : Host.t
    ; f : 'a. (Host.t -> 'a Or_error.t Effect.t) -> 'a Or_error.t Effect.t
    }
  [@@deriving sexp_of]
end

type request_host = Work.t Effect.t

type t =
  { view : Vdom.Node.t
  ; request : request_host
  ; available_hosts : Host.Set.t
  }

val component : Bonsai.graph -> t Bonsai.t
