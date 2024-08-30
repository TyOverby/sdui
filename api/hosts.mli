open! Core
open! Bonsai_web.Cont

module Host : sig
  type t = private string [@@deriving sexp_of]

  include Comparable.S with type t := t
end

type t =
  { view : Vdom.Node.t
  ; available_hosts : Host.Set.t
  ; set_worker_in_use : Host.t -> bool -> unit Effect.t
  }

val random_healthy_host : t -> Host.t option Effect.t
val component : Bonsai.graph -> t Bonsai.t
