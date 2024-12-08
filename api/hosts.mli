open! Core
open! Bonsai_web

module Host : sig
  type t = private string [@@deriving sexp_of]

  include Comparable.S with type t := t

  val to_string : t -> string
end

module Current_model : sig
  type t = private string [@@deriving sexp_of]

  include Comparable.S with type t := t

  val to_string : t -> string
end

type t =
  { view : Vdom.Node.t
  ; available_hosts : Current_model.t Host.Map.t
  ; set_worker_in_use : Host.t -> bool -> unit Effect.t
  }

val random_healthy_host : t -> Host.t option Effect.t
val component : local_ Bonsai.graph -> t Bonsai.t
