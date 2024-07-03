open! Core
open! Bonsai_web.Cont

module Job : sig
  type ('resource, 'spec) t [@@deriving sexp_of]

  val create
    :  ?sexp_of_arg:('arg -> Sexp.t)
    -> dispatch:('spec -> 'resource -> 'arg -> 'result Ui_effect.t)
    -> queue:(('resource, 'spec) t, 'spec) Job_queue.t
    -> 'spec
    -> 'arg
    -> 'result Ui_effect.t
end

module Status : sig
  type t [@@deriving sexp_of]

  val loitering_or_inactive : t
end

val component
  :  queue:(('resource, 'spec) Job.t, 'spec) Job_queue.t Bonsai.t
  -> resource:'resource Bonsai.t
  -> spec:'spec Bonsai.t
  -> spec_compare:('spec -> 'spec -> int)
  -> Bonsai.graph
  -> Status.t Bonsai.t
