open! Core
open! Bonsai_web.Cont

module Or_stale : sig
  type 'a t = private
    | Fresh of 'a
    | Stale of 'a
    | Not_computed
end

type 'a t = 'a Or_stale.t Bonsai.t

val of_bonsai
  :  equal:('a -> 'a -> bool)
  -> ?time_to_stable:Time_ns.Span.t Bonsai.t
  -> 'a Bonsai.t
  -> Bonsai.graph
  -> 'a t

val map
  :  equal:('a -> 'a -> bool)
  -> 'a t
  -> f:('a -> 'b Effect.t) Bonsai.t
  -> Bonsai.graph
  -> 'b t
