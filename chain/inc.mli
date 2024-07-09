open! Core
open! Bonsai_web.Cont

module Or_error_or_stale : sig
  type 'a t =
    | Fresh of 'a
    | Stale of 'a
    | Error of Error.t
    | Not_computed
end

type 'a t = 'a Or_error_or_stale.t Bonsai.t

val of_bonsai
  :  equal:('a -> 'a -> bool)
  -> ?time_to_stable:Time_ns.Span.t Bonsai.t
  -> 'a Bonsai.t
  -> Bonsai.graph
  -> 'a t

val of_or_error_bonsai
  :  equal:('a -> 'a -> bool)
  -> ?time_to_stable:Time_ns.Span.t Bonsai.t
  -> 'a Or_error.t Bonsai.t
  -> Bonsai.graph
  -> 'a t

val map
  :  equal:('a -> 'a -> bool)
  -> 'a t
  -> f:('a -> 'b Effect.t) Bonsai.t
  -> Bonsai.graph
  -> 'b t

val map2
  :  equal_a:('a -> 'a -> bool)
  -> equal_b:('b -> 'b -> bool)
  -> 'a t
  -> 'b t
  -> f:('a -> 'b -> 'c Effect.t) Bonsai.t
  -> Bonsai.graph
  -> 'c t

val map_pure : 'a t -> f:('a -> 'b) -> 'b t
val collapse_error : 'a Or_error.t t -> 'a t
