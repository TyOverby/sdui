open! Core
open! Bonsai_web.Cont

module Or_error_or_stale : sig
  type 'a t =
    | Fresh of 'a
    | Stale of 'a
    | Error of Error.t
    | Not_computed

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val all : 'a t list -> 'a list t
  val unzip : ('a * 'b) t -> 'a t * 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t
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
  -> f:(update:(('b option -> 'b) -> unit Effect.t) -> 'a -> 'b Effect.t) Bonsai.t
  -> Bonsai.graph
  -> 'b t

val map2
  :  equal_a:('a -> 'a -> bool)
  -> equal_b:('b -> 'b -> bool)
  -> 'a t
  -> 'b t
  -> f:(update:(('c option -> 'c) -> unit Effect.t) -> 'a -> 'b -> 'c Effect.t) Bonsai.t
  -> Bonsai.graph
  -> 'c t

val map3
  :  equal_a:('a -> 'a -> bool)
  -> equal_b:('b -> 'b -> bool)
  -> equal_c:('c -> 'c -> bool)
  -> 'a t
  -> 'b t
  -> 'c t
  -> f:
       (update:(('d option -> 'd) -> unit Effect.t) -> 'a -> 'b -> 'c -> 'd Effect.t)
         Bonsai.t
  -> Bonsai.graph
  -> 'd t

val optional : 'a Or_error_or_stale.t option Bonsai.t -> 'a option t
val map_pure : 'a t -> f:('a -> 'b) -> 'b t
val map2_pure : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val collapse_error : 'a Or_error.t t -> 'a t
