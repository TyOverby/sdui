open! Core
open Bonsai_web

val parallel_both : 'a Effect.t -> 'b Effect.t -> ('a * 'b) Effect.t
val parallel_all : 'a Effect.t list -> 'a list Effect.t

val parallel_n
  :  update:
       (('a Or_error.t Core.Int.Map.t option -> 'a Or_error.t Core.Int.Map.t)
        -> unit Effect.t)
  -> int
  -> f:(int -> 'a Or_error.t Effect.t)
  -> 'a Or_error.t Int.Map.t Effect.t

val while_running : 'a Effect.t -> do_this:unit Effect.t -> 'a Effect.t
