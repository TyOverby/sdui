open! Core
open! Bonsai_web.Cont

(* A lease-pool is a data structure for lending out values of type ['a] to
   a user of the pool for the duration of an effect.

   Values that can be lent out are provided as an input to the component in a
   set, and the pool just makes sure that no two ['a] are being lent out at
   the same time.

   - When a user attempts to perform an effect which requires an ['a], but
     there are no values in the pool at all (busy or available), the callback
     is invoked with [None],
   - if there are available values that aren't being lent out then the
     callback is immediately invoked with [Some a].
   - If all values are currently being lent out, then the effect is enqueued,
     and will be invoked as soon as a currently-lended ['a] becomes available. *)
type ('key, 'data, 'cmp) t

val create
  :  ('key, 'cmp) Comparator.Module.t
  -> ?data_equal:('data -> 'data -> bool)
  -> ('key, 'data, 'cmp) Map.t Bonsai.t
  -> Bonsai.graph
  -> ('key, 'data, 'cmp) t

val dispatcher
  :  ('key, 'data, _) t
  -> (?info:Sexp.t
      -> ?pred:('key -> 'data -> bool)
      -> (('key * 'data) Or_error.t -> 'result Effect.t)
      -> 'result Effect.t)
       Bonsai.t

val available : ('k, _, 'cmp) t -> ('k, 'cmp) Set.t Bonsai.t
val leased_out : ('k, _, 'cmp) t -> ('k, 'cmp) Set.t Bonsai.t
val debug : _ t -> Sexp.t Bonsai.t
val clear_all : _ t -> unit Effect.t Bonsai.t

val advise
  :  ('key, 'data, 'cmp) t
  -> on_take:('key -> 'data -> unit Effect.t) Bonsai.t
  -> on_return:('key -> unit Effect.t) Bonsai.t
  -> ('key, 'data, 'cmp) t
