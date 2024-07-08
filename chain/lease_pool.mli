open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

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
type ('key, 'data) t

val create
  :  ('key, 'cmp) Comparator.Module.t
  -> ?data_equal:('data -> 'data -> bool)
  -> ('key, 'data, 'cmp) Map.t Bonsai.t
  -> Bonsai.graph
  -> ('key, 'data) t

val dispatcher
  :  ('key, 'data) t
  -> (?info:Sexp.t
      -> ?pred:('key -> 'data -> bool)
      -> (('key * 'data) option -> 'result Effect.t)
      -> 'result Effect.t)
       Bonsai.t

val debug : ('key, 'data) t -> Sexp.t Bonsai.t
