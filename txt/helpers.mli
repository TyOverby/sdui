open! Core
open! Bonsai_web

val with_yoink : ('a Effect.t Value.t -> 'a Computation.t) -> 'a Computation.t

module Path : sig
  include Comparable.S_plain

  val sexp_of_t : t -> Sexp.t
end

val assoc_into_path_map
  :  ('k, 'cmp) Bonsai.comparator
  -> ('k, 'v, 'cmp) Map.t Value.t
  -> f:
       (key:'k Value.t
        -> my_path:Path.t Effect.t Value.t
        -> data:'v Value.t
        -> 'r Computation.t)
  -> (('k, Path.t * 'r, 'cmp) Map.t * 'r Path.Map.t) Computation.t

val multi_merge
  :  ('k, 'cmp) Bonsai.comparator
  -> ('k, 'v, 'cmp) Base.Map.t Value.t
  -> f:
       (key:'k Value.t
        -> my_path:Path.t Ui_effect.t Value.t
        -> data:'v Value.t
        -> ('a * 'a Path.Map.t) Computation.t)
  -> (('k, 'a, 'cmp) Map.t * 'a Path.Map.t) Computation.t

type 'a t =
  { view : Vdom.Node.t
  ; value : 'a
  }

val tree
  :  (get_all:('k, 'a, 'cmp) Map.t Effect.t Value.t
      -> ('r * ('k, 'a t, 'cmp) Map.t) Computation.t)
  -> 'r Computation.t

val alist_group : equal:('k -> 'k -> bool) -> ('k * 'v) list -> ('k * 'v list) list
