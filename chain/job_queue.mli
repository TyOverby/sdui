open! Core
open! Bonsai_web.Cont

module Item_id : sig
  type t [@@deriving sexp_of]
end

type ('a, 'spec) t

val push_back : ('a, 'spec) t -> 'spec -> 'a -> Item_id.t Ui_effect.t
val pop_front : ('a, 'spec) t -> 'spec -> 'a Ui_effect.t
val remove : ('a, 'spec) t -> Item_id.t -> unit Ui_effect.t
val debug : ('a, 'spec) t -> Vdom.Node.t

val pipe
  :  ?sexp_of_a:('a -> Sexp.t)
  -> ?sexp_of_spec:('spec -> Sexp.t)
  -> compare:('spec -> 'spec -> int)
  -> Bonsai.graph
  -> ('a, 'spec) t Bonsai.t
