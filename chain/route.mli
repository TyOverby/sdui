open! Core
open! Bonsai_web.Cont

type ('k, 'v) t =
  | Empty
  | Branch of
      { key : 'k
      ; data : 'v
      ; children : ('k, 'v) t list
      }
[@@deriving sexp_of]

val empty : _ t
val branch : key:'k -> data:'v -> children:('k, 'v) t list -> ('k, 'v) t
val render : ('k, 'v) t -> f:('k -> 'v -> Vdom.Node.t) -> Vdom.Node.t
