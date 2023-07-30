open! Core
open! Bonsai_web

type t [@@deriving yojson, sexp]

val of_string : string -> t
val to_string : t -> string
val to_vdom : ?width:int -> ?height:int -> t -> Vdom.Node.t
