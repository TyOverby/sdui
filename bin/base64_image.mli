open! Core
open! Bonsai_web

type t [@@deriving yojson, sexp]

val of_string : ?width:Int63.t -> ?height:Int63.t -> string -> t
val to_string : t -> string
val to_vdom : ?width:Int63.t -> ?height:Int63.t -> t -> Vdom.Node.t
