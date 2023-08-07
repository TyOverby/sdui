open! Core
open! Bonsai_web

type t [@@deriving yojson, sexp]

val of_string : ?width:Int63.t -> ?height:Int63.t -> string -> t
val to_string : t -> string
val to_vdom : ?width:Int63.t -> ?height:Int63.t -> ?drop_size:bool -> t -> Vdom.Node.t
val size : t -> (Int63.t * Int63.t) option
