open! Core
open! Bonsai_web.Cont

type t [@@deriving yojson, sexp, equal]

val data_url : t -> string
val of_string : ?width:Int63.t -> ?height:Int63.t -> string -> t
val to_string : t -> string
val to_vdom : ?width:Int63.t -> ?height:Int63.t -> ?drop_size:bool -> t -> Vdom.Node.t
val size : t -> (Int63.t * Int63.t) option
