open! Core
open! Bonsai_web

type kind =
  | Base64
  | Url
[@@deriving sexp, equal]

type t [@@deriving yojson, sexp, equal]

val empty : t
val is_empty : t -> bool
val of_string : ?width:Int63.t -> ?height:Int63.t -> kind:kind -> string -> t
val to_string : t -> string
val with_size : t -> width:Int63.t -> height:Int63.t -> t

val to_vdom
  :  ?attrs:Vdom.Attr.t list
  -> ?width:Int63.t
  -> ?height:Int63.t
  -> ?drop_size:bool
  -> t
  -> Vdom.Node.t

val size : t -> (Int63.t * Int63.t) option
