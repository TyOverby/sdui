open! Core
open! Bonsai_web

type t = string [@@deriving sexp]

let of_string = Fn.id
let to_string = Fn.id

let to_vdom ?width ?height t =
  let width =
    match width with
    | None -> Vdom.Attr.empty
    | Some width -> Vdom.Attr.create "width" (Int.to_string width)
  in
  let height =
    match height with
    | None -> Vdom.Attr.empty
    | Some height -> Vdom.Attr.create "height" (Int.to_string height)
  in
  Vdom.Node.img
    ~attrs:[ Vdom.Attr.src (sprintf "data:image/png;base64, %s" t); width; height ]
    ()
;;

let t_of_yojson = function
  | `String s -> s
  | other -> raise_s [%message "unknown base64 image json" (other : Yojson_safe.t)]
;;

let yojson_of_t t = `String t
