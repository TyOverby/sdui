open! Core
open! Bonsai_web
open Shared

type t =
  { width : Int63.t option
  ; height : Int63.t option
  ; content : string
  }
[@@deriving sexp]

let of_string ?width ?height content = { width; height; content }
let to_string t = t.content

let to_vdom ?width ?height ?(drop_size = false) t =
  let width =
    match drop_size, Option.first_some t.width width with
    | true, _ | _, None -> Vdom.Attr.empty
    | false, Some width -> Vdom.Attr.create "width" (Int63.to_string width)
  in
  let height =
    match drop_size, Option.first_some t.height height with
    | true, _ | _, None -> Vdom.Attr.empty
    | _, Some height -> Vdom.Attr.create "height" (Int63.to_string height)
  in
  Vdom.Node.img
    ~attrs:
      [ Vdom.Attr.src (sprintf "data:image/png;base64, %s" t.content); width; height ]
    ()
;;

let t_of_yojson = function
  | `String content -> { content; width = None; height = None }
  | other -> raise_s [%message "unknown base64 image json" (other : Yojson_safe.t)]
;;

let yojson_of_t t = `String t.content
let size t = Option.both t.width t.height
