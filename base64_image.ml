open! Core
open! Bonsai_web

type t =
  { width : Int63.t option
  ; height : Int63.t option
  ; content : string
  }
[@@deriving sexp]

let of_string ?width ?height content = { width; height; content }
let to_string t = t.content

let to_vdom ?width ?height t =
  let width =
    match Option.first_some t.width width with
    | None -> Vdom.Attr.empty
    | Some width -> Vdom.Attr.create "width" (Int63.to_string width)
  in
  let height =
    match Option.first_some t.height height with
    | None -> Vdom.Attr.empty
    | Some height -> Vdom.Attr.create "height" (Int63.to_string height)
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
