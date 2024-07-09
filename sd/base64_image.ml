open! Core
open! Bonsai_web.Cont
open Shared

type t =
  { width : Int63.t option
  ; height : Int63.t option
  ; content : string
  }
[@@deriving sexp, equal]

let data_url t = t.content
let of_string ?width ?height content = { width; height; content }
let to_string t = t.content

let to_vdom ?(attrs = []) ?width ?height ?(drop_size = false) t =
  let width =
    match drop_size, Option.first_some width t.width with
    | true, _ | _, None -> Vdom.Attr.empty
    | false, Some width -> Vdom.Attr.create "width" (Int63.to_string width)
  in
  let height =
    match drop_size, Option.first_some height t.height with
    | true, _ | _, None -> Vdom.Attr.empty
    | _, Some height -> Vdom.Attr.create "height" (Int63.to_string height)
  in
  Vdom.Node.img
    ~attrs:
      ([ Vdom.Attr.src (sprintf "data:image/png;base64, %s" t.content); width; height ] @ attrs)
    ()
;;

let t_of_yojson = function
  | `String content -> { content; width = None; height = None }
  | other -> raise_s [%message "unknown base64 image json" (other : Yojson_safe.t)]
;;

let yojson_of_t t = `String t.content
let size t = Option.both t.width t.height
