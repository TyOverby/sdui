open! Core
open! Bonsai_web.Cont
open Shared

type kind =
  | Base64
  | Url
[@@deriving sexp, equal]

type t =
  { width : Int63.t option
  ; height : Int63.t option
  ; content : string
  ; kind : kind
  }
[@@deriving sexp, equal]

let data_url t = t.content
let of_string ?width ?height ~kind content = { width; height; content; kind }
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
  let src =
    match t.kind with
    | Base64 -> sprintf "data:image/png;base64, %s" t.content
    | Url -> t.content
  in
  Vdom.Node.img ~attrs:([ Vdom.Attr.src src; width; height ] @ attrs) ()
;;

let t_of_yojson = function
  | `String content ->
    let kind = if String.is_suffix ~suffix:".png" content then Url else Base64 in
    { content; width = None; height = None; kind }
  | other -> raise_s [%message "unknown base64 image json" (other : Yojson_safe.t)]
;;

let yojson_of_t t = `String t.content
let size t = Option.both t.width t.height
