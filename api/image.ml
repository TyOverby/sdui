open! Core
open! Bonsai_web
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

let with_size t ~width ~height = { t with width = Some width; height = Some height }

let of_string ?width ?height ~kind content =
  let content =
    match kind with
    | Base64 ->
      if String.is_prefix ~prefix:"data:" content
      then content
      else "data:image/png;base64, " ^ content
    | _ -> content
  in
  { width; height; content; kind }
;;

let empty =
  of_string
    ~width:(Int63.of_int 0)
    ~height:(Int63.of_int 0)
    ~kind:Base64
    {|data:image/png;base64,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|}
;;

let is_empty = phys_equal empty
let to_string t = t.content

let to_vdom ?(attrs = []) ?width ?height ?(drop_size = false) t =
  let width =
    match drop_size, Option.first_some width t.width with
    | true, _ | _, None -> Vdom.Attr.empty
    | false, Some width ->
      Vdom.Attr.create
        "width"
        (Virtual_dom.Dom_float.to_string
           (Int63.to_float width
            /. Js_of_ocaml.Js.float_of_number
                 Js_of_ocaml.Dom_html.window##.devicePixelRatio))
  in
  let height =
    match drop_size, Option.first_some height t.height with
    | true, _ | _, None -> Vdom.Attr.empty
    | _, Some height ->
      Vdom.Attr.create
        "height"
        (Virtual_dom.Dom_float.to_string
           (Int63.to_float height
            /. Js_of_ocaml.Js.float_of_number
                 Js_of_ocaml.Dom_html.window##.devicePixelRatio))
  in
  Vdom.Node.img ~attrs:([ Vdom.Attr.src t.content; width; height ] @ attrs) ()
;;

let t_of_yojson = function
  | `String content ->
    let kind = if String.is_suffix ~suffix:".png" content then Url else Base64 in
    { content; width = None; height = None; kind }
  | other -> raise_s [%message "unknown base64 image json" (other : Yojson_safe.t)]
;;

let yojson_of_t t = `String t.content
let size t = Option.both t.width t.height
