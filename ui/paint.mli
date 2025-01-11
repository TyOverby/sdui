open! Core
open! Bonsai_web

module Images : sig
  type t =
    { image : Sd.Image.t
    ; mask : Sd.Image.t option
    }
end

module View : sig
  type t =
    { color_picker : Vdom.Node.t
    ; pen_size_slider : Vdom.Node.t
    ; layer_panel : Vdom.Node.t
    ; alt_panel : Vdom.Node.t
    ; flip_button : Vdom.Node.t
    ; forward_button : Vdom.Node.t
    ; clear_button : Vdom.Node.t
    ; clone_button : Vdom.Node.t
    ; padding : Vdom.Node.t
    ; widget : Vdom.Node.t
    }
end

type t =
  { images : Images.t Inc.t
  ; get_images : Images.t Effect.t Bonsai.t
  ; view : View.t Bonsai.t
  ; set_paint_image : (Sd.Image.t -> unit Effect.t) option Bonsai.t
  }

val component : prev:Sd.Image.t Bonsai.t -> local_ Bonsai.graph -> t
val empty_white_image : int -> int -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
