open! Core
open! Bonsai_web.Cont

module Images : sig
  type t =
    { image : Sd.Image.t
    ; mask : Sd.Image.t option
    }
end

type t =
  { images : Images.t Inc.t
  ; color_picker : Vdom.Node.t Bonsai.t
  ; pen_size_slider : Vdom.Node.t Bonsai.t
  ; layer_panel : Vdom.Node.t Bonsai.t
  ; forward_button : Vdom.Node.t Bonsai.t
  ; clear_button : Vdom.Node.t Bonsai.t
  ; widget : Vdom.Node.t Bonsai.t
  }

val component : prev:Sd.Image.t Bonsai.t -> Bonsai.graph -> t
val empty_white_image : int -> int -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
