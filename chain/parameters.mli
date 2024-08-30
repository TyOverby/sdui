open! Core
open! Bonsai_web.Cont
module Form := Bonsai_web_ui_form.With_manual_view

type t

type view :=
  direction:[ `Horizontal | `Vertical ]
  -> theme:View.Theme.t
  -> reset:unit Ui_effect.t
  -> Vdom.Node.t

val component : Bonsai.graph -> (t, view) Form.t Bonsai.t
val for_txt2img : t -> Sd.Txt2img.Query.t
val for_img2img : t -> Sd.Img2img.Query.t
val num_images : t -> int
