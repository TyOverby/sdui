open! Core
open! Bonsai_web
module Form := Bonsai_web_ui_form.With_manual_view

type t =
  { seed : Int63.t
  ; pos_prompt : string
  ; neg_prompt : string
  ; width : Int63.t
  ; height : Int63.t
  ; steps : Int63.t
  ; cfg : Int63.t
  ; denoise : Int63.t
  ; ratios : string
  ; num_images : int
  ; specific_model : Sd.Hosts.Current_model.t option
  }
[@@deriving equal]

type view :=
  direction:[ `Horizontal | `Vertical ]
  -> theme:View.Theme.t
  -> reset:unit Ui_effect.t
  -> Vdom.Node.t

val component
  :  ?models:Sd.Hosts.Current_model.Set.t Bonsai.t
  -> local_ Bonsai.graph
  -> (t, view) Form.t Bonsai.t

val for_txt2img : t -> Sd.Txt2img.Query.t
val for_img2img : t -> Sd.Img2img.Query.t
val num_images : t -> int
