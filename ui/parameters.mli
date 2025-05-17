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
  ; sampler : Sd.Samplers.t
  ; specific_model : Sd.Hosts.Current_model.t option
  ; ctrlnet : Sd.Alwayson_scripts.Ctrlnet.Query.t option
  }
[@@deriving typed_fields, equal, sexp_of, fields ~getters]

type view :=
  direction:[ `Horizontal | `Vertical ]
  -> theme:View.Theme.t
  -> reset:unit Ui_effect.t
  -> Vdom.Node.t

type basic_view := theme:View.Theme.t -> reset:unit Ui_effect.t -> Vdom.Node.t

val component
  :  ?samplers:Sd.Samplers.t list Bonsai.t
  -> ?models:Sd.Hosts.Current_model.Set.t Bonsai.t
  -> local_ Bonsai.graph
  -> (t, view) Form.t Bonsai.t

val basic_component : local_ Bonsai.graph -> (t, basic_view) Form.t Bonsai.t
val for_txt2img : t -> Sd.Txt2img.Query.t
val for_img2img : t -> Sd.Img2img.Query.t
val num_images : t -> int
