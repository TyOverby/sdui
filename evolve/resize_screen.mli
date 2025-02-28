open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_manual_view

val component
  :  local_ Bonsai.graph
  -> (get_images:Sd_chain.Paint.Images.t Effect.t
      -> set_result:
           (new_width:Int63.t -> new_height:Int63.t -> Sd.Image.t -> unit Effect.t)
      -> Vdom.Node.t)
       Bonsai.t
