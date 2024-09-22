open! Bonsai_web

type t

val empty : t

val make
  :  index:int Bonsai.t
  -> reset:unit Effect.t Bonsai.t
  -> gallery_view:Vdom.Node.t Bonsai.t
  -> form_view:Vdom.Node.t Bonsai.t
  -> color_picker:Vdom.Node.t Bonsai.t
  -> pen_size_slider:Vdom.Node.t Bonsai.t
  -> layer_panel:Vdom.Node.t Bonsai.t
  -> forward_button:Vdom.Node.t Bonsai.t
  -> clear_button:Vdom.Node.t Bonsai.t
  -> widget:Vdom.Node.t Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

val for_first_node
  :  first_image_view:Vdom.Node.t
  -> form_view:Vdom.Node.t
  -> gallery:Vdom.Node.t
  -> t

val finalize
  :  t
  -> hosts:Vdom.Node.t
  -> queue:Vdom.Node.t
  -> route:Vdom.Node.t
  -> Vdom.Node.t
