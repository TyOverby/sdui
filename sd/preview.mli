open! Core
open! Bonsai_web.Cont

val component
  :  ?params:Txt2img.Query.t Bonsai.t
  -> Progress.t Or_error.t Bonsai.t
  -> Vdom.Node.t option Bonsai.t
