open! Core
open! Bonsai_web

val component
  :  ?params:Txt2img.Query.t Value.t
  -> Progress.t Or_error.t Value.t
  -> Vdom.Node.t option Computation.t
