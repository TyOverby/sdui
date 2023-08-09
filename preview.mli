open! Core
open! Bonsai_web

val component
  : ongoing: Txt2img.Query.t option Value.t
  -> Progress.t Or_error.t Value.t
  -> Vdom.Node.t option Computation.t
