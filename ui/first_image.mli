open! Core
open! Bonsai_web

val component
  :  Bonsai.graph
  -> Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t * Vdom.Node.t Bonsai.t
