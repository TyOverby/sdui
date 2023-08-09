open! Core
open! Bonsai_web

val component
  :  width:Int63.t Value.t
  -> height:Int63.t Value.t
  -> ongoing: bool Value.t
  -> Progress.t Or_error.t Value.t
  -> Vdom.Node.t option Computation.t
