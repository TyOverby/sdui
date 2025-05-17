open! Core
open! Bonsai_web

val component
  :  on_remove:unit Effect.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Attr.t Bonsai.t
