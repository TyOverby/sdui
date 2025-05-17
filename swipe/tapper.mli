open! Core
open! Bonsai_web

val component
  :  on_remove:unit Effect.t Bonsai.t
  -> refine:unit Effect.t Bonsai.t
  -> reimagine:unit Effect.t Bonsai.t
  -> upscale:unit Effect.t Bonsai.t
  -> local_ Bonsai.graph
  -> (Vdom.Node.t -> Vdom.Node.t) Bonsai.t
