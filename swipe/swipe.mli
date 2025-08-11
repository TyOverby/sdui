open! Core
open! Bonsai_web

val component
  :  tabs:Vdom.Node.t Bonsai.t
  -> local_ Bonsai.graph
  -> Virtual_dom.Vdom.Node.t Bonsai.t
