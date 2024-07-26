open! Core
open! Bonsai_web.Cont

val component
  :  prev:Sd.Image.t Bonsai.t
  -> is_mask:bool
  -> Bonsai.graph
  -> Sd.Image.t Inc.t * Vdom.Node.t Bonsai.t

val multi
  :  prev:Sd.Image.t list Inc.Or_error_or_stale.t Bonsai.t
  -> is_mask:bool
  -> Bonsai.graph
  -> Sd.Image.t list Inc.Or_error_or_stale.t Bonsai.t * Vdom.Node.t Bonsai.t
