open! Core
open! Bonsai_web.Cont

val component
  :  prev:Sd.Base64_image.t Bonsai.t
  -> is_mask:bool
  -> Bonsai.graph
  -> Sd.Base64_image.t Inc.t * Vdom.Node.t Bonsai.t

val multi
  :  prev:Sd.Base64_image.t list Inc.Or_error_or_stale.t Bonsai.t
  -> is_mask:bool
  -> Bonsai.graph
  -> Sd.Base64_image.t list Inc.Or_error_or_stale.t Bonsai.t * Vdom.Node.t Bonsai.t
