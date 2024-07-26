open! Core
open! Bonsai_web.Cont

type t =
  { image : Sd.Image.t
  ; mask : Sd.Image.t option
  }

val component : prev:Sd.Image.t Bonsai.t -> Bonsai.graph -> t Inc.t * Vdom.Node.t Bonsai.t

val multi
  :  prev:Sd.Image.t list Inc.Or_error_or_stale.t Bonsai.t
  -> Bonsai.graph
  -> t list Inc.t * Vdom.Node.t Bonsai.t
