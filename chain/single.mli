open! Core
open! Bonsai_web.Cont

val component
  :  default_size:int
  -> pool:(Sd.Hosts.Host.t, 'a) Lease_pool.t
  -> prev:Sd.Base64_image.t list Inc.Or_error_or_stale.t  option Bonsai.t
  -> Bonsai.graph
  -> Sd.Base64_image.t list Inc.Or_error_or_stale.t Bonsai.t * Vdom.Node.t Bonsai.t
