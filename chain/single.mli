open! Core
open! Bonsai_web.Cont

val component
  :  pool:(Sd.Hosts.Host.t, 'a) Lease_pool.t
  -> prev:Sd.Base64_image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
