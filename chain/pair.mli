
open! Core
open! Bonsai_web.Cont

val component
  :
   pool:(Sd.Hosts.Host.t, 'a) Lease_pool.t
  -> prev:Sd.Base64_image.t list Inc.Or_error_or_stale.t option Bonsai.t
  -> Bonsai.graph
  -> (Sd.Base64_image.t list Inc.Or_error_or_stale.t option * Vdom.Node.t) Bonsai.t
