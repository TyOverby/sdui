open! Core
open! Bonsai_web.Cont

val component
  :  pool:(Sd.Hosts.Host.t, _, _) Lease_pool.t
  -> prev:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> Bonsai.graph
  -> (Sd.Image.t Inc.Or_error_or_stale.t option * Vdom.Node.t) Bonsai.t
