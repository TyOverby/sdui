open! Core
open! Bonsai_web.Cont

val component
  :  pool:(Sd.Hosts.Host.t, _, _) Lease_pool.t
  -> Bonsai.graph
  -> Vdom.Node.t list Bonsai.t
