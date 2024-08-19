open! Core
open! Bonsai_web.Cont

val component
  :  pool:(Sd.Hosts.Host.t, _, _) Lease_pool.t
  -> hosts_view:Vdom.Node.t Bonsai.t
  -> lease_pool_view:Vdom.Node.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
