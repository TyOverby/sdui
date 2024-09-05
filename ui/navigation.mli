open! Core
open! Bonsai_web

val component
  :  pool:(Sd.Hosts.Host.t, 'a, 'b) Lease_pool.t
  -> hosts_view:Vdom.Node.t Bonsai.t
  -> lease_pool_view:Vdom.Node.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
