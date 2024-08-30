open! Core
open! Bonsai_web.Cont

val component : pool:_ Lease_pool.t -> Vdom.Node.t Bonsai.t
