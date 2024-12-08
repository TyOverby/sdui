open! Core
open! Bonsai_web

val component
  :  data_to_string:('a -> string)
  -> pool:(_, 'a, _) Lease_pool.t
  -> Vdom.Node.t Bonsai.t
