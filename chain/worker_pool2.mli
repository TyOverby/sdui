open! Core
open! Bonsai_web.Cont

type t

val enqueue
  :  t
  -> ?sexp_of:('a -> Sexp.t)
  -> spec:Sd.Models.t
  -> f:(Sd.Hosts.Host.t -> 'a -> 'b Effect.t)
  -> 'a
  -> 'b Effect.t

val debug : t -> Vdom.Node.t
val component : hosts:Sd.Hosts.Host.Set.t Bonsai.t -> Bonsai.graph -> t Bonsai.t
