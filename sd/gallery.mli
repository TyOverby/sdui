open! Core
open! Bonsai_web.Cont

type t =
  { queue_request : Txt2img.Query.t -> unit Effect.t
  ; view : Vdom.Node.t
  }

val component
  :  hosts:Hosts.t Bonsai.t
  -> set_params:(Txt2img.Query.t -> unit Effect.t) Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t
