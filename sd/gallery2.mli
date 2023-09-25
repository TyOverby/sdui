open! Core
open! Bonsai_web

type t =
  { queue_request : Txt2img.Query.t -> unit Effect.t
  ; view : Vdom.Node.t
  }

val component
  :  request_host:Hosts.request_host Value.t
  -> set_params:(Txt2img.Query.t -> unit Effect.t) Value.t
  -> t Computation.t
