open! Core
open! Bonsai_web

type t =
  { queue_request : Txt2img.Query.t -> unit Effect.t
  ; view : Vdom.Node.t
  }

val component
  :  host_and_port:string Value.t
  -> set_params:(Txt2img.Query.t -> unit Effect.t) Value.t
  -> t Computation.t
