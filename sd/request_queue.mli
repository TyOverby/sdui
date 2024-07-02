open! Core
open! Bonsai_web

type t =
  { view : Vdom.Node.t
  ; preview_view : Vdom.Node.t option
  ; queue_request : Txt2img.Query.t -> unit Effect.t
  }

val component
  :  host_and_port:string Value.t
  -> add_images:
       (params:Txt2img.Query.t
        -> images:(Base64_image.t * Txt2img.Info.t) Or_error.t list
        -> unit Effect.t)
         Value.t
  -> t Computation.t
