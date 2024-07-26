open! Core
open! Bonsai_web.Cont

type t =
  { view : Vdom.Node.t
  ; preview_view : Vdom.Node.t option
  ; queue_request : Txt2img.Query.t -> unit Effect.t
  }

val component
  :  host_and_port:string Bonsai.t
  -> add_images:
       (params:Txt2img.Query.t
        -> images:(Image.t * Txt2img.Info.t) Or_error.t list
        -> unit Effect.t)
         Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t
