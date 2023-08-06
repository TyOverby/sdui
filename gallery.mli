open! Core
open! Bonsai_web

type t =
  { ongoing : bool
  ; wrap_request : 'a. 'a Effect.t -> 'a Effect.t
  ; add_images :
      params:Txt2img.Query.t -> images:Base64_image.t Or_error.t list -> unit Effect.t
  ; view : preview:Vdom.Node.t option -> Vdom.Node.t
  }

val component : set_params:(Txt2img.Query.t -> unit Effect.t) Value.t -> t Computation.t
