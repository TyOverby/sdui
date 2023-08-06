open! Core
open! Async_kernel
open! Bonsai_web

module Query : sig
  type t =
    { prompt : string
    ; negative_prompt : string
    ; width : Int63.t
    ; height : Int63.t
    ; steps : Int63.t
    ; cfg_scale : Int63.t
    ; sampler : Samplers.t
    ; seed : Int63.t
    ; styles : Styles.t
    }
  [@@deriving sexp, typed_fields]
end

val dispatch : host_and_port:string -> Query.t -> Base64_image.t list Or_error.t Effect.t
