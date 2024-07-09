open! Core
open! Async_kernel
open! Bonsai_web.Cont

module Query : sig
  type t =
    { init_images : Base64_image.t list
    ; prompt : string
    ; negative_prompt : string
    ; width : Int63.t
    ; height : Int63.t
    ; steps : Int63.t
    ; cfg_scale : Int63.t
    ; sampler : Samplers.t
    ; seed : Int63.t
    ; subseed_strength : float
    ; denoising_strength : float
    ; styles : Styles.t
    }
  [@@deriving sexp, typed_fields, equal]

  val of_txt2img : Txt2img.Query.t -> init_images:Base64_image.t list -> t
  val apply_info : t -> Txt2img.Info.t -> t
end

val dispatch
  :  host_and_port:string
  -> Query.t
  -> (Base64_image.t * Txt2img.Info.t) list Or_error.t Effect.t
