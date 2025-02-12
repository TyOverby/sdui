open! Core
open! Async_kernel
open! Bonsai_web

module Query : sig
  type t =
    { image : Image.t
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
    ; mask : Image.t option
    ; ctrlnet : Alwayson_scripts.Ctrlnet.Query.t option
    }
  [@@deriving sexp, typed_fields, equal]

  val of_txt2img : Txt2img.Query.t -> image:Image.t -> mask:Image.t option -> t
  val apply_info : t -> Txt2img.Info.t -> t
end

val dispatch
  :  host_and_port:string
  -> Query.t
  -> (Image.t * Txt2img.Info.t) Or_error.t Effect.t
