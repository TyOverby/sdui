open! Core
open! Async_kernel
open! Bonsai_web

module Info : sig
  type t =
    { seed : Int63.t
    ; enable_hr : bool
    }
  [@@deriving sexp]
end

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
    ; subseed_strength : float
    ; denoising_strength : float
    ; styles : Styles.t
    ; enable_hr : bool
    ; ctrlnet : Alwayson_scripts.Ctrlnet.Query.t option
    ; regional_prompter : Alwayson_scripts.Regional_prompter.Query.t option
    ; hr_upscaler : Upscaler.t
    }
  [@@deriving sexp, typed_fields, equal]

  val apply_info : t -> Info.t -> t
end

val dispatch : host_and_port:string -> Query.t -> (Image.t * Info.t) Or_error.t Effect.t
