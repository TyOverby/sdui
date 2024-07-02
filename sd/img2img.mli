open! Core
open! Async_kernel
open! Bonsai_web.Cont

module Info : sig
  type t =
    { seed : Int63.t
    ; enable_hr : bool
    }
  [@@deriving sexp]
end

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
    ; data_url : string
    }
  [@@deriving sexp, typed_fields]

  val apply_info : t -> Info.t -> t
end

val dispatch
  :  host_and_port:string
  -> Query.t
  -> (Base64_image.t * Info.t) list Or_error.t Effect.t
