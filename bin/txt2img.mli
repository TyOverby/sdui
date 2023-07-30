open! Core
open! Async_kernel
open! Bonsai_web

module Query : sig
  type t =
    { prompt : string
    ; negative_prompt : string
    ; width : int
    ; height : int
    }
  [@@deriving sexp, typed_fields]
end

val dispatch : host_and_port:string -> Query.t -> Base64_image.t list Or_error.t Effect.t
