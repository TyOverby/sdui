open! Core
open! Async_kernel
open! Bonsai_web

module Query : sig
  type t =
    { module_ : string
    ; image : Image.t
    }
  [@@deriving sexp, typed_fields, equal]
end

val dispatch : host_and_port:string -> Query.t -> Image.t Or_error.t Effect.t
