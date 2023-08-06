open! Core
open! Bonsai_web

type t =
  { progress : float
  ; eta_relative : float
  ; state : Yojson_safe.t
  ; current_image : Base64_image.t option
  }
[@@deriving sexp_of]

val state : host_and_port:string Value.t -> t Or_error.t Computation.t
