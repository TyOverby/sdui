open! Core
open! Bonsai_web.Cont
open Shared

type t =
  { progress : float
  ; eta_relative : float
  ; state : Yojson_safe.t
  ; current_image : Image.t option
  }
[@@deriving sexp_of]

val state : host_and_port:string Bonsai.t -> Bonsai.graph -> t Or_error.t Bonsai.t
