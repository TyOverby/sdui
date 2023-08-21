open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form

type t [@@deriving sexp, yojson]

val form : host_and_port:string Value.t -> t Form.t Computation.t
