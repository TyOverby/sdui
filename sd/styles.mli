open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form

type t [@@deriving sexp, yojson]

val form : request_host:Hosts.request_host Value.t -> t Form.t Computation.t
