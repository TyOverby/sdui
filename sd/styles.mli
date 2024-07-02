open! Core
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view

type t [@@deriving sexp, yojson]

val form : request_host:Hosts.request_host Bonsai.t -> Bonsai.graph -> t Form.t Bonsai.t
