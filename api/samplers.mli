open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_automatic_view

type t [@@deriving sexp, yojson, equal]

val default : t
val form : hosts:Hosts.t Bonsai.t -> local_ Bonsai.graph -> (t Form.t * Vdom.Node.t) Bonsai.t
