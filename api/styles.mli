open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_manual_view

type t [@@deriving sexp, yojson, equal]

val none : t
val form : hosts:Hosts.t Bonsai.t -> local_ Bonsai.graph -> (t, Vdom.Node.t) Form.t Bonsai.t
