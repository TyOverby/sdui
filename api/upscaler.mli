open! Core
open! Bonsai_web.Cont
module Form := Bonsai_web_ui_form.With_manual_view

type t [@@deriving sexp, yojson, equal]

val default : t
val form : hosts:Hosts.t Bonsai.t -> Bonsai.graph -> (t, Vdom.Node.t) Form.t Bonsai.t
