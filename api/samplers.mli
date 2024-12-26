open! Core
open! Bonsai_web
module Form := Bonsai_web_ui_form.With_automatic_view

type t [@@deriving sexp, yojson, equal]

val to_string : t -> string
val default : t

val all
  :  hosts:_ Hosts.Host.Map.t Bonsai.t
  -> local_ Bonsai.graph
  -> t list Or_error.t Bonsai.t

val form
  :  hosts:_ Hosts.Host.Map.t Bonsai.t
  -> local_ Bonsai.graph
  -> (t Form.t * Vdom.Node.t) Bonsai.t
