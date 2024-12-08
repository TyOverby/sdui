open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_manual_view

type t = string option [@@deriving sexp, yojson, compare]

module Current_model : sig
  type t = { sd_model_checkpoint : string } [@@deriving yojson, sexp_of]

  val dispatch_set : string * t -> unit Or_error.t Ui_effect.t
  val current : hosts:Hosts.t Bonsai.t -> local_ Bonsai.graph -> string option Bonsai.t
end

val form
  :  hosts:Hosts.t Bonsai.t
  -> available_hosts:Hosts.Host.Set.t Bonsai.t
  -> local_ Bonsai.graph
  -> (t, Vdom.Node.t) Form.t Bonsai.t
