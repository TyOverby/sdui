open! Core
open! Bonsai_web

val bonsai_driven : Vdom.Node.t Bonsai.t -> Bonsai.graph -> unit

module For_testing : sig
  val portals
    : Vdom_toplayer.For_bonsai_web_ui_toplayer.Portal.t String.Map.t Bonsai.Expert.Var.t
end
