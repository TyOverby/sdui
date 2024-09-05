open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Theme_id : sig
  type t =
    | Default
    | Kado
    | Kado_light
    | Kado_white_bg
end

val component
  :  ?default:Theme_id.t
  -> ?standalone:bool
  -> unit
  -> Bonsai.graph
  -> (View.Theme.t * Vdom.Node.t) Bonsai.t
