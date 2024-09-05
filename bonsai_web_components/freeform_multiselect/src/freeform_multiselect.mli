open! Core
open! Bonsai_web

(** These controls come unstyled by default. jane-web-style provides css that will make
    the control and option pills pretty. *)

val create
  :  ?extra_attr:Vdom.Attr.t Bonsai.t
  -> ?placeholder:string
  -> ?on_set_change:(String.Set.t -> unit Ui_effect.t) Bonsai.t
  -> ?split:(string -> string list)
  -> unit
  -> Bonsai.graph
  -> (String.Set.t * Vdom.Node.t * (String.Set.t -> unit Ui_effect.t)) Bonsai.t
