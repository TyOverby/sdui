open! Core
open Bonsai_web
open Bonsai_web_test

(** This library provides a function that lets you mock interactions with
    [Bonsai_web_ui_drag_and_drop] in bonsai tests. *)

val run
  :  ('a, 'b) Handle.t
  -> get_vdom:('a -> Vdom.Node.t)
  -> name:string
  -> Bonsai_web_ui_drag_and_drop.For_testing.Action.t
  -> unit
