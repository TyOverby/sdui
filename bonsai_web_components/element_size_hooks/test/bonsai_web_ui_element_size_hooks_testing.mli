open! Core
open Bonsai_web
open Bonsai_web_test

(** This library provides a function that lets you mock interactions with
    [Bonsai_web_ui_element_size_hooks] in bonsai tests. *)

module Bulk_size_tracker : sig
  type change =
    { selector : string
    ; width : float
    ; height : float
    }

  val change_sizes
    :  ('a, 'b) Handle.t
    -> get_vdom:('a -> Vdom.Node.t)
    -> change list
    -> unit
end

module Position_tracker : sig
  type change =
    { selector : string
    ; top : int
    ; left : int
    ; width : int
    ; height : int
    }

  val change_positions
    :  ('a, 'b) Handle.t
    -> get_vdom:('a -> Vdom.Node.t)
    -> change list
    -> unit
end
