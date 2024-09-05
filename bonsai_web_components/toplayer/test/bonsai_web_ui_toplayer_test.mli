open! Core
open Bonsai_web
open Bonsai_web_ui_toplayer
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

(* The following are untestable without a DOM simulator:
   - Actual positioning of the popover
   - `showPopover`/`hidePopover on the popover DOM element, because we can't run
     hook logic
   - close on click / escape outside, because we don't can't run global listener logic,
     or test event propagation.
*)

module rec Popover_inputs : sig
  type t =
    { content : Node_helpers.t
    ; arrow : Node_helpers.t option
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    ; nested_anchored_popovers : Anchored_popover.t list
    }
end

and Anchored_popover : sig
  type t

  val inputs : t -> Popover_inputs.t
  val content_vdom : t -> Vdom.Node.t

  (** We don't have acccess to the vdom for the entire popover in tests, just the content.
      This function makes a best-effort attempt to reconstruct it, with some limitations:
        - Extra attrs passed to the root popover node are not directly accessible.
  *)
  val best_effort_wrap_content_in_popover_vdom : t -> Vdom.Node.t
end

module Virtual_popover : sig
  type t =
    { inputs : Popover_inputs.t
    ; popover_vdom : Vdom.Node.t
    }
end

module Modal : sig
  type t =
    { vdom : Vdom.Node.t
    ; content : Node_helpers.t
    ; lock_body_scroll : bool
    ; nested_anchored_popovers : Anchored_popover.t list
    }
end

module Result : sig
  (* NOTE: These could probably be [Bonsai.t]s, but it's pretty likely that we'll
     eventually want to provide a non-Bonsai API for accessing them in tests. *)
  type t =
    { anchored_popovers : Anchored_popover.t list
    ; virtual_popovers : Virtual_popover.t list
    ; modals : Modal.t list
    }

  val wrap_app_vdom : t -> Vdom.Node.t -> Vdom.Node.t
end

(** Tests for components that use toplayer elements should wrap their vdom in this
    function before passing it to [Handle.create].

    The output Vdom will include the content of all popovers and modals. *)
val wrap_app_vdom : Vdom.Node.t Bonsai.t -> Vdom.Node.t Bonsai.t

val get_toplayer_elements : Vdom.Node.t Bonsai.t -> Result.t Bonsai.t
