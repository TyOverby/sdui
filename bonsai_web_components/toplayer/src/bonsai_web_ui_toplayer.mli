open! Core
open! Bonsai_web

(** Toplayer elements will not show up in tests by default, because they are portalled
    outside of the app root. You should use [bonsai_web_ui_toplayer_test] as a helper
    library in your testing. *)

(** [bonsai_web_ui_toplayer] contains stateful Bonsai popovers and modals:
    UI elements that appear in the browser top layer, on top of everything else in your
    web UI.

    The [Vdom.Attr.t]s produced by these computations will position the popover/modal/etc
    relative to the DOM node where the attr is attached.

    Tooltips should not contain stateful elements, so they are accessible via
    [View.tooltip_attr] without a `Bonsai.t` wrapper.

    The main difference between popovers and modals is that modals "block" the page
    content under them. For instance:
    - Moving your mouse outside of a modal should not trigger `hover` on any elements not
      in that modal.
    - Clicking outside of a modal will close only that modal, and click events will not
      propagate.

    All default stylings can be overriden through the [for_toplayer] methods of the
    [View.Theme.t].

    These implementations retain their state when closed, including whether any nested
    elements are open. You might want to wrap their contents in a [Bonsai.with_model_resetter'],
    and use an [on_deactivate] lifecycle hook to clear state, or a [Bonsai.scope_model]
    if the popover/modal is keyed by one of multiple inputs.

    Authors of reusable components might also be interested in the [vdom_toplayer]
    library, which is the foundation for this one. *)

module Position : sig
  type t = Floating_positioning_new.Position.t =
    | Auto
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Alignment : sig
  type t = Floating_positioning_new.Alignment.t =
    | Center
    | Start
    | End
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Offset : sig
  (** Allows controlling how far the floating element is positioned away from the anchor.
      Usually, you don't want to set cross_axis. *)
  type t = Floating_positioning_new.Offset.t =
    { main_axis : float
    ; cross_axis : float
    }
  [@@deriving sexp, sexp_grammar, equal, compare]

  (** Apply no offset. *)
  val zero : t
end

module Match_anchor_side : sig
  (** [Grow_to_match] will set [min-width] or [min-height]; [Match_exactly] will set [width] or [height],
      and [Shrink_to_match] will set [max-width] or [max-height].

      If not set here, max height and width will be set to the available space. *)
  type t = Floating_positioning_new.Match_anchor_side.t =
    | Grow_to_match
    | Match_exactly
    | Shrink_to_match
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Anchor : sig
  type t [@@deriving sexp_of]

  (** [top], [bottom], and [left], [right] are the # of pixels down and right from the
      top left corner to form the (top, bottom), and (left, right) borders of the
      virtual bounding box. *)
  val of_bounding_box : top:float -> left:float -> bottom:float -> right:float -> t

  (** [x] and [y] are the # of pixels right/down from the top left corner. *)
  val of_coordinate : x:float -> y:float -> t
end

module Close_on_click_outside : sig
  type t =
    | Yes
    | Yes_unless_target_is_popover
    | No
end

module Controls : sig
  type t =
    { open_ : unit Effect.t Bonsai.t
    ; close : unit Effect.t Bonsai.t
    ; is_open : bool Bonsai.t
    }

  module For_external_state : sig
    type t

    (** [create] allows you to react to outside clicks and escapes for popovers and
       modals where you own the state.
    *)
    val create
      :  close:unit Effect.t Bonsai.t
      -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
      -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
      -> ?close_on_esc:bool Bonsai.t
      -> Bonsai.graph
      -> t Bonsai.t
  end
end

module Popover : sig
  (** Popovers are a more powerful version of tooltips:

      - [content] has access to a [local_ graph], so they can have internal state.
      - [content] is only active while shown, so they can use [on_activate] and
        [on_deactivate] lifecycle hooks.
      - Multiple popovers can be open at a time; if 2 overlap, they will be stacked
        in the order that they were opened.
      - Bonsai owns popover open/closed state; tooltip state is owned by the browser. *)

  (** If [close_on_click_outside] (default [Yes]), clicks outside of a popover or its
      DOM descendants will close the popover and any descendant popovers.

      [close_on_right_click_outside] (default [No]) behaves similarly. If your popover is
      acting as a menu, you probably want to set this to [Yes].

      If [close_on_esc] (default true), pressing escape will close the focused popover
      and its descendants, and the event will not propagate.
      If focus is not on any popover, all popovers with [close_on_esc] will close.

      If [focus_on_open] (default false) is set to true, the popover root will be focused
      whenever it opens, unless some element inside it has the HTML [autofocus] attribute,
      or is otherwise already focused. Note that [Effect.Focus.on_activate] will NOT work,
      because popovers and modals open a few frames after their Bonsai computation is
      activated; use [Vdom.Attr.autofocus true] instead.

      [position] defaults to [Auto]
      [alignment] defaults to [Center]

      If [match_anchor_side_length] is set to true, the popover's main axis
      (width if position is Top/Bottom, height if position is Left/Right) will be set to
      have a length equal to the corresponding axis of the anchor. This is particularly
      useful for dropdowns and typeaheads.

      If you want to run some [unit Effect.t] on close, you can make an [on_deactivate]
      lifecycle hook inside of [content].

      Returns an "anchoring" [Vdom.Attr.t], as well as controls for opening/closing the
      popover. *)
  val create
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?has_arrow:bool Bonsai.t
    -> content:(close:unit Effect.t Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> Bonsai.graph
    -> Vdom.Attr.t Bonsai.t * Controls.t

  (** Like [create], but uses a virtual anchor (e.g. a bounding box or a coordinate pair)
      for anchoring instead of a real vdom element. *)
  val create_virtual
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?has_arrow:bool Bonsai.t
    -> content:(close:unit Effect.t Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> Anchor.t Bonsai.t
    -> Bonsai.graph
    -> Controls.t

  module For_external_state : sig
    (** These are like the regular popovers, but you own their open/closed state.

        The [opt] versions take [is_open: 'a option Bonsai.t], and are open if [is_open]
        is [Some 'a], and provide the ['a Bonsai.t] to [content].

        The [bool] versions don't accept an input, just [is_open: bool Bonsai.t].

        Typically, you'll calculate [is_open] as a function of some other state
        you own.

        They will be stacked in the order opened, so the popover whose [is_open] input
        last became [true] will appear on top. *)

    val opt
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?controls:Controls.For_external_state.t Bonsai.t
      -> ?position:Position.t Bonsai.t
      -> ?alignment:Alignment.t Bonsai.t
      -> ?offset:Offset.t Bonsai.t
      -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
      -> ?focus_on_open:bool Bonsai.t
      -> ?has_arrow:bool Bonsai.t
      -> is_open:'a option Bonsai.t
      -> content:('a Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> Bonsai.graph
      -> Vdom.Attr.t Bonsai.t

    val bool
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?controls:Controls.For_external_state.t Bonsai.t
      -> ?position:Position.t Bonsai.t
      -> ?alignment:Alignment.t Bonsai.t
      -> ?offset:Offset.t Bonsai.t
      -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
      -> ?focus_on_open:bool Bonsai.t
      -> ?has_arrow:bool Bonsai.t
      -> is_open:bool Bonsai.t
      -> content:(Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> Bonsai.graph
      -> Vdom.Attr.t Bonsai.t

    val opt_virtual
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?controls:Controls.For_external_state.t Bonsai.t
      -> ?position:Position.t Bonsai.t
      -> ?alignment:Alignment.t Bonsai.t
      -> ?offset:Offset.t Bonsai.t
      -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
      -> ?focus_on_open:bool Bonsai.t
      -> ?has_arrow:bool Bonsai.t
      -> is_open:'a option Bonsai.t
      -> content:('a Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> Anchor.t Bonsai.t
      -> Bonsai.graph
      -> unit

    val bool_virtual
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?controls:Controls.For_external_state.t Bonsai.t
      -> ?position:Position.t Bonsai.t
      -> ?alignment:Alignment.t Bonsai.t
      -> ?offset:Offset.t Bonsai.t
      -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
      -> ?focus_on_open:bool Bonsai.t
      -> ?has_arrow:bool Bonsai.t
      -> is_open:bool Bonsai.t
      -> content:(Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> Anchor.t Bonsai.t
      -> Bonsai.graph
      -> unit
  end
end

module Modal : sig
  (** Modals are like popovers, but when one is open, everything under the modal is inert:
      https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/inert

      If multiple modals are open at the same time, only the topmost one will not be
      inert.

      Inert elements will not be closed when clicking outside them, or on escape.

      Unlike popovers, [close_on_click_outside] defaults to
      [Yes_unless_target_is_popover], because if a popover appears outside of
      a modal, it should likely be interactible without closing the modal.

      Also unlike popovers, [focus_on_open] defaults to [true].

      If [lock_body_scroll] is set to true (default false), scrolling the page behind
      the modal will not be possible.

      You can style the modal backdrop by targetting the [::backdrop] pseudo-element via
      [extra_attrs].
  *)

  val create
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?lock_body_scroll:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> content:(close:unit Effect.t Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> Bonsai.graph
    -> Controls.t

  module For_external_state : sig
    val opt
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?controls:Controls.For_external_state.t Bonsai.t
      -> ?lock_body_scroll:bool Bonsai.t
      -> ?focus_on_open:bool Bonsai.t
      -> is_open:'a option Bonsai.t
      -> content:('a Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> Bonsai.graph
      -> unit

    val bool
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?controls:Controls.For_external_state.t Bonsai.t
      -> ?lock_body_scroll:bool Bonsai.t
      -> ?focus_on_open:bool Bonsai.t
      -> is_open:bool Bonsai.t
      -> content:(Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> Bonsai.graph
      -> unit
  end
end

module For_testing = Portal.For_testing
