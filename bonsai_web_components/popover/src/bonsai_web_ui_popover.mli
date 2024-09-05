open! Core
open Bonsai_web

module Result : sig
  type t =
    { wrap : Vdom.Node.t -> Vdom.Node.t
    (** [wrap] is a function that you can call to attach the popover to an element. *)
    ; open_ : unit Effect.t (** effect that when scheduled, will open the popover. *)
    ; close : unit Effect.t (** effect that when scheduled, will close the popover. *)
    ; toggle : unit Effect.t
    (** effect that when scheduled, will close or open the popover depending on the current state.*)
    ; is_open : bool
    }
end

module Direction : sig
  type t =
    | Left
    | Right
    | Down
    | Up
end

module Alignment : sig
  type t =
    | Start
    | Center
    | End
end

(** Popover's are similar to tooltips, but they can contain arbitrary state of their own,
    you can also control when they open rather than opening by default when hovering. The
    "base" of the component is the element that is always there. The "popover" itself is the
    element that pops out when then [open_] effect is scheduled. You have full control over the
    creation of the "base" element by the function that is given to the popover. The popover element
    that you create through the popover function is later wrapped around another div to
    handle the "hovering" styles for you. You can attach extra attrs to this element through
    [popover_extra_attr]. The popover element itself also has some default styling of its own
    which you can override using [popover_style_attr].

    [component] will "wrap" your "base" around a [Vdom.Node.span]. If you want to attach
    attributes to the wrapping span, you can use [?base_extra_attr].

    [close_when_clicked_outside], when set to true, will close the popover if
    a click occurs outside of the popover. *)
val component
  :  ?popover_extra_attr:Vdom.Attr.t Bonsai.t
  -> ?popover_style_attr:Vdom.Attr.t Bonsai.t
  -> ?base_extra_attr:Vdom.Attr.t Bonsai.t
  -> ?allow_event_propagation_when_clicked_outside:
       ([ `Left_click | `Right_click | `Escape ] -> bool) Bonsai.t
  -> ?on_close:unit Effect.t Bonsai.t
  -> ?keep_popover_inside_window:bool Bonsai.t
  -> close_when_clicked_outside:bool Bonsai.t
  -> direction:Direction.t Bonsai.t
  -> alignment:Alignment.t Bonsai.t
  -> popover:(close:unit Effect.t Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> unit
  -> Bonsai.graph
  -> Result.t Bonsai.t
