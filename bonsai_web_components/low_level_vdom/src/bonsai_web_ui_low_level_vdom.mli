open! Core
open! Js_of_ocaml
open Bonsai_web
open! Virtual_dom

(** The [Virtual_dom] library provides 2 lower-level APIs for accessing raw DOM nodes
    and their lifecycles:

    - A [Virtual_dom.Vdom.Node.widget] allows you to create a [Vdom.Node.t] that is
      entirely managed by you: you are responsible for creating a [Dom_html.element Js.t]
      in [init], patching it through [update], and cleaning up on [destroy].
    - A [Virtual_dom.Vdom.Attr.Hooks.t] allows you to create a [Vdom.Attr.t] that you can
      attach to any Vdom element, allowing you to run custom logic on [init], [update],
      and [destroy].

    Both [widget] and [Hooks] can have their own state, and receive inputs.
    You should use [widget] when you intend to fully manage some portion of the DOM,
    and [Hooks] if you want to add behavior to some existing Vdom.

    This library wraps those Vdom primitives in a Bonsai wrapper, allowing you to read
    and run effects against the internal state / input of hooks and widgets.

    It also provides [Dom_ref.t], which is a hook that lets you get the
    [Dom_html.element Js.t]s generated for some [Vdom.Node.t] in effects. *)

module Widget : sig
  module type S = sig
    type element = private #Dom_html.element
    type input
    type state

    (** The [init] function is used to construct the widget's initial state and the
      dom element that it is tied to.  It is passed the input for the widget, alongside
      a [get_input] function that will always return the most recent input passed to the
      widget.  You can close over [get_input] or store it in your [state]. *)
    val init : get_input:(unit -> input) -> input -> state * element Js.t

    (** Update is called whenever the input to the widget changes.  The most recent input
      value that was passed to the widget is available via [prev_input]. *)
    val update : prev_input:input -> input -> state -> element Js.t -> element Js.t

    (** [destroy] is called when the widget leaves the page.  You should do any resource
      cleanup here. *)
    val destroy : input -> state -> element Js.t -> unit
  end

  type ('input, 'state) t = private
    { view : Vdom.Node.t (** The view of the widget *)
    ; modify : ('input -> 'state -> unit) -> unit Effect.t
    (** A callback for modifying the widget.  The most recent inputs and the current state
      of the widgets are provided. *)
    ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
    (** [read] lets you look at the most recent input and current state of any instances
      of the widget. *)
    }

  (** A widget excludes some portion of the DOM from being patched by Virtual DOM,
      and provides [init] and [update] functions you can use to create and manage that
      DOM yourself.
      A common usecase is integrating a non-Bonsai/Vdom UI component into your app. *)
  val component
    :  ?vdom_for_testing:('input -> Vdom.Node.t)
    -> (module S with type input = 'input and type state = 'state)
    -> 'input Bonsai.t
    -> Bonsai.graph
    -> ('input, 'state) t Bonsai.t
end

module Hook : sig
  module type S = sig
    type input
    type state

    (** The [init] function is used to construct the hook's initial state, and run any
      "initialization" side effects. It will be called after the [Dom_html.element Js.t]
      is created, but potentially before it is mounted into the DOM.

      It is passed the input for the widget, alongside a [get_input] function that will
      always return the most recent input passed to the widget.
      You can close over [get_input] or store it in your [state]. *)
    val init : get_input:(unit -> input) -> input -> Dom_html.element Js.t -> state

    (** [update] is called whenever the input to the hook changes. The most recent input
      value that was passed to the widget is available via [prev_input]. *)
    val update : prev_input:input -> input -> state -> Dom_html.element Js.t -> unit

    (* [destroy] is called when the underlying DOM element leaves the page, or if the hook
       attr is no longer set on the corresponding [Vdom.Node.t]. *)
    val destroy : input -> state -> Dom_html.element Js.t -> unit
  end

  type ('input, 'state) t = private
    { attr : Vdom.Attr.t
    ; modify : ('input -> 'state -> unit) -> unit Effect.t
    (** [modify] allows you to run a side effect, with access to the hook's state and
        most recent input.*)
    ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
    (** [read] lets you look at the most recent input and current state of all instances
        of the hook. *)
    }

  (* A hook lets you run custom logic when the DOM element corresponding to some
     [Vdom.Node.t] is created, or when inputs change. *)
  val component
    :  (module S with type input = 'input and type state = 'state)
    -> hook_name:string
    -> 'input Bonsai.t
    -> Bonsai.graph
    -> ('input, 'state) t Bonsai.t
end

module Dom_ref : sig
  type t =
    { attr : Vdom.Attr.t
    ; nodes : Dom_html.element Js.t list Effect.t
    }

  (** Allows you to get the [Dom_html.element Js.t]s for some [Vdom.Node.t] in effects.
      You should not store the output of the [nodes] effect in state, because it is
      mutable and may change unexpectedly. *)
  val tracker : Bonsai.graph -> t Bonsai.t
end

module Mutable_state_tracker : sig
  (** A mutable-state tracker is meant to be used in concert with [Vdom.Node.widget] or
      [Vdom.Attr.create_hook].  Because a widget and hook can exist in multiple places in
      the dom at the same time, this state-tracker actually tracks a collection of states,
      which is why [read] returns a list, and the callback you pass to [modify] can get
      called multiple times per invocation.

      [unsafe_init] should be called inside the widget or hooks's [init] function and is
      passed some subset of the widget's state.  Then, you must store the returned Id.t in
      the widget's state.  If [unsafe_init] is called, then you _must_ call [unsafe_destroy]
      with the returned ID or you risk leaking memory, and making the output of calls to
      [read] return stale data.

      [unsafe_destroy] should be called inside the widget or hook's [destroy] function,
      and must be passed the same [id] that was returned from the corresponding [create].

      [modify] can be invoked to run some mutating function on the widget's state.  It is legal to
      use this function anywhere that an Effect can be scheduled.

      [read] provides access to the states of each instance managed by this mutable-state-tracker.
      You can use this function anywhere that an Effect can be scheduled. *)

  module Id : T

  type 's t =
    { unsafe_init : 's -> Id.t
    ; unsafe_destroy : Id.t -> unit
    ; modify : ('s -> unit) -> unit Effect.t
    ; read : 'a. ('s -> 'a) -> 'a list Effect.t
    }

  val component : unit -> Bonsai.graph -> 's t Bonsai.t
end
