open! Core
open! Bonsai_web

(** This component supports splitting a larger pane into 2 smaller panes (horizontal or
    vertical) than can be resized by dragging.

    It supports size constraints of either the panels (percentage or absolute). Take a
    look at the split pane example to interactively see the effect of different
    parameters. *)

module Split_dir : sig
  (** The split can either be horizontal (first pane on top of second pane) or vertical
      (the second pane on top of the first pane) *)

  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp, equal]
end

module Size : sig
  (** Initial sizes and constraints can be given either as pixels or percentages *)

  type t =
    | Percent of Percent.t
    | Px of float
  [@@deriving sexp, equal]
end

module Panel : sig
  (** The split pane has two panels, the [First] and the [Second].

      - If the layout is horizontal, the first panel is on the left
      - If the layout is vertical, the first panel is on top *)

  type t =
    | First
    | Second
  [@@deriving sexp, equal]
end

module Panel_and_size : sig
  type t =
    { panel : Panel.t
    ; size : Size.t
    }
  [@@deriving sexp, equal]

  val percent : Panel.t -> Percent.t -> t
  val px : Panel.t -> float -> t
end

module Constraint : sig
  (** You can optionally choose to limit the maximum/or minimum of either panel in either
      percentage or absolute pixel terms.

      Setting multiple constraints will combine them to give the most restrictive
      constraint and should generally behave as expected.

      {4 More details}

      Constraints are combined to a single (maximum, minimum) constraint on the position
      of the separator within the container, by taking the most restrictive constraint for
      each side.

      The default constraints allow any valid position within the container.

      If multiple constraints apply to the same end (i.e. maximum size of the first panel
      & minimum size of the second) the most restrictive one wins.

      If both sides overlap (min > max) (which means the container is too small for the
      constraints to be satisfiable) resizing will cease to function. The point it's
      locked at is somewhat arbitrary -- currently it takes the midpoint of the min and
      max, but this shouldn't be considered stable. *)

  module Direction : sig
    type t =
      | Min
      | Max
    [@@deriving sexp, equal]
  end

  type t =
    { direction : Direction.t
    ; panel : Panel.t
    ; size : Size.t
    }
  [@@deriving sexp, equal]

  val max_percent : panel:Panel.t -> Percent.t -> t
  val min_percent : panel:Panel.t -> Percent.t -> t
  val max_px : panel:Panel.t -> float -> t
  val min_px : panel:Panel.t -> float -> t
  val of_panel_and_size : Direction.t -> Panel_and_size.t -> t
  val to_panel_and_size : t -> Panel_and_size.t
end

module On_container_resize : sig
  (** This specifies how the panel sizes should be adjusted when the amount of space
      allocated to the split pane resizes.

      - [Keep_proportion] attempts to keep the position of the separator in the same
        location relative to the size of the container.
      - [Try_to_keep_panel_size panel] attempts to keep one of the panels at the same size
        as it was before.

      In both cases the new size will be subject to the constraints, so may not actually
      match the exact previous proportion/size. *)
  type t =
    | Keep_proportion
    | Try_to_keep_panel_size of Panel.t
  [@@deriving sexp, equal]
end

(** Use to attach additional Vdom attributes to each of the divs constructed by [create] *)
module Panel_extra_attrs : sig
  type t =
    { first_panel : Vdom.Attr.t
    ; second_panel : Vdom.Attr.t
    ; separator : Vdom.Attr.t
    }
end

module Panel_sizes : sig
  type t =
    { first_panel_px : float
    ; second_panel_px : float
    }
  [@@deriving fields ~getters] [@@fields.no_zero_alloc]
end

type t

(** Creates a split pane. Include it in the page with [to_vdom].

    If the direction is [Horizontal], [first_panel] is on the left and [second_panel] on
    the right. If it is [Vertical], [first_panel] is on the top and [second_panel] on the
    bottom.

    By default the of the panes will hide any overflow; if you want to have scroll make
    sure to set overflow(-x/y) auto on the node you pass in [first_panel] and
    [second_panel].

    [initial_size] decides the initial size of each panel. (default 50%).

    [separator_size_px] is the size of the separator that can be dragged to resize the
    panes (default 10px).

    [separator_color] is the background color to use for the separator as a
    [Css_gen.Color.t]. The default is gray (#eee).

    [on_container_resize] determines what will happen when the amount of space allocated
    to the pane changes (default [Keep_proportion]).

    [constraints] are limits on the amount of resizing that can be done (default: []).
    Constraints also take precendence over the initial size and resize behavior.

    {3 Dynamism}

    Most parameters changing will update the state of the split pane accordingly; new
    constraints will be applied, the pane will change from vertical and horizontal.

    However the component will not reset to the initial size when [initial_size] updates;
    use [inject_set_size] to trigger a resize instead. *)
val create
  :  ?initial_size:Panel_and_size.t Bonsai.t
  -> ?separator_size_px:int Bonsai.t
  -> ?separator_color:Css_gen.Color.t Bonsai.t
  -> ?on_container_resize:On_container_resize.t Bonsai.t
  -> ?constraints:Constraint.t list Bonsai.t
  -> ?panel_extra_attrs:Panel_extra_attrs.t Bonsai.t
  -> direction:Split_dir.t Bonsai.t
  -> first_panel:Vdom.Node.t Bonsai.t
  -> second_panel:Vdom.Node.t Bonsai.t
  -> unit
  -> Bonsai.graph
  -> t Bonsai.t

(** Renders the split pane as a vdom node. The split pane will expand to fill the size of
    its parent. *)
val to_vdom : t -> Vdom.Node.t

(** Request that the container changes to a certain size. The actual size applied may not
    be the requested one as constraints are applied to this new size.

    It will cause the separator to jump until the mouse moves again if set while the
    container is being resized. It will not work if fired before the container has been
    rendered; use the initial size parameter instead. *)
val inject_set_size : t -> Panel_and_size.t -> unit Effect.t

(** Returns the sizes of each panel in float pixels, if available. Callers can use this
    to, for example, position other elements based on the panel sizes.

    Returns [None] when the split pane hasn't been initialized *)
val panel_sizes : t -> Panel_sizes.t option

module For_testing : sig
  (** These are exported for the tests and example page; user code should not depend on
      anything here as it could change at any time *)

  module Parameters : sig
    type t =
      { direction : Split_dir.t
      ; initial_size : Panel_and_size.t
      ; separator_size_px : int
      ; separator_color : Css_gen.Color.t option
      ; on_container_resize : On_container_resize.t
      ; constraints : Constraint.t list
      }
    [@@deriving sexp, sexp_grammar, equal]

    val default : t
  end

  val create_from_parameters
    :  Parameters.t Bonsai.t
    -> first_panel:Vdom.Node.t Bonsai.t
    -> second_panel:Vdom.Node.t Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t

  module Container_dimensions : sig
    type t =
      { width : float
      ; height : float
      }
    [@@deriving sexp, equal, fields ~iterators:create] [@@fields.no_zero_alloc]

    val for_direction : t -> direction:Split_dir.t -> float
  end

  module Action : sig
    type t =
      | Set_size of Panel_and_size.t
      | Parameters_changed
      | Container_resized of Container_dimensions.t
      | Drag_start of
          { container_start : float
          ; separator_start : float
          ; mouse_pos : int
          }
      | Drag_cancelled
      | Drag_move of { mouse_pos : int }
      | Drag_end of { mouse_pos : int }
    [@@deriving sexp_of]
  end

  module State : sig
    type t [@@deriving sexp, equal]

    val first_panel_px : t -> float option
    val container_dimensions : t -> Container_dimensions.t option
  end

  val state_machine
    :  parameters:Parameters.t Bonsai.t
    -> Bonsai.graph
    -> State.t Bonsai.t * (Action.t -> unit Effect.t) Bonsai.t
end
