open! Core
open! Bonsai_web

module type Comparator = sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end

type ('k, 'cmp) comparator =
  (module Comparator with type t = 'k and type comparator_witness = 'cmp)

(** A vertical list component which moves items into their proper place during
    drag and drop. Items use absolute positioning for explicit layout; that is,
    the nth item is [n * item_height] pixels from the top of the container.
    Items outside the list may be dragged into the list to extend it. *)
val list
  :  ('source, 'cmp) comparator
  -> dnd:('source, int) Bonsai_web_ui_drag_and_drop.t Bonsai.t
       (** The drag-and-drop universe the list should operate in; other items in the
      universe may be dragged into the list *)
  -> ?enable_debug_overlay:bool
       (** Display a transparent overlay on targets to make it clear where an item
      may be dropped. *)
  -> ?extra_item_attrs:Vdom.Attr.t Bonsai.t
       (** Extra attributes to put on the wrapper div for each item in the list. For
      example, you might want to make each item animate into and out of
      position. *)
  -> ?dragged_item_attrs:Vdom.Attr.t Bonsai.t
       (** Extra attributes to put on the wrapper div for the dragged item *)
  -> ?get_extra_item_attrs:('source -> int -> Vdom.Attr.t option) Bonsai.t
       (** Extra attributes to put on the wrapper div for a specific item in the list. 
      Allows fine-grain control over the attrs to add to items *)
  -> ?left:Css_gen.Length.t
       (** The space between the left edge of an item and the list container *)
  -> ?right:Css_gen.Length.t
       (** The space between the right edge of an item and the list container *)
  -> ?empty_list_placeholder:
       (item_is_hovered:bool Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
       (** What to display when there are no items in the list. [item_is_hovered] is
      provided in case you wish to change the placeholder based on whether an
      item is being hovered above the empty list.  *)
  -> ?default_item_height:int
       (** The items and drop targets are spaced evenly every item_height. In order
      to look natural, each item should have height [item_height]. *)
  -> ?add_drop_target_for_appending:bool
       (** If true, an additional [default_item_height] of space at the end of the list
      will be added. Items dragged into that space will be appended to the list.
      (Default: true) *)
  -> ('source, Vdom.Node.t * int, 'cmp) Map.t Bonsai.t
     (** The items that should be displayed in the list. Each item should have its
      view and its current rank. Updating the rank of an item must be done via
      the [on_drop] callback of the drag-and-drop universe. *)
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** Similar to [list], but creates the drag-and-drop universe and handles the
    [on_drop] event, making it fully self-contained. *)
val simple
  :  ('key, 'cmp) comparator
  -> ?sentinel_name:string
  -> ?enable_debug_overlay:bool
  -> ?extra_item_attrs:Vdom.Attr.t Bonsai.t
  -> ?left:Css_gen.Length.t
  -> ?right:Css_gen.Length.t
  -> ?empty_list_placeholder:
       (item_is_hovered:bool Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> ?default_item_height:int
  -> ?add_drop_target_for_appending:bool
  -> render:
       (index:int Bonsai.t
        -> source:Vdom.Attr.t Bonsai.t
        -> 'key Bonsai.t
        -> Bonsai.graph
        -> ('data * Vdom.Node.t) Bonsai.t)
  -> ('key, 'cmp) Set.t Bonsai.t
  -> Bonsai.graph
  -> (('key * 'data) list * Vdom.Node.t) Bonsai.t

module Action : sig
  type 'a item =
    | Move of 'a * int
    | Set of 'a
    | Remove of 'a
    | Overwrite of 'a list
  [@@deriving sexp]

  type 'a t = 'a item list [@@deriving sexp]
end

(** Similar to [simple], but exposes the components injection function. This is
    used by the [Bonsai_web_ui_form] wrapper of this library. *)
val with_inject
  :  ('key, 'cmp) comparator
  -> ?sentinel_name:string
  -> ?enable_debug_overlay:bool
  -> ?extra_item_attrs:Vdom.Attr.t Bonsai.t
  -> ?left:Css_gen.Length.t
  -> ?right:Css_gen.Length.t
  -> ?empty_list_placeholder:
       (item_is_hovered:bool Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> ?default_item_height:int
  -> ?add_drop_target_for_appending:bool
  -> (index:int Bonsai.t
      -> source:Vdom.Attr.t Bonsai.t
      -> 'key Bonsai.t
      -> Bonsai.graph
      -> ('data * Vdom.Node.t) Bonsai.t)
  -> Bonsai.graph
  -> (('key * 'data) list * Vdom.Node.t * ('key Action.t -> unit Effect.t)) Bonsai.t

module Multi : sig
  (** Similar to [simple] from the parent module, but for multiple lists, whose
      items can be moved between each other. The ['which] type parameter as an
      identifier for the different lists.

      The result is a map of all the list contents. Each list contains a
      list of items and rendered view for that list.
  *)
  val simple
    :  ('key, 'cmp) comparator
    -> ('which, 'which_cmp) comparator
    -> ?sentinel_name:string
    -> ?enable_debug_overlay:bool
    -> ?extra_item_attrs:Vdom.Attr.t Bonsai.t
    -> ?left:Css_gen.Length.t
    -> ?right:Css_gen.Length.t
    -> ?empty_list_placeholder:
         (item_is_hovered:bool Bonsai.t
          -> 'which Bonsai.t
          -> Bonsai.graph
          -> Vdom.Node.t Bonsai.t)
    -> ?default_item_height:int
    -> ?add_drop_target_for_appending:bool
    -> render:
         (index:int Bonsai.t
          -> source:Vdom.Attr.t Bonsai.t
          -> 'which Bonsai.t (** The current list an item is rendered inside. *)
          -> 'key Bonsai.t
          -> Bonsai.graph
          -> ('data * Vdom.Node.t) Bonsai.t)
    -> lists:('which, 'which_cmp) Set.t Bonsai.t
         (** The set of lists that items can be placed in. *)
    -> default_list:'which Bonsai.t
         (** Initially, all items are placed in [default_list]. *)
    -> ('key, 'cmp) Set.t Bonsai.t
    -> Bonsai.graph
    -> (('which, ('key * 'data) list * Vdom.Node.t, 'which_cmp) Map.t * Vdom.Node.t)
         Bonsai.t

  module Action : sig
    type ('key, 'which, 'which_cmp) item =
      | Move of 'key * 'which * int
      | Set of 'which * 'key
      | Remove of 'key
      | Overwrite of ('which, 'key list, 'which_cmp) Map.t

    type ('key, 'which, 'which_cmp) t = ('key, 'which, 'which_cmp) item list
  end

  val with_inject
    :  ('key, 'cmp) comparator
    -> ('which, 'which_cmp) comparator
    -> ?sentinel_name:string
    -> ?enable_debug_overlay:bool
    -> ?extra_item_attrs:Vdom.Attr.t Bonsai.t
    -> ?left:Css_gen.Length.t
    -> ?right:Css_gen.Length.t
    -> ?empty_list_placeholder:
         (item_is_hovered:bool Bonsai.t
          -> 'which Bonsai.t
          -> Bonsai.graph
          -> Vdom.Node.t Bonsai.t)
    -> ?default_item_height:int
    -> ?add_drop_target_for_appending:bool
    -> lists:('which, 'which_cmp) Set.t Bonsai.t
    -> (index:int Bonsai.t
        -> source:Vdom.Attr.t Bonsai.t
        -> 'which Bonsai.t
        -> 'key Bonsai.t
        -> Bonsai.graph
        -> ('data * Vdom.Node.t) Bonsai.t)
    -> Bonsai.graph
    -> (('which, ('key * 'data) list * Vdom.Node.t, 'which_cmp) Map.t
       * Vdom.Node.t
       * (('key, 'which, 'which_cmp) Action.t -> unit Effect.t))
         Bonsai.t
end
