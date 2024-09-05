open! Core
open! Bonsai_web
module Collated := Incr_map_collate.Collated

module By_cell : sig
  type ('k, 'col_id, 'presence) t

  val focused : ('k, 'col_id, 'presence) t -> 'presence
  val focus_is_locked : ('k, 'col_id, 'presence) t -> bool
  val lock_focus : ('k, 'col_id, 'presence) t -> unit Effect.t
  val unlock_focus : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_up : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_down : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_left : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_right : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_top : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_bottom : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_leftmost : ('k, 'col_id, 'presence) t -> unit Effect.t
  val focus_rightmost : ('k, 'col_id, 'presence) t -> unit Effect.t
  val page_up : ('k, 'col_id, 'presence) t -> unit Effect.t
  val page_down : ('k, 'col_id, 'presence) t -> unit Effect.t
  val unfocus : ('k, 'col_id, 'presence) t -> unit Effect.t

  (** [focus k] sets the focus to the `col_id cell in the row keyed by k. *)
  val focus : ('k, 'col_id, 'presence) t -> 'k -> 'col_id -> unit Effect.t

  (** [focus_index n] sets the focus to the `col_id cell in the nth row from the top of the
      entire table. The first row is 0, the second is 1, and so on. *)
  val focus_index : ('k, 'col_id, 'presence) t -> int -> 'col_id -> unit Effect.t

  type ('k, 'col_id) optional = ('k, 'col_id, ('k * 'col_id) option) t
end

module By_row : sig
  type ('k, 'presence) t

  val focused : ('k, 'presence) t -> 'presence
  val focus_is_locked : ('k, 'presence) t -> bool
  val lock_focus : ('k, 'presence) t -> unit Effect.t
  val unlock_focus : ('k, 'presence) t -> unit Effect.t
  val focus_up : ('k, 'presence) t -> unit Effect.t
  val focus_down : ('k, 'presence) t -> unit Effect.t
  val page_up : ('k, 'presence) t -> unit Effect.t
  val page_down : ('k, 'presence) t -> unit Effect.t
  val unfocus : ('k, 'presence) t -> unit Effect.t

  (** [focus k] sets the focus to the row keyed by k. *)
  val focus : ('k, 'presence) t -> 'k -> unit -> unit Effect.t

  (** [focus_index n] sets the focus to the nth row from the top of the
      entire table. The first row is 0, the second is 1, and so on. *)
  val focus_index : ('k, 'presence) t -> int -> unit -> unit Effect.t

  type 'k optional = ('k, 'k option) t

  module Expert : sig
    (** [keyless] disables selecting a row by key, or returning the keyed row.
        It can be useful for unifying controls for several tables with different keys.
        It is cursed. *)
    val keyless : 'a optional -> Nothing.t optional
  end
end

module Kind : sig
  type ('a, 'presence, 'k, 'col_id) t =
    | None : (unit, unit, 'k, 'col_id) t
    | By_row :
        { on_change : ('k option -> unit Effect.t) Bonsai.t
        ; compute_presence : 'k option Bonsai.t -> Bonsai.graph -> 'presence Bonsai.t
        ; key_rank : ('k -> int option Effect.t) Bonsai.t
        }
        -> (('k, 'presence) By_row.t, 'presence, 'k, 'col_id) t
    | By_cell :
        { on_change : (('k * 'col_id) option -> unit Effect.t) Bonsai.t
        ; compute_presence :
            ('k * 'col_id) option Bonsai.t -> Bonsai.graph -> 'presence Bonsai.t
        ; key_rank : ('k -> int option Effect.t) Bonsai.t
        }
        -> (('k, 'col_id, 'presence) By_cell.t, 'presence, 'k, 'col_id) t
end

type ('key, 'col_id, 'kind) focused =
  | Nothing_focused : ('key, 'col_id, _) focused
  | Cell_focused : ('key * 'col_id) -> ('key, 'col_id, _ By_cell.t) focused
  | Row_focused : 'key -> ('key, 'col_id, _ By_row.t) focused

type ('kind, 'key, 'col_id) t =
  { focus : 'kind
  ; visually_focused : ('key, 'col_id, 'kind) focused
  }

val component
  :  ('kind, 'presence, 'key, 'col_id) Kind.t
  -> ('key, 'cmp) Bonsai.comparator
  -> ('col_id, _) Bonsai.comparator
  -> collated:('key, 'data) Collated.t Bonsai.t
  -> leaves:'col_id Header_tree.leaf list Bonsai.t
  -> range:(int * int) Bonsai.t
  -> scroll_to_index:(int -> unit Effect.t) Bonsai.t
  -> scroll_to_column:('col_id -> unit Effect.t) Bonsai.t
  -> Bonsai.graph
  -> ('kind, 'key, 'col_id) t Bonsai.t

val get_on_cell_click
  :  ('r, _, 'key, 'column) Kind.t
  -> 'r Bonsai.t
  -> ('key -> 'column -> unit Effect.t) Bonsai.t
