open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Table := Bonsai_web_ui_partial_render_table
open Bonsai_perf_shared

module Row : sig
  type t =
    { symbol : string
    ; edge : float
    ; max_edge : float
    ; bsize : int
    ; bid : float
    ; ask : float
    ; asize : int
    }

  include Comparator.S with type t := t

  val of_int : int -> t
  val init_rows : int -> t Int.Map.t
end

module type S = sig
  type column_id

  val first_column : column_id
  val all : (int, Row.t, column_id) Table.Expert.Columns.t
  val with_column_groups : (int, Row.t, column_id) Table.Expert.Columns.t
end

(** An [Action.t] represents the possible actions that can be performed on a partial render
    table. *)
module Action : sig
  type 'key t =
    | Unfocus
    | Focus_up
    | Focus_down
    | Focus_left
    | Focus_right
    | Page_up
    | Page_down
    | Focus_first_column of 'key
  [@@deriving sexp, equal]
end

module Prt_input : sig
  (** An [Input.t] packages up all of the inputs to the partial render table and provides
    facilities for modifying individual components. *)
  type ('key, 'data, 'cmp) t

  (** [create] produces a [t], with defaults for most components of the input.

      [filter] defaults to [None]
      [order] defaults to [Compare.Unchanged]
      [rank_range] defaults to [Which_range.To 100]
      [key_range] defaults to [Which_range.All_rows]
      [autosize] defaults to [false]
  *)
  val create
    :  ?filter:(key:'a -> data:'b -> bool) option
    -> ?order:('a, 'b, 'c) Incr_map_collate.Compare.t
    -> ?rank_range:int Incr_map_collate.Collate.Which_range.t
    -> ?key_range:'a Incr_map_collate.Collate.Which_range.t
    -> ?autosize:bool
    -> ('a, 'b, 'c) Base.Map.t
    -> ('a, 'b, 'c) t

  (** [apply_filter] produces an interaction to change the current filter. *)
  val apply_filter
    :  ('key, 'data, 'cmp) t Input.t
    -> (key:'key -> data:'data -> bool)
    -> 'action Interaction.t

  (** [clear_filter] produces an interaction to remove the current filter. *)
  val clear_filter : _ t Input.t -> 'action Interaction.t

  (** [update_map] produces an interaction to change the map whose data is being rendered in
    the table. *)
  val update_map
    :  ('key, 'data, 'cmp) t Input.t
    -> f:(('key, 'data, 'cmp) Map.t -> ('key, 'data, 'cmp) Map.t)
    -> 'action Interaction.t

  (** [set_order] produces an interaction to change the current ordering. *)
  val set_order
    :  ('key, 'data, 'cmp) t Input.t
    -> ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    -> 'action Interaction.t

  (** [set_rank_range] produces an interaction to change the currently visible rank range. *)
  val set_rank_range
    :  _ t Input.t
    -> int Incr_map_collate.Collate.Which_range.t
    -> 'action Interaction.t

  (** [scroll] generates an interaction with abs(start-stop) [change_input]s, which set the
    [rank_range]'s low end to the values between [start] (inclusive) and [stop]
    (exclusive), keeping [window_size] elements in the range. *)
  val scroll
    :  _ t Input.t
    -> start:int
    -> stop:int
    -> window_size:int
    -> 'action Interaction.t
end

module Config : sig
  type t =
    | Dynamic_cells of
        { use_state_in_cells : bool
        ; col_groups : bool
        }
    | Dynamic_cols of { col_groups : bool }
    | Dynamic_experimental of { use_state_in_cells : bool }
  [@@deriving equal, compare, sexp_of, enumerate, hash]

  include
    Config
    with type t := t
     and type input = (int, Row.t, Int.comparator_witness) Prt_input.t
     and type output = Vdom.Node.t * (int Action.t -> unit Effect.t)
     and type action = int Action.t

  val all_flat : t list
  val all_grouped : t list
end

val scenarios : (Config.input, Config.action) Scenario.t list
