open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing : sig
  type cell =
    { cell_focused : bool
    ; view : Vdom.Node.t
    }

  type row =
    { id : Opaque_map.Key.t
    ; row_focused : bool
    ; cells : cell list
    }

  type t =
    { column_names : Vdom.Node.t list list (** See [Header_tree.column_names]. *)
    ; rows : row list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

val component
  :  themed_attrs:Table_view.Themed.t Bonsai.t
  -> autosize:bool Bonsai.t
  -> key_comparator:('key, 'cmp) Bonsai.comparator
  -> column_id_comparator:('column_id, 'column_id_cmp) Bonsai.comparator
  -> row_height:[< `Px of int ] Bonsai.t
  -> headers:'column_id Header_tree.t Bonsai.t
  -> leaves:'column_id Header_tree.leaf list Bonsai.t
  -> assoc:
       (('key * 'data) Opaque_map.t Bonsai.t
        -> Bonsai.graph
        -> ('key * ('column_id * Vdom.Node.t) list) Opaque_map.t Bonsai.t)
  -> column_widths:('column_id, Column_size.t, 'column_id_cmp) Map.t Bonsai.t
  -> visually_focused:('key, 'column_id, 'kind) Focus.focused Bonsai.t
  -> on_cell_click:('key -> 'column_id -> unit Effect.t) Bonsai.t
  -> extra_row_attrs:('key -> Vdom.Attr.t list) Bonsai.t
  -> ('key, 'data) Collated.t Bonsai.t
  -> ('key * 'data) Opaque_map.t Bonsai.t
  -> Bonsai.graph
  -> (Table_view.Body.t * For_testing.t Lazy.t) Bonsai.t
