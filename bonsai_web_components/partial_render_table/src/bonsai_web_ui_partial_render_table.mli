open! Core
open! Bonsai_web
module Order = Bonsai_web_ui_partial_render_table_protocol.Order
module Sort_state := Bonsai_web_ui_partial_render_table_protocol.Sort_state
module Sort_kind := Bonsai_web_ui_partial_render_table_protocol.Sort_kind

module For_testing : sig
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

module Focus_by_row = Focus.By_row
module Focus_by_cell = Focus.By_cell

module Indexed_column_id : sig
  type t [@@deriving equal, sexp]

  val of_int : int -> t
  val to_int : t -> int
end

module Basic : sig
  module Focus : sig
    module By_row = Focus.By_row
    module By_cell = Focus.By_cell

    type ('a, 'p, 'k, 'c) t =
      | None : (unit, unit, 'k, 'c) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Bonsai.t }
          -> ('k Focus_by_row.optional, 'k option, 'k, 'c) t
      | By_cell :
          { on_change : (('k * 'c) option -> unit Effect.t) Bonsai.t }
          -> (('k, 'c) By_cell.optional, ('k * 'c) option, 'k, 'c) t
  end

  module Result : sig
    type ('focus, 'column_id) t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; num_filtered_rows : int
      ; sortable_state : 'column_id Sortable.t
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      (** [set_column_width] cannot set the width of the column smaller than the minimum
          width of the header. *)
      }
    [@@deriving fields ~getters]
  end

  module Columns : sig
    (** There are a few ways to specify the columns on a partial render, table,
        and they each have their own tradeoffs and capibilities.  You can not
        mix-and-match column kinds.  Read the doc comments for each of the
        submodules to learn more.  *)
    module Indexed_column_id = Indexed_column_id

    type ('key, 'data, 'column_id) t
    type ('key, 'data, 'column_id) columns := ('key, 'data, 'column_id) t

    module Dynamic_experimental : sig
      module Sort_kind : sig
        type ('key, 'data) sort := 'key * 'data -> 'key * 'data -> int

        (** A [Sort_kind.t] consists of [forward] (ascending) and [reverse] (descending)
            sorting implementations. *)
        type ('key, 'data) t = ('key, 'data) Sort_kind.t =
          { forward : ('key, 'data) sort
          ; reverse : ('key, 'data) sort
          }

        (** Most sort functions are reversible, so you can create a [t] from a
            "forward" sort function via [reversible], or a "backward" sort function via
            [reversible']. *)
        val reversible : forward:('key, 'data) sort -> ('key, 'data) t

        val reversible' : reverse:('key, 'data) sort -> ('key, 'data) t
      end

      val build
        :  ?sorts:
             ('column_id Bonsai.t
              -> Bonsai.graph
              -> ('key, 'data) Sort_kind.t option Bonsai.t)
        -> ('column_id, _) Bonsai.comparator
        -> columns:'column_id list Bonsai.t
        -> render_header:
             ('column_id Bonsai.t
              -> Bonsai.graph
              -> (Sort_state.t -> Vdom.Node.t) Bonsai.t)
        -> render_cell:
             ('column_id Bonsai.t
              -> 'key Bonsai.t
              -> 'data Bonsai.t
              -> Bonsai.graph
              -> Vdom.Node.t Bonsai.t)
        -> ('key, 'data, 'column_id) columns

      (** [Sortable] provides types, state, and ui helper functions to sort your table
          data by one or more columns. *)
      module Sortable = Column.Dynamic_experimental.Sortable
    end

    module Dynamic_cells : sig
      (** Dynamic_cells is a column-specification format with the following
          tradeoffs:

          - Pro: Each cell is it's own bonsai computation, so you can stick complex
            components in side of them, like forms, or graphs.
          - Con: The set of columns must be statically known ahead of time, and can
            not be determined dynamically. *)

      type ('key, 'data) t

      val column
        :  ?sort:('key * 'data -> 'key * 'data -> int) Bonsai.t
             (** If this column is sortable, you can provide the sorting function here *)
        -> ?sort_reversed:('key * 'data -> 'key * 'data -> int) Bonsai.t
             (** If the column has a specialized "reverse order", you can provide it here. *)
        -> ?initial_width:Css_gen.Length.t
        -> ?visible:bool Bonsai.t
             (** [visible] can be set to [false] to hide the whole column. *)
        -> ?resizable:bool Bonsai.t
             (** [resizable] can be set to [false] disable resizing for this column. *)
        -> header:(Sort_state.t -> Vdom.Node.t) Bonsai.t
             (** [header] determines the contents of the column header. *)
        -> cell:
             (key:'key Bonsai.t
              -> data:'data Bonsai.t
              -> Bonsai.graph
              -> Vdom.Node.t Bonsai.t)
             (** [cell] is the function determines the contents of every cell in this column. *)
        -> unit
        -> ('key, 'data) t

      (** [group ~label children] builds a header-group that has [children] underneath it.
          The content of header-group is set to [label] *)
      val group : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t list -> ('key, 'data) t

      (** [expand ~label child] builds a header-group that has a single child underneath it. *)
      val expand : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t -> ('key, 'data) t

      (** [lift] pulls a list of columns out into a column specification for use in the primary APIs  *)
      val lift : ('key, 'data) t list -> ('key, 'data, Indexed_column_id.t) columns

      (** [Sortable] provides types, state, and ui helper functions to sort your table
          data by one or more columns. *)
      module Sortable = Column.Dynamic_cells_with_sorter.Sortable
    end

    module Dynamic_columns : sig
      (** Dynamic_columns is a column-specification format with the
          following tradeoffs:

          - Pro: The set of columns, and how to render them can be determined
            dynamically ([lift] takes a column list inside a Value.t)
          - Con: Cells are computed with plain functions, and can not maintain
            state. *)
      type ('key, 'data) t

      val column
        :  ?sort:('key * 'data -> 'key * 'data -> int)
             (** If this column is sortable, you can provide the sorting function here *)
        -> ?sort_reversed:('key * 'data -> 'key * 'data -> int)
             (** If the column has a specialized "reverse order", you can provide it here. *)
        -> ?initial_width:Css_gen.Length.t
        -> ?visible:bool (** [visible] can be set to [false] to hide the whole column. *)
        -> ?resizable:bool
             (** [resizable] can be set to [false] disable resizing for this column. *)
        -> header:(Sort_state.t -> Vdom.Node.t)
             (** [header] determines the contents of the column header. *)
        -> cell:(key:'key -> data:'data -> Vdom.Node.t)
             (** [cell] is the function determines the contents of every cell in this column. *)
        -> unit
        -> ('key, 'data) t

      (** [group ~label children] builds a header-group that has [children] underneath it.
          The content of header-group is set to [label] *)
      val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t

      (** [lift] pulls a list of columns out into a column specification for use in the primary APIs  *)
      val lift
        :  ('key, 'data) t list Bonsai.t
        -> ('key, 'data, Indexed_column_id.t) columns

      (** [Sortable] provides types, state, and ui helper functions to sort your table
          data by one or more columns. *)
      module Sortable = Column.Dynamic_columns_with_sorter.Sortable
    end
  end

  type 'a compare := 'a -> 'a -> int

  (** This is the main UI component for the table content. *)
  val component
    :  ?theming:Table_view.Theming.t
    -> ?autosize:bool Bonsai.t
         (** If [autosize] is [true], columns will autoresize to fit content. The
        [set_column_width] effect, and draggable resize UI, will only set min-width.

        If [false], columns can be set to any size, but will not autoresize. *)
    -> ?filter:(key:'key -> data:'data -> bool) Bonsai.t
         (** An optional function may be provided, which filters the rows in the table. *)
    -> ?override_sort:
         ('key compare -> ('key * 'data) compare -> ('key * 'data) compare) Bonsai.t
         (** override_sort is an optional function that transforms the tables current sort,
        taking into account the default-sort and any user-provided sorts that they've
        added by clicking on column headers.

        [override_sort] is also given the comparison function for the key of the table,
        which the overrider can use as a fall-back for when the the ('key * 'data)
        comparison function returns 0. *)
    -> ?default_sort:('key * 'data) compare Bonsai.t
         (** An optional function may be provided to sort the table. *)
    -> ?multisort_columns_when:
         [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click ] Bonsai.t
         (** When the combination in [multisort_columns_when] is used, new columns are added to
        the sort order instead of replacing the existing sort. Defaults to
        [`Shift_click]. *)
    -> ?preload_rows:int
    -> ?extra_row_attrs:('key -> Vdom.Attr.t list) Bonsai.t
         (** [extra_row_attrs] will be added to the themed/functional attrs attached by the PRT
        on each row. In general, theming of the PRT should be done by passing
        [~theming:`Themed] and customizing the PRT-related attributes in the theme.
        However, this parameter can be used to attach attributes for testing. *)
    -> ('key, 'cmp) Bonsai.comparator
    -> focus:('focus, 'presence, 'key, 'column_id) Focus.t
    -> row_height:[ `Px of int ] Bonsai.t
         (** [row_height] is the height of every row in the table. If the row height
        is specified to be 0px or less, we instead use 1px. *)
    -> columns:('key, 'data, 'column_id) Columns.t
    -> ('key, 'data, 'cmp) Map.t Bonsai.t (** The input data for the table *)
    -> Bonsai.graph
    -> ('focus, 'column_id) Result.t Bonsai.t
end

module Expert : sig
  (** In the [Basic] module, you pass the component all of the table data at once. In the
      [Expert] module, by contrast, you give it a "collation" -- that is, a filtered,
      sorted, range-restricted window -- of the full table. This can be useful when the
      table data is too large to pass to the client directly, or when you'd like to update
      your table via RPC. *)

  open Incr_map_collate

  module Focus : sig
    module By_row = Focus.By_row
    module By_cell = Focus.By_cell

    type ('a, 'p, 'k, 'c) t = ('a, 'p, 'k, 'c) Focus.Kind.t =
      | None : (unit, unit, 'k, 'c) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Bonsai.t
          (** Row-selection is not required to be inside the viewport, so the selected row
              can be offscreen such that it isn't given to the table component. [compute_presence]
              forces the user to consider if a row is considered 'focused' or not. *)
          ; compute_presence : 'k option Bonsai.t -> Bonsai.graph -> 'p Bonsai.t
          (** A user might try to focus-by-key a row that has not been filtered out,
              but is not inside the viewport. In that case, [key_rank] will be used as
              a fallback to compute the desired index.
              If the effect returns `None`, the key does not correspond to a row under the
              current filter conditions, and the focus will be a no-op. *)
          ; key_rank : ('k -> int option Effect.t) Bonsai.t
          }
          -> (('k, 'p) Focus_by_row.t, 'p, 'k, 'c) t
      | By_cell :
          { on_change : (('k * 'c) option -> unit Effect.t) Bonsai.t
          ; compute_presence :
              ('k * 'c) option Bonsai.t -> Bonsai.graph -> 'presence Bonsai.t
          ; key_rank : ('k -> int option Effect.t) Bonsai.t
          }
          -> (('k, 'c, 'presence) By_cell.t, 'presence, 'k, 'c) t
  end

  module Result : sig
    type ('focus, 'column_id) t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      (** [set_column_width] cannot set the width of the column smaller than the minimum
          width of the header. *)
      }
    [@@deriving fields ~getters]
  end

  module Columns : sig
    module Indexed_column_id = Indexed_column_id

    type ('key, 'data, 'column_id) t
    type ('key, 'data, 'column_id) columns := ('key, 'data, 'column_id) t

    module Dynamic_experimental : sig
      val build
        :  ('column_id, 'a) Bonsai.comparator
        -> columns:'column_id list Bonsai.t
        -> render_header:('column_id Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
        -> render_cell:
             ('column_id Bonsai.t
              -> 'key Bonsai.t
              -> 'data Bonsai.t
              -> Bonsai.graph
              -> Vdom.Node.t Bonsai.t)
        -> ('key, 'data, 'column_id) columns

      (** [Sortable] provides types, state, and ui helper functions to sort your table
          data by one or more columns. *)
      module Sortable = Column.Dynamic_experimental.Sortable
    end

    module Dynamic_cells : sig
      type ('key, 'data) t

      val column
        :  ?initial_width:Css_gen.Length.t
        -> ?visible:bool Bonsai.t
        -> ?resizable:bool Bonsai.t
        -> header:Vdom.Node.t Bonsai.t
        -> cell:
             (key:'key Bonsai.t
              -> data:'data Bonsai.t
              -> Bonsai.graph
              -> Vdom.Node.t Bonsai.t)
        -> unit
        -> ('key, 'data) t

      val group : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t list -> ('key, 'data) t
      val lift : ('key, 'data) t list -> ('key, 'data, Indexed_column_id.t) columns

      (** [Sortable] provides types, state, and ui helper functions to sort your table
          data by one or more columns. *)
      module Sortable = Column.Dynamic_cells.Sortable
    end

    module Dynamic_columns : sig
      type ('key, 'data) t

      val column
        :  ?initial_width:Css_gen.Length.t
        -> ?visible:bool
        -> ?resizable:bool
        -> header:Vdom.Node.t
        -> cell:(key:'key -> data:'data -> Vdom.Node.t)
        -> unit
        -> ('key, 'data) t

      val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t

      val lift
        :  ('key, 'data) t list Bonsai.t
        -> ('key, 'data, Indexed_column_id.t) columns

      (** [Sortable] provides types, state, and ui helper functions to sort your table
          data by one or more columns. *)
      module Sortable = Column.Dynamic_columns.Sortable
    end
  end

  val collate
    :  ?operation_order:[ `Filter_first | `Sort_first ]
    -> filter_equal:('filter -> 'filter -> bool)
         (** [filter_equal] is used to decide when the filters have actually changed, requiring
        a recomputation of the collation. *)
    -> order_equal:('order -> 'order -> bool)
         (** [order_equal] is used to decide when the sorting params have actually changed,
        requiring a recomputation of the collation. *)
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
         (** [filter_to_predicate] takes the current set of filters ['filter] and optionally
        returns a function that can apply those filters to each row. When
        [filter_to_predicate] returns [None], no filtering is done. *)
    -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
         (** [order_to_compare] takes the current set of sort params ['order] and uses the
        [Compare] specification to decide how to apply them. Return [Unchanged] to perform
        no sorting. *)
    -> ('k, 'v, 'cmp) Map.t Bonsai.t
       (** A [Map.t] containing the source for all the table data, pre-collation. *)
    -> ('k, 'filter, 'order) Collate.t Bonsai.t
       (** A [Collate.t] is a specification for how to perform collation: it's where the
        ['filter], ['order], and rank range are defined. *)
    -> Bonsai.graph
    -> (('k, 'v) Collated.t * ('k -> int option Effect.t)) Bonsai.t

  val component
    :  ?theming:Table_view.Theming.t
    -> ?autosize:bool Bonsai.t
         (** If [autosize] is [true], columns will autoresize to fit content. The
        [set_column_width] effect, and draggable resize UI, will only set min-width.

        If [false], columns can be set to any size, but will not autoresize. *)
    -> ?preload_rows:int
         (** [preload_rows] is the number of rows that are maintained before and after the
        viewport range. This number can have a significant effect on performance: too
        small and scrolling might be choppy; too large and you start to lose some of the
        benefits of partial rendering. *)
    -> ?extra_row_attrs:('key -> Vdom.Attr.t list) Bonsai.t
         (** [extra_row_attrs] will be added to the themed/functional attrs attached by the PRT
        on each row. In general, theming of the PRT should be done by passing
        [~theming:`Themed] and customizing the PRT-related attributes in the theme.
        However, this parameter can be used to attach attributes for testing. *)
    -> ('key, 'cmp) Bonsai.comparator
    -> focus:('focus, 'presence, 'key, 'column_id) Focus.t
    -> row_height:[ `Px of int ] Bonsai.t
         (** [row_height] is the height of every row in the table. If the row height
        is specified to be 0px or less, we instead use 1px. *)
    -> columns:('key, 'row, 'column_id) Columns.t
    -> ('key, 'row) Collated.t Bonsai.t
       (** The collated value is the proper input to the component.
        You can use [Expert.collate] to get a Collated.t value, or do
        the collation manually on the server by using the Incr_map_collate
        library manually. *)
    -> Bonsai.graph
    -> ('focus, 'column_id) Result.t Bonsai.t
end
