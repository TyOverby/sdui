open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Incr_map_collate
open Bonsai_web_ui_partial_render_table_protocol
module Bbox = Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox
module Order = Order
module Sortable = Sortable
module Focus_by_row = Focus.By_row
module Focus_by_cell = Focus.By_cell
module Scroll = Bonsai_web_ui_scroll_utilities
module Indexed_column_id = Column.Indexed_column_id

module For_testing = struct
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

let default_preload = 70

module Expert = struct
  module Focus = struct
    include Focus
    include Kind
  end

  module Result = struct
    type ('focus, 'column_id) t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      }
    [@@deriving fields ~getters]
  end

  module Columns = struct
    module Indexed_column_id = Indexed_column_id

    type ('key, 'data, 'column) t = ('key, 'data, 'column) Column_intf.t

    module Dynamic_cells = Column.Dynamic_cells
    module Dynamic_columns = Column.Dynamic_columns
    module Dynamic_experimental = Column.Dynamic_experimental
  end

  module Row_height_model = struct
    type t = [ `Px of int ] [@@deriving sexp, equal]
  end

  let print_in_tests f =
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_benchmark | `Node | `Node_benchmark -> Effect.Ignore
    | `Node_test -> Effect.of_sync_fun (fun () -> print_endline (f ())) ()
  ;;

  let implementation
    (type column column_cmp key presence data cmp)
    ?extra_row_attrs
    ~autosize
    ~theming
    ~preload_rows
    (key_comparator : (key, cmp) Bonsai.comparator)
    (column_id_comparator : (column, column_cmp) Bonsai.comparator)
    ~(focus : (_, presence, key, column) Focus.Kind.t)
    ~row_height
    ~headers
    ~assoc
    (collated : (key, data) Collated.t Bonsai.t)
    graph
    =
    let extra_row_attrs =
      match extra_row_attrs with
      | None -> Bonsai.return (fun _ -> [])
      | Some extra_row_attrs -> extra_row_attrs
    in
    let theme = View.Theme.current graph in
    let themed_attrs =
      let%arr theme = theme
      and autosize = autosize in
      Table_view.Themed.create ~autosize theme () theming
    in
    let row_height =
      let%arr (`Px row_height) = row_height in
      `Px (Int.max 1 row_height)
    in
    let focus_kind = focus in
    let input_map = Bonsai.map ~f:Collated.to_opaque_map collated in
    let private_body_classname =
      let path = Bonsai.path_id graph in
      let%arr path = path in
      "partial-render-table-body-" ^ path
    in
    let table_body_selector =
      let%arr private_body_classname = private_body_classname in
      "." ^ private_body_classname
    in
    let header_client_rect, set_header_client_rect = Bonsai.state_opt graph in
    let header_client_rect =
      Bonsai.cutoff ~equal:[%equal: Bbox.t option] header_client_rect
    in
    let set_header_client_rect =
      let%arr set_header_client_rect = set_header_client_rect in
      fun b -> set_header_client_rect (Some b)
    in
    let table_body_visible_rect, set_table_body_visible_rect =
      Bonsai.state_opt ~equal:[%equal: Bbox.t] graph
    in
    let table_body_visible_rect =
      Bonsai.cutoff ~equal:[%equal: Bbox.t option] table_body_visible_rect
    in
    let table_body_client_rect, set_table_body_client_rect =
      Bonsai.state_opt graph ~equal:[%equal: Bbox.t]
    in
    let table_body_client_rect =
      Bonsai.cutoff ~equal:[%equal: Bbox.t option] table_body_client_rect
    in
    let module Column_cmp = (val column_id_comparator) in
    let module Column_widths_model = struct
      type t = Column_size.t Map.M(Column_cmp).t [@@deriving sexp_of, equal]
    end
    in
    let column_widths, set_column_width =
      Bonsai.state_machine0
        graph
        ~sexp_of_model:[%sexp_of: Column_widths_model.t]
        ~equal:[%equal: Column_widths_model.t]
        ~default_model:(Map.empty (module Column_cmp))
        ~apply_action:
          (fun
            (_ : _ Bonsai.Apply_action_context.t) model (column_id, `Px_float width) ->
          (* While checking for float equality is usually not a good idea,
               this is meant to handle the specific case when a column has
               "display:none", in which case the width will be exactly 0.0, so
               there is no concern about float rounding errors. *)
          Map.update model column_id ~f:(fun prev ->
            if Float.equal width 0.0
            then (
              match prev with
              | None -> Hidden { prev_width_px = None }
              | Some (Visible { width_px }) -> Hidden { prev_width_px = Some width_px }
              | Some (Hidden _ as prev) -> prev)
            else (
              let rounded = Float.round_decimal ~decimal_digits:2 width in
              Visible { width_px = rounded })))
    in
    let column_widths =
      Bonsai.cutoff ~equal:[%equal: Column_widths_model.t] column_widths
    in
    let set_column_width =
      let%arr set_column_width = set_column_width in
      fun ~column_id width -> set_column_width (column_id, width)
    in
    let row_count = collated >>| Collated.num_filtered_rows in
    let header_height_px =
      match%arr header_client_rect with
      | None -> 0.0
      | Some table_body_visible_rect -> Bbox.height table_body_visible_rect
    in
    let range_without_preload =
      (* The goal of this value is to track the index range of the rows that would be
         visible in the table if the table were full.  Usually this would be as easy as
         looking at the table_body_visible_rect, but we also account for the header occluding
         some of the first rows of the table.  For that, we need to look at the client-rects for
         the body and the header and subtract their overlap. *)
      let%arr table_body_visible_rect = table_body_visible_rect
      and table_body_client_rect = table_body_client_rect
      and header_client_rect =
        header_client_rect
        (* We need to know about autosize because the sticky header takes up 0 height in
           the table container, so it doesn't technically occlude anything initially. *)
      and autosize = autosize
      and header_height_px = header_height_px in
      fun (`Px row_height_px) ->
        let row_height_px = Float.of_int row_height_px in
        match table_body_visible_rect, table_body_client_rect, header_client_rect with
        | Some { min_y = body_min_y; max_y = body_max_y; _ }, _, None ->
          (* if we don't have the header-height yet, just assume that there's
             no overlap. *)
          let low = body_min_y /. row_height_px in
          let high = (body_max_y -. row_height_px +. 2.) /. row_height_px in
          Some (Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high)))
        | ( Some { min_y = body_min_y; max_y = body_max_y; _ }
          , Some { min_y = client_body_min_y; _ }
          , Some { max_y = header_max_y; _ } ) ->
          let low_offset, high_offset =
            let header_offset =
              Float.min header_height_px (header_max_y -. client_body_min_y)
            in
            match autosize with
            (* autosize:false shifts the top of the header down in position due to the
               attr that calculates visible client rect being on an element that only
               contains the header *)
            | false -> header_offset, 0.
            (* When autosize:true, the header is in the same container as the body. That
               container is where the visible client rect attr is attached, so the client
               rect also considers the header as part of what is visible. Due to this, we
               have to subtract the header height from the bottom of the rect, as we're
               reducing the visible body height and not its position.
            *)
            | true -> 0., header_offset *. -1.
          in
          let low = (body_min_y +. low_offset) /. row_height_px in
          let high =
            (body_max_y +. high_offset -. row_height_px +. 2.) /. row_height_px
          in
          Some (Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high)))
        | _ -> None
    in
    let midpoint_of_container =
      let%arr table_body_visible_rect = table_body_visible_rect in
      match table_body_visible_rect with
      | None -> 0.0, 0.0
      | Some rect -> (rect.max_x +. rect.min_x) /. 2.0, (rect.max_y -. rect.min_y) /. 2.0
    in
    let scroll_to_index =
      let%arr header_height_px = header_height_px
      and range_without_preload = range_without_preload
      and midpoint_of_container_x, _ = midpoint_of_container
      and table_body_selector = table_body_selector
      and autosize = autosize
      and (`Px row_height_px) = row_height in
      fun index ->
        let range_start, range_end =
          (* [range_without_preload] can be [None] if the number of rows in
             the table shrinks such that the table becomes invisible. By
             providing a small index, we ensure that the padding around the
             table does not force the page to remain large enough for the
             existing scroll position. With the padding removed, the browser
             should automatically reduce the range of the scrollbar, and
             possibly bringing the table back in view, which would quickly
             correct this value to something more useful. *)
          range_without_preload (`Px row_height_px) |> Option.value ~default:(0, 1)
        in
        let row_height_px = Float.of_int row_height_px in
        let to_top =
          let header_offset =
            match autosize with
            (* scrolling this row to the top of the display involves
             scrolling to a pixel that is actually [header_height] _above_
             the target row. *)
            (* autosize:false shifts the top of the header down in position due to the
               attr that calculates visible client rect being on an element that only
               contains the header *)
            | false -> header_height_px
            (* When autosize:true, the header is in the same container as the body. That
               container is where the visible client rect attr is attached, so the client
               rect also considers the header as part of what is visible. Due to this, we
               have to subtract the header height from the bottom of the rect, as we're
               reducing the visible body height and not its position.
            *)
            | true -> 0.
          in
          Some ((row_height_px *. Float.of_int index) -. header_offset)
        in
        let to_bottom =
          let header_offset =
            match autosize with
            (* In autosize:true, the header is in the same container as the body, so we
               need to account for the headers height in row offset calculations *)
            | true -> header_height_px
            | false -> 0.
          in
          (* scroll to the bottom of this row means scrolling to the top of
             a one-pixel element just below this row *)
          Some ((row_height_px *. Float.of_int (index + 1)) +. header_offset)
        in
        let y_px =
          if index <= range_start
          then to_top
          else if index >= range_end
          then to_bottom
          else None
        in
        match y_px with
        | Some y_px ->
          let%bind.Effect () =
            print_in_tests (fun () ->
              [%string "scrolling to index %{index#Int} at %{y_px#Float}0px"])
          in
          Scroll.to_position_inside_element
            ~x_px:midpoint_of_container_x
            ~y_px
            ~selector:table_body_selector
            `Minimal
          |> Effect.ignore_m
        | None ->
          print_in_tests (fun () -> "skipping scroll because target already in view")
    in
    let leaves = Bonsai.map ~f:Header_tree.leaves headers in
    let scroll_to_column =
      let width (column : Column_size.t) =
        match column with
        | Visible { width_px } -> width_px
        | Hidden { prev_width_px = _ } -> 0.0
      in
      let get_offset_and_width =
        let%arr column_widths = column_widths
        and leaves = leaves in
        fun column_id ->
          List.fold_until
            ~init:0.0
            leaves
            ~finish:(fun _ -> None)
            ~f:(fun offset leaf ->
              let column_width =
                Option.map (Map.find column_widths leaf.column_id) ~f:width
                |> Option.value ~default:0.0
              in
              match
                Comparable.equal Column_cmp.comparator.compare leaf.column_id column_id
              with
              | true -> Stop (Some (offset, column_width))
              | false -> Continue (offset +. column_width))
      in
      let%arr get_offset_and_width = get_offset_and_width
      and table_body_visible_rect = table_body_visible_rect
      and table_body_selector = table_body_selector
      and _, midpoint_of_container_y = midpoint_of_container in
      fun column_id ->
        match table_body_visible_rect with
        | None -> Effect.Ignore
        | Some rect ->
          let offset_and_width = get_offset_and_width column_id in
          (match offset_and_width with
           | None -> Effect.Ignore
           | Some (offset, width) ->
             let scroll_me offset =
               Scroll.to_position_inside_element
                 ~x_px:offset
                 ~y_px:midpoint_of_container_y
                 ~selector:table_body_selector
                 `Minimal
               |> Effect.ignore_m
             in
             let%bind.Effect () =
               print_in_tests (fun () ->
                 let column_id =
                   [%sexp (column_id : Column_cmp.t)] |> Sexp.to_string_hum
                 in
                 [%string "scrolling column with id %{column_id} into view, if necessary"])
             in
             if Float.( < ) offset rect.min_x
             then scroll_me offset
             else if Float.( > ) (offset +. width) rect.max_x
             then scroll_me (offset +. width)
             else Effect.Ignore)
    in
    let keep_top_row_in_position =
      let%arr range_without_preload = range_without_preload
      and header_height_px = header_height_px
      and midpoint_of_container_x, _ = midpoint_of_container
      and table_body_selector = table_body_selector
      and table_body_visible_rect = table_body_visible_rect in
      fun (`Px old_row_height_px) (`Px new_row_height_px) ->
        match range_without_preload (`Px old_row_height_px), table_body_visible_rect with
        | None, None | None, Some _ ->
          (* If there are no rows visible [range_without_preload] will return
             [None]. In this case we should do nothing because the scroll
             position is outside the tables jurisdiction. *)
          Effect.Ignore
        | Some _, None ->
          (* [range_without_preload] is expected to return a range only if the
             table body is partly visible, so this case is unexpected. We don't
             do anything except print because we need to visible rect to do
             anything correct. *)
          Effect.print_s
            [%message
              [%here]
                "BUG: the visible rect shouldn't be none when there is range of rows"]
        | Some (range_start, _range_end), Some table_body_visible_rect ->
          let old_row_height_px, new_row_height_px =
            Float.of_int old_row_height_px, Float.of_int new_row_height_px
          in
          (* If some rows of the table are visible, we scroll such that the top
             visible row remains in the same position in the viewport. *)
          let old_y_px = old_row_height_px *. Float.of_int range_start in
          let new_y_px = new_row_height_px *. Float.of_int range_start in
          let y_px =
            match Float.(new_y_px < old_y_px) with
            | true -> new_y_px -. header_height_px
            | false ->
              new_y_px
              -. header_height_px
              +. (table_body_visible_rect.max_y -. table_body_visible_rect.min_y)
              -. 1.0
          in
          let%bind.Effect () =
            print_in_tests (fun () ->
              [%string "scrolling position %{y_px#Float}px into view"])
          in
          Scroll.to_position_inside_element
            ~x_px:midpoint_of_container_x
            ~y_px
            ~selector:table_body_selector
            `Minimal
          |> Effect.ignore_m
    in
    let%sub () =
      (* If [row_height] changes, we want scrolling to follow the visible set
         of rows to their new location. To do this, we calculate the new
         position of the current top row, and then scroll their immediately. *)
      let callback =
        let%arr keep_top_row_in_position = keep_top_row_in_position in
        fun prev_row_height new_row_height ->
          match prev_row_height with
          | Some prev_row_height ->
            keep_top_row_in_position prev_row_height new_row_height
          | None -> Effect.Ignore
      in
      Bonsai.Edge.on_change'
        ~sexp_of_model:[%sexp_of: Row_height_model.t]
        ~equal:[%equal: Row_height_model.t]
        row_height
        ~callback
        graph;
      Bonsai.return ()
    in
    let range_without_preload =
      let prev_row_height =
        Bonsai.previous_value
          ~sexp_of_model:[%sexp_of: Row_height_model.t]
          ~equal:[%equal: Row_height_model.t]
          row_height
          graph
      in
      let%arr prev_row_height = prev_row_height
      and row_height = row_height
      and range_without_preload = range_without_preload in
      (* If the [row_height] just changed, then we will be scrolling to a new
         position that will leave [range_without_preload] unchanged. Thus, we
         should intentionally _not_ account for the new change in row_height
         yet, since otherwise we will get flickering in the UI. *)
      let range =
        match prev_row_height with
        | Some prev_row_height -> range_without_preload prev_row_height
        | None -> range_without_preload row_height
      in
      Option.value range ~default:(0, 1)
    in
    let%sub { focus; visually_focused } =
      Focus.component
        focus_kind
        key_comparator
        column_id_comparator
        ~leaves
        ~collated
        ~range:range_without_preload
        ~scroll_to_index
        ~scroll_to_column
        graph
    in
    let on_cell_click = Focus.get_on_cell_click focus_kind focus in
    let%sub body, body_for_testing =
      Table_body.component
        ~themed_attrs
        ~autosize
        ~key_comparator
        ~column_id_comparator
        ~row_height
        ~headers
        ~leaves
        ~assoc
        ~column_widths
        ~visually_focused
        ~on_cell_click
        ~extra_row_attrs
        collated
        input_map
        graph
    in
    let head =
      Table_header.component
        headers
        ~themed_attrs
        ~autosize
        ~column_widths
        ~set_column_width
        ~set_header_client_rect
        graph
    in
    let view =
      let vis_change_attr =
        let%arr set_table_body_visible_rect = set_table_body_visible_rect
        and set_table_body_client_rect = set_table_body_client_rect in
        Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
          ()
          ~client_rect_changed:(fun bounds -> set_table_body_client_rect (Some bounds))
          ~visible_rect_changed:(fun visible_bounds ->
            set_table_body_visible_rect visible_bounds)
      in
      let total_height =
        let%arr row_count = row_count
        and (`Px row_height_px) = row_height in
        row_count * row_height_px
      in
      let%arr head = head
      and body = body
      and private_body_classname = private_body_classname
      and vis_change_attr = vis_change_attr
      and total_height = total_height
      and autosize = autosize
      and themed_attrs = themed_attrs in
      Table_view.Table.view
        themed_attrs
        ~private_body_classname
        ~vis_change_attr
        ~total_height
        ~autosize
        head
        body
    in
    let range =
      let%arr low, high = range_without_preload
      and row_count = row_count in
      let low = Int.max 0 (low - preload_rows) in
      let low =
        (* always fetch a range starting at an even index in order to make
           css-selecting on even and odd rows work. *)
        low - (low % 2)
      in
      let high = Int.min row_count (high + preload_rows) in
      let low, high = low, Int.max low high in
      low, high
    in
    let%arr view = view
    and range = range
    and body_for_testing = body_for_testing
    and focus = focus
    and set_column_width = set_column_width in
    let for_testing =
      let%map.Lazy body = body_for_testing in
      { For_testing.body }
    in
    { Result.view; range; for_testing; focus; set_column_width }
  ;;

  let component
    (type key focus presence data cmp column_id)
    ?(theming = `Themed)
    ?(autosize = Bonsai.return false)
    ?(preload_rows = default_preload)
    ?extra_row_attrs
    (key_comparator : (key, cmp) Bonsai.comparator)
    ~(focus : (focus, presence, key, column_id) Focus.Kind.t)
    ~row_height
    ~(columns : (key, data, column_id) Column_intf.t)
    (collated : (key, data) Collated.t Bonsai.t)
    graph
    =
    let (T { value; vtable; column_id }) = columns in
    let module T = (val vtable) in
    let headers = T.headers value graph in
    let assoc cells = T.instantiate_cells value key_comparator cells in
    implementation
      ?extra_row_attrs
      ~autosize
      ~preload_rows
      ~theming
      key_comparator
      column_id
      ~focus
      ~row_height
      ~headers
      ~assoc
      collated
      graph
  ;;

  let collate
    (type k v cmp filter order)
    ?operation_order
    ~filter_equal
    ~order_equal
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    (data : (k, v, cmp) Map.t Bonsai.t)
    (collate : (k, filter, order) Collate.t Bonsai.t)
    =
    let data_and_collate = Bonsai.both data collate in
    Bonsai.Incr.compute data_and_collate ~f:(fun data_and_collate ->
      let open Ui_incr.Let_syntax in
      let%pattern_bind data, collate = data_and_collate in
      Incr_map_collate.collate
        ?operation_order
        ~filter_equal
        ~order_equal
        ~filter_to_predicate
        ~order_to_compare
        data
        collate
      |> fun x ->
      let key_rank =
        let%map.Incremental key_rank = Incr_map_collate.key_rank x in
        Effect.of_sync_fun key_rank
      in
      Incremental.both (Incr_map_collate.collated x) key_rank)
  ;;
end

module Basic = struct
  module Focus = struct
    include Focus

    type ('a, 'p, 'k, 'c) t =
      | None : (unit, unit, 'k, 'c) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Bonsai.t }
          -> ('k Focus_by_row.optional, 'k option, 'k, 'c) t
      | By_cell :
          { on_change : (('k * 'c) option -> unit Effect.t) Bonsai.t }
          -> (('k, 'c) By_cell.optional, ('k * 'c) option, 'k, 'c) t
  end

  module Result = struct
    type ('focus, 'column_id) t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; num_filtered_rows : int
      ; sortable_state : 'column_id Sortable.t
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      }
    [@@deriving fields ~getters]
  end

  module Columns = struct
    module Indexed_column_id = Indexed_column_id

    type ('key, 'data, 'column_id) t = ('key, 'data, 'column_id) Column_intf.with_sorter

    module Dynamic_cells = Column.Dynamic_cells_with_sorter
    module Dynamic_columns = Column.Dynamic_columns_with_sorter
    module Dynamic_experimental = Column.Dynamic_experimental_with_sorter
  end

  module Rank_range = struct
    type t = int Collate.Which_range.t [@@deriving sexp, equal]
  end

  type 'a compare = 'a -> 'a -> int

  let component
    : type key presence focus data cmp column_id.
      ?theming:Table_view.Theming.t
      -> ?autosize:bool Bonsai.t
      -> ?filter:(key:key -> data:data -> bool) Bonsai.t
      -> ?override_sort:
           (key compare -> (key * data) compare -> (key * data) compare) Bonsai.t
      -> ?default_sort:(key * data) compare Bonsai.t
      -> ?multisort_columns_when:
           [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click ] Bonsai.t
      -> ?preload_rows:int
      -> ?extra_row_attrs:(key -> Vdom.Attr.t list) Bonsai.t
      -> (key, cmp) Bonsai.comparator
      -> focus:(focus, presence, key, column_id) Focus.t
      -> row_height:[ `Px of int ] Bonsai.t
      -> columns:(key, data, column_id) Column_intf.with_sorter
      -> (key, data, cmp) Map.t Bonsai.t
      -> Bonsai.graph
      -> (focus, column_id) Result.t Bonsai.t
    =
    fun ?(theming = `Themed)
      ?(autosize = Bonsai.return false)
      ?filter
      ?override_sort
      ?default_sort
      ?multisort_columns_when
      ?(preload_rows = default_preload)
      ?extra_row_attrs
      key_comparator
      ~focus
      ~row_height
      ~columns
      map
      graph ->
    let module Key_cmp = (val key_comparator) in
    let filter = Bonsai.transpose_opt filter in
    let rank_range, set_rank_range =
      Bonsai.state
        (Collate.Which_range.To 0)
        ~sexp_of_model:[%sexp_of: Rank_range.t]
        ~equal:[%equal: Rank_range.t]
        graph
    in
    let (Y { value; vtable; column_id }) = columns in
    let module Col_id = (val column_id) in
    let sortable_state =
      Sortable.state ~equal:(Comparable.equal Col_id.comparator.compare) () graph
    in
    let module Column = (val vtable) in
    let assoc cells = Column.instantiate_cells value key_comparator cells in
    let default_sort =
      match default_sort with
      | None -> Bonsai.return None
      | Some v -> v >>| Option.some
    in
    let multisort_columns_when =
      Option.value multisort_columns_when ~default:(Bonsai.return `Shift_click)
    in
    let%sub sorters, headers =
      Column.headers_and_sorters ~multisort_columns_when value sortable_state graph
    in
    let collate =
      let override_sort =
        match override_sort with
        | None -> Bonsai.return None
        | Some override -> override >>| Option.some
      in
      let order =
        let%arr sorters = sorters
        and default_sort = default_sort
        and sortable_state = sortable_state
        and override_sort = override_sort in
        let override_sort =
          Option.map override_sort ~f:(fun override_sort ->
            override_sort Key_cmp.comparator.compare)
        in
        Order.to_compare
          (Sortable.order sortable_state)
          ?override_sort
          ~sorters
          ~default_sort
      in
      let%arr filter = filter
      and order = order
      and rank_range = rank_range in
      let key_range = Collate.Which_range.All_rows in
      { Collate.filter; order; key_range; rank_range }
    in
    let%sub collated, key_rank =
      Expert.collate
        ~filter_equal:phys_equal
        ~filter_to_predicate:Fn.id
        ~order_equal:phys_equal
        ~order_to_compare:Fn.id
        map
        collate
        graph
    in
    let focus : (focus, presence, key, column_id) Expert.Focus.Kind.t =
      match focus with
      | None -> None
      | By_row { on_change } ->
        let compute_presence focus _graph =
          let%arr focus = focus
          and map = map in
          match focus with
          | None -> None
          | Some focus -> if Map.mem map focus then Some focus else None
        in
        By_row { on_change; compute_presence; key_rank }
      | By_cell { on_change } ->
        let compute_presence focus _graph =
          let%arr focus = focus
          and map = map in
          match focus with
          | None -> None
          | Some ((focused_key, _) as focus) ->
            if Map.mem map focused_key then Some focus else None
        in
        By_cell { on_change; compute_presence; key_rank }
    in
    let num_filtered_rows =
      let%arr collated = collated in
      Collated.num_filtered_rows collated
    in
    let%sub ({ range = viewed_range; _ } as result) =
      Expert.implementation
        ?extra_row_attrs
        ~preload_rows
        ~theming
        ~autosize
        key_comparator
        column_id
        ~focus
        ~row_height
        ~headers
        ~assoc
        collated
        graph
    in
    let () =
      Bonsai.Edge.on_change
        ~sexp_of_model:[%sexp_of: int * int]
        ~equal:[%equal: int * int]
        viewed_range
        ~callback:
          (let%map set_rank_range = set_rank_range in
           fun (low, high) -> set_rank_range (Collate.Which_range.Between (low, high)))
        graph
    in
    let%arr { view; for_testing; range = _; focus; set_column_width } = result
    and num_filtered_rows = num_filtered_rows
    and sortable_state = sortable_state in
    { Result.view
    ; for_testing
    ; focus
    ; num_filtered_rows
    ; sortable_state
    ; set_column_width
    }
  ;;
end
