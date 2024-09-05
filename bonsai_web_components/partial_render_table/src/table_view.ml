open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Theming = struct
  type t =
    [ `Legacy_don't_use_theme
    | `Themed
    ]
end

module Themed = struct
  type t =
    { header_cell : Vdom.Attr.t
    ; header_row : Vdom.Attr.t
    ; header : Vdom.Attr.t
    ; autosize_table_bottom_border_element : Vdom.Attr.t
    ; autosize_table_cell_wrapper : Vdom.Attr.t
    ; cell : Vdom.Attr.t
    ; cell_focused : Vdom.Attr.t
    ; row : Vdom.Attr.t
    ; row_focused : Vdom.Attr.t
    ; body : Vdom.Attr.t
    ; table : Vdom.Attr.t
    }

  module Legacy_style =
    [%css
    stylesheet
      {|
        .header_cell {
          text-align: center;
          font-weight: bold;
        }
        |}]

  module Prt_view = Bonsai_web_ui_view.For_components.Prt

  let create ?autosize theme () = function
    | `Legacy_don't_use_theme ->
      { header_cell = Legacy_style.header_cell
      ; header_row = Vdom.Attr.empty
      ; header = Vdom.Attr.class_ "prt-table-header"
      ; autosize_table_cell_wrapper = Vdom.Attr.empty
      ; autosize_table_bottom_border_element = Vdom.Attr.empty
      ; cell = Vdom.Attr.class_ "prt-table-cell"
      ; cell_focused = Vdom.Attr.class_ "prt-table-cell-selected"
      ; row = Vdom.Attr.class_ "prt-table-row"
      ; row_focused = Vdom.Attr.class_ "prt-table-row-selected"
      ; body = Vdom.Attr.empty
      ; table = Vdom.Attr.empty
      }
    | `Themed ->
      let styling = Prt_view.styling ?autosize theme () in
      { header_cell = styling.header_cell
      ; header_row = styling.header_row
      ; header = styling.header
      ; autosize_table_cell_wrapper = styling.autosize_table_cell_wrapper
      ; autosize_table_bottom_border_element =
          styling.autosize_table_bottom_border_element
      ; cell = styling.cell
      ; cell_focused = styling.cell_focused
      ; row = styling.row
      ; row_focused = styling.row_focused
      ; body = styling.body
      ; table = styling.table
      }
  ;;
end

(* These styles make the table functional and interactive;
   they are applied regardless of theme. *)
module Functional_style =
  [%css
  stylesheet
    {|
      .partial_render_table_container * {
        box-sizing: border-box;
      }

      /* The default value for the [overflow-anchor] CSS property is [auto], which
         permits the browser to scroll the page in order to minimize content shifts.
         This interacts poorly with the PRT because our virtual-dom diff-and-patch
         algorithm often removes and re-inserts elements. To fix this, we disable
         overflow-anchor for all elements that contain a partial render table. */
      :has(.partial_render_table_container) {
        overflow-anchor: none;
      }

      .partial_render_table_container {
        width: max-content;
        position: relative;
      }

      .default_partial_render_table_body {
        position: relative;
      }

      .sortable_header_cell {
        white-space: pre;
        cursor: pointer;
      }

      .header_label {
        user-select: none;
      }

      .leaf_header {
        overflow: hidden;
      }

      .leaf_header_resizable {
        resize: horizontal;
        padding-right: 10px; /* Space for the resizer */
      }

      .partial_render_table_header {
        position: sticky;
        top: 0px;
        z-index: 99;
      }

      .row {
        contain: strict;
      }

      .cell {
        overflow: hidden;
        display: inline-block;
        contain: strict;
      }
      |}]

(* This function takes a vdom node and if it's an element, it adds extra attrs, classes, key,
   and style info to it, but if it's not an element, it wraps that node in a div that has those
   attributes.  This can be useful if you get a vdom node from the
   user of this API, and want to avoid excessive node wrapping. *)
let set_or_wrap ~attrs =
  let open Vdom.Node in
  function
  | Element e -> Element (Element.map_attrs e ~f:(fun a -> Vdom.Attr.(a @ many attrs)))
  | other -> div ~attrs [ other ]
;;

let int_to_px_string px = Int.to_string px ^ "px"
let float_to_px_string px = Virtual_dom.Dom_float.to_string_fixed 2 px ^ "px"

module Header_label = struct
  let wrap_clickable ~sortable ~handle_click contents =
    let attrs =
      if sortable
      then
        [ Functional_style.sortable_header_cell
        ; handle_click
        ; Vdom.Attr.role "button"
        ; Vdom.Attr.tabindex 0
        ]
      else []
    in
    Vdom.Node.div ~attrs [ contents ]
  ;;

  (* As an externally exposed component with no prior style overrides,
     we don't allow opting out of theming to keep user code simpler. *)
  let wrap_with_icon
    ?(sort_indicator_attrs = [])
    (label : Vdom.Node.t)
    (sort_state : Bonsai_web_ui_partial_render_table_protocol.Sort_state.t)
    =
    match sort_state with
    | Not_sortable -> Vdom.Node.div [ Vdom.Node.span [ label ] ]
    | _ ->
      let get_arrow = function
        | `Asc -> "▲"
        | `Desc -> "▼"
      in
      let sort_indicator =
        let%map.Option indicator =
          match sort_state with
          | Not_sortable | Not_sorted -> None
          | Single_sort dir -> Some (get_arrow dir)
          | Multi_sort { dir; index } -> Some [%string "%{get_arrow dir} %{index#Int}"]
        in
        Vdom.Node.span ~attrs:sort_indicator_attrs [ Vdom.Node.text indicator ]
      in
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.style
              (Css_gen.flex_container ~column_gap:(`Px 6) ~align_items:`Baseline ())
          ]
        [ Vdom.Node.span [ label ]
        ; sort_indicator
          |> Option.value ~default:(Vdom.Node.none_deprecated [@alert "-deprecated"])
        ]
  ;;
end

module Header = struct
  let attr_colspan i =
    match i with
    | 0 -> Vdom.Attr.style (Css_gen.display `None)
    | 1 -> Vdom.Attr.empty
    | i -> Vdom.Attr.create_float "colspan" (Int.to_float i)
  ;;

  module Header_cell = struct
    type t = Vdom.Node.t

    let leaf_view
      (themed_attrs : Themed.t)
      ~column_width
      ~set_column_width
      ~visible
      ~resizable
      ~label
      ~autosize
      ()
      =
      let on_change_tracker =
        match autosize with
        | false ->
          Bonsai_web_ui_element_size_hooks.Size_tracker.on_change (fun ~width ~height:_ ->
            set_column_width (`Px_float width))
        | true ->
          (* This is just here for compatibility with tests. The test expects to find an
             element with the sizetracker attribute  *)
          Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
            (fun ~width:_ ~height:_ -> Effect.Ignore)
      in
      let node = if autosize then Vdom.Node.th else Vdom.Node.td in
      node
        ~attrs:
          [ themed_attrs.header_cell
          ; on_change_tracker
          ; Vdom.Attr.colspan 1
          ; Functional_style.header_label
          ; Functional_style.leaf_header
          ; (if resizable then Functional_style.leaf_header_resizable else Vdom.Attr.empty)
          ; Vdom.Attr.style
              Css_gen.(width column_width @> if visible then empty else display `None)
          ]
        [ label ]
    ;;

    let spacer_view (themed_attrs : Themed.t) ~colspan ~autosize () =
      let node = if autosize then Vdom.Node.th else Vdom.Node.td in
      node ~attrs:[ themed_attrs.header_cell; attr_colspan colspan ] []
    ;;

    let group_view (themed_attrs : Themed.t) ~colspan ~autosize ~label () =
      let node = if autosize then Vdom.Node.th else Vdom.Node.td in
      node
        ~attrs:
          [ themed_attrs.header_cell
          ; attr_colspan colspan
          ; Functional_style.header_label
          ]
        [ label ]
    ;;
  end

  module Header_row = struct
    type t = Vdom.Node.t

    let view (themed_attrs : Themed.t) contents =
      Vdom.Node.tr ~attrs:[ themed_attrs.header_row ] contents
    ;;
  end

  type t = Vdom.Node.t

  let view_impl (themed_attrs : Themed.t) ~set_header_client_rect ~autosize header_rows =
    let attrs =
      [ themed_attrs.header
      ; Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
          ()
          ~client_rect_changed:set_header_client_rect
      ; Functional_style.partial_render_table_header
      ]
    in
    match autosize with
    | false -> Vdom.Node.table ~attrs [ Vdom.Node.tbody header_rows ]
    | true -> Vdom.Node.thead ~attrs header_rows
  ;;

  (* Fun fact: the header is the only part of partial_render_table that is displayed
     as an actual HTML table!.... unless you're using the table_view *)
  let view (themed_attrs : Themed.t) ~set_header_client_rect ~autosize header_rows =
    view_impl themed_attrs ~set_header_client_rect ~autosize header_rows
  ;;
end

module Cell = struct
  module Col_styles = struct
    type t = Vdom.Attr.t list

    (* Css_gen is really slow, so we need to re-use the results of all these functions
       whenever possible.  The difference between non-cached and cached css is the
       difference between 200ms stabilizations and 0.2ms stabiliations while scrolling.

       The reason that Css_gen is so slow is because apparently "sprintf" is _really_
       slow. *)
    let create
      (type column_id cmp)
      (module Col_cmp : Bonsai.Comparator
        with type t = column_id
         and type comparator_witness = cmp)
      ~(themed_attrs : Themed.t)
      ~autosize
      ~row_height
      ~(col_widths : (column_id, Column_size.t, cmp) Map.t)
      ~(leaves : column_id Header_tree.leaf list)
      =
      let height_styles =
        let h = int_to_px_string row_height in
        Css_gen.(
          create ~field:"height" ~value:h
          @> create ~field:"min-height" ~value:h
          @> create ~field:"max-height" ~value:h)
      in
      let styles_by_column =
        List.map
          leaves
          ~f:
            (fun
              { visible = is_visible
              ; column_id
              ; leaf_header = _
              ; initial_width = _
              ; resizable = _
              }
            ->
            let width_styles =
              match autosize with
              | false ->
                (* We use the previous width even when hidden, so that the rendering engine has
                   less work to do if re-adding a column. Columns that are not currently visible
                   are hidden via `display: None`. *)
                let w =
                  match Map.find col_widths column_id with
                  | None | Some (Hidden { prev_width_px = None }) -> "0.00px"
                  | Some (Hidden { prev_width_px = Some width })
                  | Some (Visible { width_px = width }) -> float_to_px_string width
                in
                Css_gen.(
                  create ~field:"width" ~value:w
                  @> create ~field:"min-width" ~value:w
                  @> create ~field:"max-width" ~value:w)
              | true -> Css_gen.empty
            in
            let visible_styles =
              match is_visible with
              | false -> Css_gen.display `None
              | true -> Css_gen.empty
            in
            ( column_id
            , [ Vdom.Attr.style Css_gen.(height_styles @> width_styles @> visible_styles)
              ; themed_attrs.cell
              ] ))
        |> Map.of_alist_exn (module Col_cmp)
      in
      Staged.stage (fun column -> Map.find_exn styles_by_column column)
    ;;
  end

  type t = Vdom.Node.t

  let view
    (themed_attrs : Themed.t)
    ~is_focused
    ~col_styles
    ~on_cell_click
    ~autosize
    content
    =
    let focused_attr =
      if is_focused then themed_attrs.cell_focused else Vdom.Attr.empty
    in
    let shared_attrs =
      Vdom.Attr.on_click (fun _ -> on_cell_click) :: focused_attr :: col_styles
    in
    match autosize with
    | false -> set_or_wrap content ~attrs:(Functional_style.cell :: shared_attrs)
    | true ->
      (* In order for the table cell to have a constrained height, we have to wrap the
         content in a div. This is because table cells (div with dislay:table-cell OR td)
         treat the height value as a min-height value, and seem to just ignore max-height

         This wrapping does incur a performance hit. Using chrome profiler,
         animation frame times were:

         With wrapped div: 19-22ms on average, with spikes to 30-32ms and huge (rarer)
         spikes to ~45ms

         Without wrapped div: 14-16 on average, with spikes to 20ms, and rare spikes to
         30ms

         Unfortunately, there's no way of constraining height aside from using this
         wrapping div.

         In the PRT example bonsai, we're diffing ~200 additional elements, but it's
         surprising that that costs so much more time
      *)
      Vdom.Node.div
        ~attrs:[ themed_attrs.autosize_table_cell_wrapper ]
        [ set_or_wrap ~attrs:shared_attrs content ]
  ;;
end

module Row = struct
  module Styles = struct
    type t = Css_gen.t

    let create ~row_height ~row_width ~autosize =
      let h = int_to_px_string row_height in
      let w = float_to_px_string row_width in
      let open Css_gen in
      create ~field:"height" ~value:h
      @>
      match autosize with
      | false -> create ~field:"width" ~value:w @> flex_container ()
      | true -> Css_gen.empty
    ;;
  end

  type t = Vdom.Node.t

  let view (themed_attrs : Themed.t) ~styles ~is_focused ~extra_attrs ~autosize cells =
    let focused_attr = if is_focused then themed_attrs.row_focused else Vdom.Attr.empty in
    let display_style =
      if autosize
      then Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"table-row")
      else Vdom.Attr.empty
    in
    Vdom.Node.lazy_
      (lazy
        (Vdom.Node.div
           ~attrs:
             (themed_attrs.row
              :: Vdom.Attr.style styles
              :: focused_attr
              :: Functional_style.row
              :: display_style
              :: extra_attrs)
           cells))
  ;;
end

module Body = struct
  type t = Vdom.Node.t

  module Body_row_key = struct
    module T = struct
      type t =
        | Top_padding
        | Row of Opaque_map.Key.t
        | Bottom_border
        | Bottom_padding
      [@@deriving compare, sexp, equal]
    end

    include Comparable.Make (T)
    include T
  end

  let view_impl (themed_attrs : Themed.t) ~padding_top ~padding_bottom ~rows =
    let style =
      Vdom.Attr.style
        (Css_gen.concat
           [ Css_gen.padding_top (`Px padding_top)
           ; Css_gen.padding_bottom (`Px padding_bottom)
           ])
    in
    Vdom.Node.div
      ~attrs:[ themed_attrs.body; style ]
      [ Vdom.Node.Map_children.make ~tag:"div" rows ]
  ;;

  let table_view_impl (themed_attrs : Themed.t) ~padding_top ~padding_bottom ~rows =
    let styles =
      Vdom.Attr.style
        (Css_gen.concat
           [ Css_gen.create ~field:"display" ~value:"table-row-group"
             (* This field is required so that the bottom border element can be
                  positioned properly *)
           ; Css_gen.create ~field:"position" ~value:"relative"
           ])
    in
    (* A bit of a silly hack required due to slow CSS selectors. Using an absolutely
       positioned element in order to display the bottom border as we cannot use
       :last-child to select the last row anymore due to the padding elements technically
       being the first and last chlidren.

       We could use :last-of-type by changing the padding elements to <tr> elements, but
       that selector is extremely slow (adds ~8-10ms of recalculate styles per frame).

       We are also using the same nested structure of row and cell so that we can take
       advantage of the CSS selectors that are already used for the row/cell to draw
       the proper border color for the element after the focused row.

       Seems like there is no real performance hit using this method vs using CSS
       selectors.
    *)
    let bottom_border_element =
      Vdom.Node.div
        ~key:"bottom_border"
        ~attrs:[ themed_attrs.row ]
        [ Vdom.Node.div
            ~attrs:
              [ themed_attrs.autosize_table_bottom_border_element; themed_attrs.cell ]
            []
        ]
    in
    (* Initially this was done with ppx_css and css pseudoelements, but something about
       adding the attr to the top-level div and changing the cssvars made things a lot
       slower. Could possibly be due to how the diffing is done for the top-level element,
       or may be something to do with the browser implementation of updating that var, but
       using child elements is significantly faster as of now.
    *)
    let rows =
      Map.set
        rows
        ~key:Body_row_key.Top_padding
        ~data:
          (Vdom.Node.div
             ~key:"top_padding"
             ~attrs:[ Vdom.Attr.style (Css_gen.height (`Px padding_top)) ]
             [])
      |> Map.set
           ~key:Body_row_key.Bottom_padding
           ~data:
             (Vdom.Node.div
                ~key:"bottom_padding"
                ~attrs:[ Vdom.Attr.style (Css_gen.height (`Px padding_bottom)) ]
                [])
      |> Map.set ~key:Body_row_key.Bottom_border ~data:bottom_border_element
    in
    Vdom.Node.Map_children.make
      ~tag:"div"
      ~attr:Vdom.Attr.(styles @ themed_attrs.body)
      rows
  ;;

  let view themed_attrs ~padding_top ~padding_bottom ~rows ~autosize =
    Vdom.Node.lazy_
      (lazy
        ((match autosize with
          | false -> view_impl
          | true -> table_view_impl)
           themed_attrs
           ~padding_top
           ~padding_bottom
           ~rows))
  ;;
end

module Table = struct
  let view
    (themed_attrs : Themed.t)
    ~private_body_classname
    ~vis_change_attr
    ~total_height
    ~autosize
    head
    body
    =
    (* If the number is large enough, it will use scientific notation for unknown reasons.
       However, the number is accurate, and scientific notation is in spec.
       https://developer.mozilla.org/en-US/docs/Web/CSS/number *)
    let inner_container_attrs =
      [ Vdom.Attr.class_ private_body_classname
      ; Vdom.Attr.style Css_gen.(height (`Px total_height))
        (* This attr determines where the visible client rect is for the body. If the
           structure of the elements it is placed on changes, row index/scroll position
           calculation may need to be corrected. *)
      ; vis_change_attr
      ]
    in
    let children =
      match autosize with
      | false ->
        [ head
        ; Vdom.Node.div
            ~attrs:
              (Functional_style.default_partial_render_table_body :: inner_container_attrs)
            [ body ]
        ]
      | true -> [ Vdom.Node.div ~attrs:inner_container_attrs [ head; body ] ]
    in
    Vdom.Node.div
      ~attrs:[ themed_attrs.table; Functional_style.partial_render_table_container ]
      children
  ;;
end
