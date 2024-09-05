open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
open Shared

module Test = struct
  include Shared.Test

  let create
    (type a column_id)
    ?(visible_range = 0, 100)
    ?(map = small_map)
    ?(should_print_styles = false)
    ?(should_set_bounds = true)
    component
    : (a, column_id) t
    =
    let min_vis, max_vis = visible_range in
    let input_var = Bonsai.Expert.Var.create map in
    let filter_var = Bonsai.Expert.Var.create (fun ~key:_ ~data:_ -> true) in
    let { Component.component
        ; get_vdom
        ; get_testing = _
        ; get_focus = _
        ; get_inject
        ; get_num_filtered_rows
        ; summarize_focus = _
        }
      =
      component (Bonsai.Expert.Var.value input_var) (Bonsai.Expert.Var.value filter_var)
    in
    let handle =
      Handle.create
        (module struct
          type t = a

          let view result =
            result
            |> get_vdom
            |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
            |> Virtual_dom_test_helpers.Node_helpers.to_string_html
                 ~path_censoring_message:""
                 ~hash_censoring_message:""
                 ~filter_printed_attributes:(fun ~key ~data:_ ->
                   should_print_styles || not (String.is_prefix ~prefix:"style." key))
          ;;

          type incoming = column_id Action.t

          let incoming = get_inject
        end)
        component
    in
    let t = { handle; get_vdom; input_var; filter_var; get_num_filtered_rows } in
    if should_set_bounds then set_bounds t ~low:min_vis ~high:max_vis;
    t
  ;;

  let print_message_on_result_recomputation t =
    let result = Incr.map (Handle.result_incr t.handle) ~f:t.get_vdom in
    Incr.Observer.on_update_exn (Incr.observe result) ~f:(function
      | Initialized _ -> print_endline "Initialized"
      | Changed _ -> print_endline "Changed"
      | Invalidated -> assert false)
  ;;
end

let%expect_test "autosizing table initial HTML" =
  let test =
    Test.create
      (Test.Component.default ~autosize:(Bonsai.return true) ~theming:`Themed ())
      ~visible_range:(0, 2)
      ~should_set_bounds:false
  in
  Handle.show_diff test.handle;
  [%expect
    {|
    +|<div class="partial_render_table_container table"
    +|     custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
    +|  <div class="partial-render-table-body-" bounds-change=<opaque>>
    +|    <thead class="header partial_render_table_header" bounds-change=<opaque>>
    +|      <tr class="header_row">
    +|        <th colspan="1"
    +|            class="header_cell header_label leaf_header leaf_header_resizable"
    +|            size_tracker=<fun>>
    +|          <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    +|            <div>
    +|              <span> key </span>
    +|            </div>
    +|          </div>
    +|        </th>
    +|        <th colspan="1"
    +|            class="header_cell header_label leaf_header leaf_header_resizable"
    +|            size_tracker=<fun>>
    +|          <div>
    +|            <div>
    +|              <span> a </span>
    +|            </div>
    +|          </div>
    +|        </th>
    +|        <th colspan="1"
    +|            class="header_cell header_label leaf_header leaf_header_resizable"
    +|            size_tracker=<fun>>
    +|          <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    +|            <div>
    +|              <span> b </span>
    +|            </div>
    +|          </div>
    +|        </th>
    +|        <th colspan="1"
    +|            class="header_cell header_label leaf_header leaf_header_resizable"
    +|            size_tracker=<fun>>
    +|          <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    +|            <div>
    +|              <span> d </span>
    +|            </div>
    +|          </div>
    +|        </th>
    +|      </tr>
    +|    </thead>
    +|    <div class="body">
    +|      <div @key=top_padding> </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 0 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|      </div>
    +|      <div @key=bottom_border class="body_row">
    +|        <div class="autosize_table_bottom_border_element body_cell"> </div>
    +|      </div>
    +|      <div @key=bottom_padding> </div>
    +|    </div>
    +|  </div>
    +|</div>
    |}]
;;

let%expect_test "column visibility" =
  let is_column_b_visible_var = Bonsai.Expert.Var.create true in
  let is_column_b_visible = Bonsai.Expert.Var.value is_column_b_visible_var in
  let test =
    Test.create
      ~should_print_styles:true
      (Test.Component.default ~autosize:(Bonsai.return true) ~is_column_b_visible ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Bonsai.Expert.Var.set is_column_b_visible_var false;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>
                  style={
                    width: 50px;
    +|              display: none;
                  }>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div style={
                         display: flex;
                         flex-direction: row;
                         flex-wrap: nowrap;
                         align-items: baseline;
                         column-gap: 6px;
                       }>
                    <span> b </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>
    === DIFF HUNK ===
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }>
                  <input @on_input> </input>
                  hello
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
    -|               }> 1.000000 </div>
    +|                 display: none;
    +|               }> </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1 </div>
              </div>
            </div>
            <div class="body_row row" style={ height: 1px; display: table-row; }>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
    === DIFF HUNK ===
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
    -|               }> 2.000000 </div>
    +|                 display: none;
    +|               }> </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2 </div>
              </div>
            </div>
            <div class="body_row row" style={ height: 1px; display: table-row; }>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
    === DIFF HUNK ===
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
    -|               }> 2.000000 </div>
    +|                 display: none;
    +|               }> </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> --- </div>
              </div>
            </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding style={ height: 0px; }> </div>
          </div>
    |}]
;;

let%expect_test "stabilization of view range" =
  let test =
    Test.create
      (Test.Component.default ~autosize:(Bonsai.return true) ())
      ~visible_range:(0, 2)
      ~should_set_bounds:false
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container table"
         custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
      <div class="partial-render-table-body-" bounds-change=<opaque>>
        <thead class="header partial_render_table_header" bounds-change=<opaque>>
          <tr class="header_row">
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </th>
          </tr>
        </thead>
        <div class="body">
          <div @key=top_padding> </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 0 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hello
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                there
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2 </div>
            </div>
          </div>
          <div @key=bottom_border class="body_row">
            <div class="autosize_table_bottom_border_element body_cell"> </div>
          </div>
          <div @key=bottom_padding> </div>
        </div>
      </div>
    </div>
    |}];
  (* Change the visibility to show the rest of the nodes *)
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect {| |}];
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect {| |}];
  Test.set_bounds test ~low:0 ~high:100;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2 </div>
              </div>
            </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 4 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            world
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> --- </div>
    +|        </div>
    +|      </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "resize-column" =
  let resize_via_size_changed_hook (test : _ Test.t) ~idx ~width =
    Test.resize_column test ~idx ~width
  in
  let resize_via_result_function (test : (_, Indexed_column_id.t) Test.t) ~idx ~width =
    Handle.do_actions
      test.handle
      [ Set_column_width { column_id = Indexed_column_id.of_int idx; width } ]
  in
  let test =
    Test.create
      ~should_print_styles:true
      (Test.Component.default ~autosize:(Bonsai.return true) ())
  in
  (*
     Size changed hook shouldn't do anything for the autosize view
  *)
  let resize = resize_via_size_changed_hook in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  resize test ~idx:0 ~width:10.0 ~autosize:true;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect {| |}];
  List.iter [ resize_via_result_function ] ~f:(fun resize ->
    let test =
      Test.create
        ~should_print_styles:true
        (Test.Component.default ~autosize:(Bonsai.return true) ())
    in
    Handle.recompute_view_until_stable test.handle;
    Handle.store_view test.handle;
    resize test ~idx:0 ~width:10.0;
    Handle.recompute_view_until_stable test.handle;
    Handle.show_diff ~location_style:Separator test.handle;
    [%expect
      {|
      === DIFF HUNK ===
        <div class="partial_render_table_container table"
             custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
          <div class="partial-render-table-body-" bounds-change=<opaque> style={ height: 3px; }>
            <thead class="header partial_render_table_header" bounds-change=<opaque>>
              <tr class="header_row">
                <th colspan="1"
                    class="header_cell header_label leaf_header leaf_header_resizable"
                    size_tracker=<fun>
                    style={
      -|              width: 50px;
      +|              width: 10.00px;
                    }>
                  <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                    <div style={
                           display: flex;
                           flex-direction: row;
                           flex-wrap: nowrap;
                           align-items: baseline;
                           column-gap: 6px;
                         }>
                      <span> key </span>
                    </div>
                  </div>
                </th>
                <th colspan="1"
                    class="header_cell header_label leaf_header leaf_header_resizable"
                    size_tracker=<fun>
      |}])
;;

let%expect_test "big table" =
  (* The PRT always renders [low-25, high+25], so 50,50 will render a big chunk
     centered at 50 *)
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(50, 50)
      (Test.Component.default ~autosize:(Bonsai.return true) ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container table"
         custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
      <div class="partial-render-table-body-" bounds-change=<opaque>>
        <thead class="header partial_render_table_header" bounds-change=<opaque>>
          <tr class="header_row">
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </th>
          </tr>
        </thead>
        <div class="body">
          <div @key=top_padding> </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 51 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hi
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 25.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 100 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 52 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hi
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 26.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 100 </div>
            </div>
          </div>
          <div @key=bottom_border class="body_row">
            <div class="autosize_table_bottom_border_element body_cell"> </div>
          </div>
          <div @key=bottom_padding> </div>
        </div>
      </div>
    </div>
    |}];
  (* extending the range upwards should only add to the end *)
  Test.set_bounds test ~low:55 ~high:60;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 51 </div>
    +|          <div class="body_cell" @on_click> 55 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hi
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 25.000000 </div>
    +|          <div class="body_cell" @on_click> 27.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 100 </div>
              </div>
            </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 52 </div>
    +|          <div class="body_cell" @on_click> 56 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hi
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 26.000000 </div>
    +|          <div class="body_cell" @on_click> 28.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 100 </div>
    +|        </div>
    +|      </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 57 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 28.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 100 </div>
    +|        </div>
    +|      </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 58 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 29.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 100 </div>
    +|        </div>
    +|      </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 59 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 29.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 100 </div>
    +|        </div>
    +|      </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 60 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 30.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 100 </div>
    +|        </div>
    +|      </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 61 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 30.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 100 </div>
    +|        </div>
    +|      </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 62 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 31.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 100 </div>
              </div>
            </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "typing into a column, leaving that column, and then coming back. " =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(50, 50)
      (Test.Component.default ~autosize:(Bonsai.return true) ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Handle.input_text
    test.handle
    ~get_vdom:Table.Result.view
    ~selector:"input"
    ~text:"hello world";
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 51 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
    -|            hi
    +|            hi hello world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 25.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 100 </div>
              </div>
            </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 52 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
    |}];
  (* move out of bounds (really 99-25 through 100) *)
  Test.set_bounds test ~low:99 ~high:99;
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  (* move back into bounds *)
  Test.set_bounds test ~low:50 ~high:50;
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container table"
         custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
      <div class="partial-render-table-body-" bounds-change=<opaque>>
        <thead class="header partial_render_table_header" bounds-change=<opaque>>
          <tr class="header_row">
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </th>
          </tr>
        </thead>
        <div class="body">
          <div @key=top_padding> </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 51 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hi hello world
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 25.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 100 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 52 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hi
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 26.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 100 </div>
            </div>
          </div>
          <div @key=bottom_border class="body_row">
            <div class="autosize_table_bottom_border_element body_cell"> </div>
          </div>
          <div @key=bottom_padding> </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "table body is not recomputed more often than necessary" =
  (* The size_tracker and visibility hooks that PRT uses can be called by the browser more
     often than one would expect. For instance, if one places an element over the table,
     it causes the size_tracker hook on every column to fire. If you have a large table
     with lots of columns and lots of rows, it can be expensive to recompute the table
     body n times, once for each column. *)
  let test = Test.create (Test.Component.default ~autosize:(Bonsai.return true) ()) in
  Test.print_message_on_result_recomputation test;
  Test.resize_column test ~idx:0 ~width:1. ~autosize:true;
  Handle.recompute_view test.handle;
  Test.set_bounds test ~low:0 ~high:300;
  Handle.recompute_view test.handle;
  [%expect
    {|
    Initialized
    Changed
    |}];
  (* Sanity check: re-stabilizing after doing no actions does not cause recomputation *)
  Handle.recompute_view test.handle;
  [%expect {| |}];
  (* Re-setting a column to its existing width should not cause a re-fire *)
  Test.resize_column test ~idx:0 ~width:1. ~autosize:true;
  Handle.recompute_view test.handle;
  [%expect {| |}];
  (* Re-setting the bounds to the same value should not cause a re-fire *)
  Test.set_bounds test ~low:0 ~high:300;
  Handle.recompute_view test.handle;
  [%expect {| |}]
;;

let%expect_test "table body is not recomputed more often than necessary" =
  let test =
    Test.create (fun input _filter_var ->
      let component graph =
        let%sub collation, key_rank =
          Table_expert.collate
            ~filter_equal:[%compare.equal: unit]
            ~order_equal:[%compare.equal: unit]
            ~filter_to_predicate:(fun () -> None)
            ~order_to_compare:(fun () -> Unchanged)
            input
            (Bonsai.return
               { Incr_map_collate.Collate.filter = ()
               ; order = ()
               ; key_range = All_rows
               ; rank_range = All_rows
               })
            graph
        in
        let columns =
          [ Table_expert.Columns.Dynamic_cells.column
              ~header:(Bonsai.return (Vdom.Node.text "key"))
              ~cell:(fun ~key ~data:_ _graph ->
                let%arr key = key in
                Vdom.Node.textf "%d" key)
              ()
          ]
          |> Table_expert.Columns.Dynamic_cells.lift
        in
        Table_expert.component
          (module Int)
          ~focus:
            (By_row
               { on_change = Bonsai.return (Fn.const Effect.Ignore)
               ; compute_presence = (fun focus _graph -> focus)
               ; key_rank
               })
          ~row_height:(Bonsai.return (`Px 10))
          ~columns
          ~autosize:(Bonsai.return true)
          collation
          graph
      in
      { Test.Component.component
      ; get_vdom = Table_expert.Result.view
      ; get_testing = Table_expert.Result.for_testing
      ; get_inject = Shared.Test.Component.get_inject_expert
      ; get_focus = Table_expert.Result.focus
      ; get_num_filtered_rows = (fun _ -> None)
      ; summarize_focus = (fun ?num_filtered_rows:_ _ -> "")
      })
  in
  Test.print_message_on_result_recomputation test;
  Handle.recompute_view test.handle;
  Test.resize_column test ~idx:0 ~width:1. ~autosize:true;
  Test.set_bounds test ~low:0 ~high:300;
  Handle.recompute_view test.handle;
  [%expect {| Initialized |}];
  (* Sanity check: re-stabilizing after doing no actions does not cause recomputation *)
  Handle.recompute_view test.handle;
  [%expect {| |}];
  (* Changing the bounds should not cause a re-fire because we are doing our own collation
     and don't rely on result.bounds. *)
  Test.set_bounds test ~low:100 ~high:300;
  Handle.recompute_view test.handle;
  [%expect {| |}]
;;

let%expect_test "test is browser" =
  let open Js_of_ocaml in
  Dom_html.document |> Obj.magic |> Js_of_ocaml.Js.Optdef.test |> printf "%b";
  [%expect {| false |}]
;;

let%expect_test "sorting legacy renderer" =
  let test =
    Test.create
      (Test.Component.default ~autosize:(Bonsai.return true) ~use_legacy_header:true ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container table"
         custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
      <div class="partial-render-table-body-" bounds-change=<opaque>>
        <thead class="header partial_render_table_header" bounds-change=<opaque>>
          <tr class="header_row">
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <span> ◇  key </span>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div> a </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <span> ◇  b </span>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <span> ◇  d </span>
              </div>
            </th>
          </tr>
        </thead>
        <div class="body">
          <div @key=top_padding> </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 0 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hello
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                there
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 4 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                world
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> --- </div>
            </div>
          </div>
          <div @key=bottom_border class="body_row">
            <div class="autosize_table_bottom_border_element body_cell"> </div>
          </div>
          <div @key=bottom_padding> </div>
        </div>
      </div>
    </div>
    |}];
  (* this one is the key, clicking on it does nothing (it's already sorted by the key) *)
  Handle.click_on test.handle ~selector:"th:nth-child(1) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ◇  key </span>
    +|            <span> ⬘  key </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  b </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
    |}];
  (* this one actually does stuff, click on it twice for a reverse sort *)
  Handle.click_on test.handle ~selector:"th:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.click_on test.handle ~selector:"th:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ⬘  key </span>
    +|            <span> ◇  key </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ◇  b </span>
    +|            <span> ⬙  b </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  d </span>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 0 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2 </div>
              </div>
    === DIFF HUNK ===
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
            </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 0 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|      </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}];
  Handle.click_on test.handle ~selector:"th:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  key </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ⬙  b </span>
    +|            <span> ◇  b </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  d </span>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 0 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2 </div>
              </div>
    === DIFF HUNK ===
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
            </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 0 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|      </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}];
  (* specialized reverse sort *)
  (* for these items, nothing changes for the primary sort*)
  Handle.click_on test.handle ~selector:"th:nth-child(4) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff test.handle;
  [%expect
    {|
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  b </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ◇  d </span>
    +|            <span> ⬘  d </span>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
              </div>
    |}];
  (* but in reverse, notice that [None]s stay on the bottom *)
  Handle.click_on test.handle ~selector:"th:nth-child(4) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff test.handle;
  [%expect
    {|
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  b </span>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ⬘  d </span>
    +|            <span> ⬙  d </span>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            there
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2 </div>
    +|        </div>
    +|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
            </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            there
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 2.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 2 </div>
    -|        </div>
    -|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
    |}]
;;

let%expect_test "sorting default renderer" =
  let multisort_columns_when = Bonsai.Expert.Var.create `Shift_click in
  let test =
    Test.create
      (Test.Component.default
         ~autosize:(Bonsai.return true)
         ~multisort_columns_when:(Bonsai.Expert.Var.value multisort_columns_when)
         ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container table"
         custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
      <div class="partial-render-table-body-" bounds-change=<opaque>>
        <thead class="header partial_render_table_header" bounds-change=<opaque>>
          <tr class="header_row">
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </th>
            <th colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </th>
          </tr>
        </thead>
        <div class="body">
          <div @key=top_padding> </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 0 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                hello
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 1 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                there
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2 </div>
            </div>
          </div>
          <div class="body_row row">
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 4 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click>
                <input @on_input> </input>
                world
              </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> 2.000000 </div>
            </div>
            <div class="autosize_table_cell_wrapper">
              <div class="body_cell" @on_click> --- </div>
            </div>
          </div>
          <div @key=bottom_border class="body_row">
            <div class="autosize_table_bottom_border_element body_cell"> </div>
          </div>
          <div @key=bottom_padding> </div>
        </div>
      </div>
    </div>
    |}];
  (* this one is the key, clicking on it does nothing (it's already sorted by the key) *)
  Handle.click_on test.handle ~selector:"th:nth-child(1) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    +|              <span> ▲ </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    |}];
  (* this one actually does stuff, click on it twice for a reverse sort *)
  Handle.click_on test.handle ~selector:"th:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.click_on test.handle ~selector:"th:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    -|              <span> ▲ </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    +|              <span> ▼ </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 0 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2 </div>
              </div>
    === DIFF HUNK ===
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
            </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 0 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|      </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}];
  (* Ctrl+clicking while disabled for multiselect *)
  Handle.click_on
    ~ctrl_key_down:true
    test.handle
    ~selector:"th:nth-child(4) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    -|              <span> ▼ </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
    +|              <span> ▲ </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 0 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2 </div>
              </div>
    === DIFF HUNK ===
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
            </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 0 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|      </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}];
  (* Ctrl+clicking while enabled for multiselect: should select both *)
  Bonsai.Expert.Var.set multisort_columns_when `Ctrl_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~ctrl_key_down:true
    test.handle
    ~selector:"th:nth-child(3) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    +|              <span> ▲ 2 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
    -|              <span> ▲ </span>
    +|              <span> ▲ 1 </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
    |}];
  (* Ctrl+clicking while either enabled: should add another to multiselect *)
  Bonsai.Expert.Var.set multisort_columns_when `Shift_or_ctrl_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~ctrl_key_down:true
    test.handle
    ~selector:"th:nth-child(1) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    +|              <span> ▲ 3 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    |}];
  (* Ctrl+clicking while that mode is disabled again *)
  Bonsai.Expert.Var.set multisort_columns_when `Shift_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~ctrl_key_down:true
    test.handle
    ~selector:"th:nth-child(4) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    -|              <span> ▲ 3 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    -|              <span> ▲ 2 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
    -|              <span> ▲ 1 </span>
    +|              <span> ▼ </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            there
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2 </div>
    +|        </div>
    +|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
            </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            there
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 2.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 2 </div>
    -|        </div>
    -|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
    |}];
  (* Shift+clicking while disabled for multiselect *)
  Bonsai.Expert.Var.set multisort_columns_when `Ctrl_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~shift_key_down:true
    test.handle
    ~selector:"th:nth-child(4) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
    -|              <span> ▼ </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 0 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2 </div>
              </div>
            </div>
    -|      <div class="body_row row">
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 0 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1.000000 </div>
    -|        </div>
    -|        <div class="autosize_table_cell_wrapper">
    -|          <div class="body_cell" @on_click> 1 </div>
    -|        </div>
    -|      </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 2.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> --- </div>
              </div>
    |}];
  (* Shift+clicking while enabled for multiselect: should select both *)
  Bonsai.Expert.Var.set multisort_columns_when `Shift_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~shift_key_down:true
    test.handle
    ~selector:"th:nth-child(3) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    +|              <span> ▲ </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    |}];
  (* Shift+clicking while either enabled: should add another to multiselect *)
  Bonsai.Expert.Var.set multisort_columns_when `Shift_or_ctrl_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~shift_key_down:true
    test.handle
    ~selector:"th:nth-child(1) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    +|              <span> ▲ 2 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    -|              <span> ▲ </span>
    +|              <span> ▲ 1 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    |}];
  (* Shift+clicking while that mode is disabled again *)
  Bonsai.Expert.Var.set multisort_columns_when `Ctrl_click;
  Handle.recompute_view_until_stable test.handle;
  Handle.click_on
    ~shift_key_down:true
    test.handle
    ~selector:"th:nth-child(4) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container table"
           custom-css-vars=((--row-odd-fg black)(--row-odd-bg white)(--row-focused-fg black)(--row-focused-border #0a90bf)(--row-focused-bg #e0f7ff)(--row-even-fg black)(--row-even-bg #e6e6e6)(--header-header-border grey)(--header-fg white)(--header-body-border grey)(--header-bg black)(--fg black)(--cell-focused-fg black)(--cell-focused-bg #e0f7ff)(--body-body-border grey)(--bg white))>
        <div class="partial-render-table-body-" bounds-change=<opaque>>
          <thead class="header partial_render_table_header" bounds-change=<opaque>>
            <tr class="header_row">
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    -|              <span> ▲ 2 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    -|              <span> ▲ 1 </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
    +|              <span> ▲ </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
    |}];
  (* Clicking already sorted twice removed all sorts *)
  Handle.click_on test.handle ~selector:"th:nth-child(4) > div" ~get_vdom:test.get_vdom;
  Handle.click_on test.handle ~selector:"th:nth-child(4) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
    -|              <span> ▲ </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
            <div class="body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
    |}]
;;

let%expect_test "locking columns also disallows focus change due to clicks" =
  let test =
    Test.create
      (Test.Component.default ~autosize:(Bonsai.return true) ~theming:`Themed ())
      ~visible_range:(0, 2)
      ~should_set_bounds:false
  in
  Handle.store_view test.handle;
  Handle.click_on test.handle ~get_vdom:test.get_vdom ~selector:"div div div div div div";
  Handle.show_diff test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))

                  </div>
                </div>
              </th>
              <th colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </th>
            </tr>
          </thead>
          <div class="body">
            <div @key=top_padding> </div>
    -|      <div class="body_row row">
    +|      <div class="body_row_focused body_row row">
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1.000000 </div>
              </div>
              <div class="autosize_table_cell_wrapper">
                <div class="body_cell" @on_click> 1 </div>
              </div>
            </div>
    +|      <div class="body_row row">
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 1 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click>
    +|            <input @on_input> </input>
    +|            there
    +|          </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2.000000 </div>
    +|        </div>
    +|        <div class="autosize_table_cell_wrapper">
    +|          <div class="body_cell" @on_click> 2 </div>
    +|        </div>
    +|      </div>
            <div @key=bottom_border class="body_row">
              <div class="autosize_table_bottom_border_element body_cell"> </div>
            </div>
            <div @key=bottom_padding> </div>
          </div>
        </div>
      </div>
    |}];
  Handle.do_actions test.handle [ Lock_focus ];
  (* Clicking does nothing while the focus is locked *)
  Handle.click_on
    test.handle
    ~get_vdom:test.get_vdom
    ~selector:"div div div div div:nth-child(2) div";
  Handle.show_diff test.handle;
  [%expect {| |}];
  Handle.do_actions test.handle [ Unlock_focus ];
  (* Clicking moves the focus again after it's unlocked *)
  Handle.click_on
    test.handle
    ~get_vdom:test.get_vdom
    ~selector:"div div div div div:nth-child(2) div";
  Handle.show_diff test.handle;
  [%expect {| scrolling to index 0 at 0.0px |}]
;;
