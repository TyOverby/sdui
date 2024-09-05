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

let%expect_test "column visibility" =
  let is_column_b_visible_var = Bonsai.Expert.Var.create true in
  let is_column_b_visible = Bonsai.Expert.Var.value is_column_b_visible_var in
  let test =
    Test.create
      ~should_print_styles:true
      (Test.Component.default ~theming:`Legacy_don't_use_theme ~is_column_b_visible ())
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
              </td>
              <td colspan="1"
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
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>
    === DIFF HUNK ===
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }>
                  <input @on_input> </input>
                  hello
                </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
    -|               }> 1.000000 </div>
    +|                 display: none;
    +|               }> </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1 </div>
              </div>
              <div class="prt-table-row row"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
    === DIFF HUNK ===
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
    -|               }> 2.000000 </div>
    +|                 display: none;
    +|               }> </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2 </div>
              </div>
              <div class="prt-table-row row"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
    === DIFF HUNK ===
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
    -|               }> 2.000000 </div>
    +|                 display: none;
    +|               }> </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> --- </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "stabilization of view range" =
  let test =
    Test.create
      (Test.Component.default ~theming:`Legacy_don't_use_theme ())
      ~visible_range:(0, 2)
      ~should_set_bounds:false
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container">
      <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
        <tbody>
          <tr>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
        <div>
          <div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 0 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hello
              </div>
              <div class="cell prt-table-cell" @on_click> 1.000000 </div>
              <div class="cell prt-table-cell" @on_click> 1 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 1 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                there
              </div>
              <div class="cell prt-table-cell" @on_click> 2.000000 </div>
              <div class="cell prt-table-cell" @on_click> 2 </div>
            </div>
          </div>
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
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
                <div class="cell prt-table-cell" @on_click> 1.000000 </div>
                <div class="cell prt-table-cell" @on_click> 1 </div>
              </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 1 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> 2 </div>
              </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 4 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            world
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 2.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> --- </div>
    +|        </div>
            </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "resize-column" =
  let test =
    Test.create
      ~should_print_styles:true
      (Test.Component.default ~theming:`Legacy_don't_use_theme ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Test.resize_column test ~idx:0 ~width:10.0 ~autosize:false;
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
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
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>
    === DIFF HUNK ===
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-"
             bounds-change=<opaque>
             style={
               height: 3px;
             }>
          <div style={ padding-top: 0px; padding-bottom: 0px; }>
            <div>
              <div class="prt-table-row row"
                   style={
                     height: 1px;
    -|               width: 0.00px;
    +|               width: 10.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
    -|                 width: 0.00px;
    +|                 width: 10.00px;
    -|                 min-width: 0.00px;
    +|                 min-width: 10.00px;
    -|                 max-width: 0.00px;
    +|                 max-width: 10.00px;
                     }> 0 </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }>
                  <input @on_input> </input>
                  hello
                </div>
                <div class="cell prt-table-cell"
                     @on_click
    === DIFF HUNK ===
                       max-width: 0.00px;
                     }> 1.000000 </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1 </div>
              </div>
              <div class="prt-table-row row"
                   style={
                     height: 1px;
    -|               width: 0.00px;
    +|               width: 10.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
    -|                 width: 0.00px;
    +|                 width: 10.00px;
    -|                 min-width: 0.00px;
    +|                 min-width: 10.00px;
    -|                 max-width: 0.00px;
    +|                 max-width: 10.00px;
                     }> 1 </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell"
                     @on_click
    === DIFF HUNK ===
                       max-width: 0.00px;
                     }> 2.000000 </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2 </div>
              </div>
              <div class="prt-table-row row"
                   style={
                     height: 1px;
    -|               width: 0.00px;
    +|               width: 10.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
    -|                 width: 0.00px;
    +|                 width: 10.00px;
    -|                 min-width: 0.00px;
    +|                 min-width: 10.00px;
    -|                 max-width: 0.00px;
    +|                 max-width: 10.00px;
                     }> 4 </div>
                <div class="cell prt-table-cell"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell"
                     @on_click
    |}]
;;

let%expect_test "big table" =
  (* The PRT always renders [low-25, high+25], so 50,50 will render a big chunk
     centered at 50 *)
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(50, 50)
      (Test.Component.default ~theming:`Legacy_don't_use_theme ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container">
      <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
        <tbody>
          <tr>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
        <div>
          <div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 51 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hi
              </div>
              <div class="cell prt-table-cell" @on_click> 25.000000 </div>
              <div class="cell prt-table-cell" @on_click> 100 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 52 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hi
              </div>
              <div class="cell prt-table-cell" @on_click> 26.000000 </div>
              <div class="cell prt-table-cell" @on_click> 100 </div>
            </div>
          </div>
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
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
              <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 51 </div>
    +|          <div class="cell prt-table-cell" @on_click> 55 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  hi
                </div>
    -|          <div class="cell prt-table-cell" @on_click> 25.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 27.000000 </div>
                <div class="cell prt-table-cell" @on_click> 100 </div>
              </div>
              <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 52 </div>
    +|          <div class="cell prt-table-cell" @on_click> 56 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  hi
                </div>
    -|          <div class="cell prt-table-cell" @on_click> 26.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 28.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 100 </div>
    +|        </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 57 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 28.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 100 </div>
    +|        </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 58 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 29.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 100 </div>
    +|        </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 59 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 29.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 100 </div>
    +|        </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 60 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 30.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 100 </div>
    +|        </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 61 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 30.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 100 </div>
    +|        </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 62 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hi
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 31.000000 </div>
                <div class="cell prt-table-cell" @on_click> 100 </div>
              </div>
            </div>
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
      (Test.Component.default ~theming:`Legacy_don't_use_theme ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.store_view test.handle;
  Handle.input_text
    test.handle
    ~get_vdom:Table.Result.view
    ~selector:".prt-table-cell:nth-child(2) input"
    ~text:"hello world";
  Handle.recompute_view_until_stable test.handle;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 51 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
    -|            hi
    +|            hi hello world
                </div>
                <div class="cell prt-table-cell" @on_click> 25.000000 </div>
                <div class="cell prt-table-cell" @on_click> 100 </div>
              </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 52 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  hi
                </div>
                <div class="cell prt-table-cell" @on_click> 26.000000 </div>
                <div class="cell prt-table-cell" @on_click> 100 </div>
              </div>
            </div>
          </div>
        </div>
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
    <div class="partial_render_table_container">
      <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
        <tbody>
          <tr>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
        <div>
          <div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 51 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hi hello world
              </div>
              <div class="cell prt-table-cell" @on_click> 25.000000 </div>
              <div class="cell prt-table-cell" @on_click> 100 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 52 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hi
              </div>
              <div class="cell prt-table-cell" @on_click> 26.000000 </div>
              <div class="cell prt-table-cell" @on_click> 100 </div>
            </div>
          </div>
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
  let test = Test.create (Test.Component.default ~theming:`Legacy_don't_use_theme ()) in
  Test.print_message_on_result_recomputation test;
  Test.resize_column test ~idx:0 ~width:1. ~autosize:false;
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
  Test.resize_column test ~idx:0 ~width:1. ~autosize:false;
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
          ~theming:`Legacy_don't_use_theme
          ~focus:
            (By_row
               { on_change = Bonsai.return (Fn.const Effect.Ignore)
               ; compute_presence = (fun focus _graph -> focus)
               ; key_rank
               })
          ~row_height:(Bonsai.return (`Px 10))
          ~columns
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
  Test.resize_column test ~idx:0 ~width:1. ~autosize:false;
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
      (Test.Component.default ~theming:`Legacy_don't_use_theme ~use_legacy_header:true ())
  in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container">
      <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
        <tbody>
          <tr>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <span> ◇  key </span>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div> a </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <span> ◇  b </span>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <span> ◇  d </span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
        <div>
          <div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 0 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hello
              </div>
              <div class="cell prt-table-cell" @on_click> 1.000000 </div>
              <div class="cell prt-table-cell" @on_click> 1 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 1 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                there
              </div>
              <div class="cell prt-table-cell" @on_click> 2.000000 </div>
              <div class="cell prt-table-cell" @on_click> 2 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 4 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                world
              </div>
              <div class="cell prt-table-cell" @on_click> 2.000000 </div>
              <div class="cell prt-table-cell" @on_click> --- </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  (* this one is the key, clicking on it does nothing (it's already sorted by the key) *)
  Handle.click_on test.handle ~selector:"td:nth-child(1) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ◇  key </span>
    +|            <span> ⬘  key </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  b </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
    |}];
  (* this one actually does stuff, click on it twice for a reverse sort *)
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ⬘  key </span>
    +|            <span> ◇  key </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ◇  b </span>
    +|            <span> ⬙  b </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  d </span>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
    -|        <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 0 </div>
    -|          <div class="cell prt-table-cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    -|          <div class="cell prt-table-cell" @on_click> 1 </div>
    -|        </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 1 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> 2 </div>
              </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 4 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> --- </div>
              </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 0 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 1 </div>
    +|        </div>
            </div>
          </div>
        </div>
      </div>
    |}];
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  key </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ⬙  b </span>
    +|            <span> ◇  b </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  d </span>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 0 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 1 </div>
    +|        </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 1 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> 2 </div>
              </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 4 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> --- </div>
              </div>
    -|        <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 0 </div>
    -|          <div class="cell prt-table-cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    -|          <div class="cell prt-table-cell" @on_click> 1 </div>
    -|        </div>
            </div>
          </div>
        </div>
      </div>
    |}];
  (* specialized reverse sort *)
  (* for these items, nothing changes for the primary sort*)
  Handle.click_on test.handle ~selector:"td:nth-child(4) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff test.handle;
  [%expect
    {|
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  b </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ◇  d </span>
    +|            <span> ⬘  d </span>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 0 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
                <div class="cell prt-table-cell" @on_click> 1.000000 </div>
                <div class="cell prt-table-cell" @on_click> 1 </div>
    |}];
  (* but in reverse, notice that [None]s stay on the bottom *)
  Handle.click_on test.handle ~selector:"td:nth-child(4) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff test.handle;
  [%expect
    {|
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div> a </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <span> ◇  b </span>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    -|            <span> ⬘  d </span>
    +|            <span> ⬙  d </span>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 1 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            there
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 2.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 2 </div>
    +|        </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 0 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  hello
                </div>
                <div class="cell prt-table-cell" @on_click> 1.000000 </div>
                <div class="cell prt-table-cell" @on_click> 1 </div>
              </div>
    -|        <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 1 </div>
    -|          <div class="cell prt-table-cell" @on_click>
    -|            <input @on_input> </input>
    -|            there
    -|          </div>
    -|          <div class="cell prt-table-cell" @on_click> 2.000000 </div>
    -|          <div class="cell prt-table-cell" @on_click> 2 </div>
    -|        </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 4 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> --- </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "sorting default renderer" =
  let test = Test.create (Test.Component.default ~theming:`Legacy_don't_use_theme ()) in
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    <div class="partial_render_table_container">
      <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
        <tbody>
          <tr>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> key </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
            <td colspan="1"
                class="header_cell header_label leaf_header leaf_header_resizable"
                size_tracker=<fun>>
              <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                <div>
                  <span> d </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
        <div>
          <div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 0 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                hello
              </div>
              <div class="cell prt-table-cell" @on_click> 1.000000 </div>
              <div class="cell prt-table-cell" @on_click> 1 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 1 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                there
              </div>
              <div class="cell prt-table-cell" @on_click> 2.000000 </div>
              <div class="cell prt-table-cell" @on_click> 2 </div>
            </div>
            <div class="prt-table-row row">
              <div class="cell prt-table-cell" @on_click> 4 </div>
              <div class="cell prt-table-cell" @on_click>
                <input @on_input> </input>
                world
              </div>
              <div class="cell prt-table-cell" @on_click> 2.000000 </div>
              <div class="cell prt-table-cell" @on_click> --- </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  (* this one is the key, clicking on it does nothing (it's already sorted by the key) *)
  Handle.click_on test.handle ~selector:"td:nth-child(1) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    +|              <span> ▲ </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
    |}];
  (* this one actually does stuff, click on it twice for a reverse sort *)
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    -|              <span> ▲ </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    +|              <span> ▼ </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
    -|        <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 0 </div>
    -|          <div class="cell prt-table-cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    -|          <div class="cell prt-table-cell" @on_click> 1 </div>
    -|        </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 1 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> 2 </div>
              </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 4 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> --- </div>
              </div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 0 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 1 </div>
    +|        </div>
            </div>
          </div>
        </div>
      </div>
    |}];
  (* Click on second column, creating a multi-sort  *)
  Handle.click_on
    ~shift_key_down:true
    test.handle
    ~selector:"td:nth-child(1) > div"
    ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    +|              <span> ▲ 2 </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    -|              <span> ▼ </span>
    +|              <span> ▼ 1 </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
    |}];
  (* Third column is already reverse sorted, so clicking it again removes all sorts. *)
  Handle.click_on test.handle ~selector:"td:nth-child(3) > div" ~get_vdom:test.get_vdom;
  Handle.show_diff ~location_style:Separator test.handle;
  [%expect
    {|
    === DIFF HUNK ===
      <div class="partial_render_table_container">
        <table class="partial_render_table_header prt-table-header" bounds-change=<opaque>>
          <tbody>
            <tr>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> key </span>
    -|              <span> ▲ 2 </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div>
                  <div>
                    <span> a </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> b </span>
    -|              <span> ▼ 1 </span>
                  </div>
                </div>
              </td>
              <td colspan="1"
                  class="header_cell header_label leaf_header leaf_header_resizable"
                  size_tracker=<fun>>
                <div role="button" tabindex="0" class="sortable_header_cell" @on_click>
                  <div>
                    <span> d </span>
                  </div>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body partial-render-table-body-" bounds-change=<opaque>>
          <div>
            <div>
    +|        <div class="prt-table-row row">
    +|          <div class="cell prt-table-cell" @on_click> 0 </div>
    +|          <div class="cell prt-table-cell" @on_click>
    +|            <input @on_input> </input>
    +|            hello
    +|          </div>
    +|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    +|          <div class="cell prt-table-cell" @on_click> 1 </div>
    +|        </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 1 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  there
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> 2 </div>
              </div>
              <div class="prt-table-row row">
                <div class="cell prt-table-cell" @on_click> 4 </div>
                <div class="cell prt-table-cell" @on_click>
                  <input @on_input> </input>
                  world
                </div>
                <div class="cell prt-table-cell" @on_click> 2.000000 </div>
                <div class="cell prt-table-cell" @on_click> --- </div>
              </div>
    -|        <div class="prt-table-row row">
    -|          <div class="cell prt-table-cell" @on_click> 0 </div>
    -|          <div class="cell prt-table-cell" @on_click>
    -|            <input @on_input> </input>
    -|            hello
    -|          </div>
    -|          <div class="cell prt-table-cell" @on_click> 1.000000 </div>
    -|          <div class="cell prt-table-cell" @on_click> 1 </div>
    -|        </div>
            </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "removed columns still count toward the total table width" =
  let module Table = Bonsai_web_ui_partial_render_table in
  let module Column = Table.Basic.Columns.Dynamic_columns in
  let map = Bonsai.return (Int.Map.of_alist_exn [ 1, 1; 2, 2 ]) in
  let render_header str = Column.Sortable.Header.with_icon (Vdom.Node.text str) in
  let column_a =
    Column.column
      ~header:(render_header "a")
      ~cell:(fun ~key:_ ~data -> Vdom.Node.text (Int.to_string data))
      ()
  in
  let column_b =
    Column.column
      ~header:(render_header "b")
      ~cell:(fun ~key:_ ~data -> Vdom.Node.text (Int.to_string (data * 2)))
      ()
  in
  let column_c =
    Column.column
      ~header:(render_header "c")
      ~cell:(fun ~key:_ ~data -> Vdom.Node.text (Int.to_string (data * 3)))
      ()
  in
  let columns_var = Bonsai.Expert.Var.create [ column_a; column_b ] in
  let component =
    Table.Basic.component
      (module Int)
      ~theming:`Legacy_don't_use_theme
      ~focus:None
      ~row_height:(Bonsai.return (`Px 20))
      ~columns:(Column.lift (Bonsai.Expert.Var.value columns_var))
      map
  in
  let get_vdom { Table.Basic.Result.view; _ } = view in
  let handle =
    Handle.create
      (Result_spec.vdom
         ~filter_printed_attributes:(fun ~key ~data:_ -> String.equal key "style.width")
         get_vdom)
      component
  in
  let resize_column ~idx ~width =
    Shared.Test.resize_column_for_handle handle ~get_vdom ~idx ~autosize:false ~width
  in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    <div>
      <table>
        <tbody>
          <tr>
            <td style={ width: 50px; }>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td style={ width: 50px; }>
              <div>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div>
        <div>
          <div>
            <div style={ width: 0.00px; }>
              <div style={ width: 0.00px; }> 1 </div>
              <div style={ width: 0.00px; }> 2 </div>
            </div>
            <div style={ width: 0.00px; }>
              <div style={ width: 0.00px; }> 2 </div>
              <div style={ width: 0.00px; }> 4 </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  resize_column ~idx:0 ~width:10.;
  resize_column ~idx:1 ~width:20.;
  Handle.show handle;
  [%expect
    {|
    <div>
      <table>
        <tbody>
          <tr>
            <td style={ width: 10.00px; }>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td style={ width: 20.00px; }>
              <div>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div>
        <div>
          <div>
            <div style={ width: 30.00px; }>
              <div style={ width: 10.00px; }> 1 </div>
              <div style={ width: 20.00px; }> 2 </div>
            </div>
            <div style={ width: 30.00px; }>
              <div style={ width: 10.00px; }> 2 </div>
              <div style={ width: 20.00px; }> 4 </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.set columns_var [ column_a; column_b; column_c ];
  Handle.recompute_view handle;
  resize_column ~idx:2 ~width:30.;
  Handle.show handle;
  [%expect
    {|
    <div>
      <table>
        <tbody>
          <tr>
            <td style={ width: 10.00px; }>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
            <td style={ width: 20.00px; }>
              <div>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
            <td style={ width: 30.00px; }>
              <div>
                <div>
                  <span> c </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div>
        <div>
          <div>
            <div style={ width: 60.00px; }>
              <div style={ width: 10.00px; }> 1 </div>
              <div style={ width: 20.00px; }> 2 </div>
              <div style={ width: 30.00px; }> 3 </div>
            </div>
            <div style={ width: 60.00px; }>
              <div style={ width: 10.00px; }> 2 </div>
              <div style={ width: 20.00px; }> 4 </div>
              <div style={ width: 30.00px; }> 6 </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.set columns_var [ column_b; column_c ];
  Handle.show handle;
  [%expect
    {|
    <div>
      <table>
        <tbody>
          <tr>
            <td style={ width: 10.00px; }>
              <div>
                <div>
                  <span> b </span>
                </div>
              </div>
            </td>
            <td style={ width: 20.00px; }>
              <div>
                <div>
                  <span> c </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div>
        <div>
          <div>
            <div style={ width: 30.00px; }>
              <div style={ width: 10.00px; }> 2 </div>
              <div style={ width: 20.00px; }> 3 </div>
            </div>
            <div style={ width: 30.00px; }>
              <div style={ width: 10.00px; }> 4 </div>
              <div style={ width: 20.00px; }> 6 </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "removed columns still count toward the total table width" =
  let module Table = Bonsai_web_ui_partial_render_table in
  let module Columns = Table.Basic.Columns.Dynamic_columns in
  let map = Bonsai.return (Int.Map.of_alist_exn (List.init 100 ~f:(fun i -> i, i))) in
  let column_a =
    Columns.column
      ~header:(Columns.Sortable.Header.with_icon (Vdom.Node.text "a"))
      ~cell:(fun ~key:_ ~data -> Vdom.Node.text (Int.to_string data))
      ()
  in
  let component =
    Table.Basic.component
      (module Int)
      ~theming:`Legacy_don't_use_theme
      ~focus:None
      ~row_height:(Bonsai.return (`Px 1))
      ~preload_rows:1
      ~columns:(Columns.lift (Bonsai.return [ column_a ]))
      map
  in
  let get_vdom { Table.Basic.Result.view; _ } = view in
  let handle =
    Handle.create
      (Result_spec.vdom
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           String.equal key "style.padding-top" || String.equal key "style.padding-bottom")
         get_vdom)
      component
  in
  Test.set_bounds_for_handle handle ~get_vdom ~low:5 ~high:10;
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    <div>
      <table>
        <tbody>
          <tr>
            <td>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div>
        <div style={ padding-top: 4px; padding-bottom: 87px; }>
          <div>
            <div>
              <div> 4 </div>
            </div>
            <div>
              <div> 5 </div>
            </div>
            <div>
              <div> 6 </div>
            </div>
            <div>
              <div> 7 </div>
            </div>
            <div>
              <div> 8 </div>
            </div>
            <div>
              <div> 9 </div>
            </div>
            <div>
              <div> 10 </div>
            </div>
            <div>
              <div> 11 </div>
            </div>
            <div>
              <div> 12 </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  Test.clear_bounds_for_handle handle ~get_vdom;
  Handle.show_diff ~location_style:Separator handle;
  [%expect {| |}];
  Handle.show handle;
  (* Note that clearing the visible bounds causes the padding to go to 0. *)
  [%expect
    {|
    <div>
      <table>
        <tbody>
          <tr>
            <td>
              <div>
                <div>
                  <span> a </span>
                </div>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div>
        <div style={ padding-top: 0px; padding-bottom: 97px; }>
          <div>
            <div>
              <div> 0 </div>
            </div>
            <div>
              <div> 1 </div>
            </div>
            <div>
              <div> 2 </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    |}]
;;
