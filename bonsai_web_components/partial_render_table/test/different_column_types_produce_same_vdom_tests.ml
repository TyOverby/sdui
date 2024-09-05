open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax

let init ~autosize ~num_rows config =
  let input =
    Shared_with_bench.Prt_input.create
      ~autosize
      (Shared_with_bench.Row.init_rows num_rows)
  in
  let component = Shared_with_bench.Config.computation config (return input) in
  let handle = Handle.create (Result_spec.vdom fst) component in
  Handle.show handle
;;

let test ~num_rows config =
  print_endline "===== Not Autosize =====";
  init ~autosize:false ~num_rows config;
  print_endline "\n\n===== Autosize =====";
  init ~autosize:true ~num_rows config
;;

let%expect_test "Flat vdom" =
  let f config =
    test ~num_rows:5 config;
    [%expect
      {|
      ===== Not Autosize =====
      <div class="partial_render_table_container_hash_replaced_in_test table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-focused-fg_hash_replaced_in_test black)(--row-focused-border_hash_replaced_in_test #0a90bf)(--row-focused-bg_hash_replaced_in_test #e0f7ff)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--cell-focused-fg_hash_replaced_in_test black)(--cell-focused-bg_hash_replaced_in_test #e0f7ff)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <table class="header_hash_replaced_in_test partial_render_table_header_hash_replaced_in_test"
               bounds-change=<opaque>>
          <tbody>
            <tr class="header_row_hash_replaced_in_test">
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> symbol </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> max_edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bsize </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bid </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> ask </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> asize </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body_hash_replaced_in_test partial-render-table-body-bonsai_path_replaced_in_test"
             bounds-change=<opaque>
             style={
               height: 5px;
             }>
          <div class="body_hash_replaced_in_test" style={ padding-top: 0px; padding-bottom: 0px; }>
            <div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> JANE0 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> JANE1 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
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
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> JANE2 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 2. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
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
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> JANE3 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 3 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 3. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 3 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> JANE4 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 4 </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 4. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 4 </div>
              </div>
            </div>
          </div>
        </div>
      </div>


      ===== Autosize =====
      <div class="partial_render_table_container_hash_replaced_in_test table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-focused-fg_hash_replaced_in_test black)(--row-focused-border_hash_replaced_in_test #0a90bf)(--row-focused-bg_hash_replaced_in_test #e0f7ff)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--cell-focused-fg_hash_replaced_in_test black)(--cell-focused-bg_hash_replaced_in_test #e0f7ff)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <div class="partial-render-table-body-bonsai_path_replaced_in_test"
             bounds-change=<opaque>
             style={
               height: 5px;
             }>
          <thead class="header_hash_replaced_in_test partial_render_table_header_hash_replaced_in_test"
                 bounds-change=<opaque>>
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> symbol </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> max_edge </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bsize </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bid </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> ask </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> asize </th>
            </tr>
          </thead>
          <div class="body_hash_replaced_in_test"
               style={
                 display: table-row-group;
                 position: relative;
               }>
            <div @key=top_padding style={ height: 0px; }> </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> JANE0 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0 </div>
              </div>
            </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> JANE1 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1 </div>
              </div>
            </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> JANE2 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 2 </div>
              </div>
            </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> JANE3 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 3. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 3. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 3 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 3. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 3. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 3 </div>
              </div>
            </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> JANE4 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 4. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 4. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 4 </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 4. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 4. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 4 </div>
              </div>
            </div>
            <div @key=bottom_border class="body_row_hash_replaced_in_test">
              <div class="autosize_table_bottom_border_element_hash_replaced_in_test body_cell_hash_replaced_in_test"> </div>
            </div>
            <div @key=bottom_padding style={ height: 0px; }> </div>
          </div>
        </div>
      </div>
      |}]
  in
  List.iter Shared_with_bench.Config.all_flat ~f
;;

let%expect_test "Grouped vdom" =
  let f config =
    test ~num_rows:2 config;
    [%expect
      {|
      ===== Not Autosize =====
      <div class="partial_render_table_container_hash_replaced_in_test table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-focused-fg_hash_replaced_in_test black)(--row-focused-border_hash_replaced_in_test #0a90bf)(--row-focused-bg_hash_replaced_in_test #e0f7ff)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--cell-focused-fg_hash_replaced_in_test black)(--cell-focused-bg_hash_replaced_in_test #e0f7ff)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <table class="header_hash_replaced_in_test partial_render_table_header_hash_replaced_in_test"
               bounds-change=<opaque>>
          <tbody>
            <tr class="header_row_hash_replaced_in_test">
              <td class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test"> Edges </td>
              <td class="header_cell_hash_replaced_in_test"> </td>
            </tr>
            <tr class="header_row_hash_replaced_in_test">
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                    display: none;
                  }> max_edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bsize </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body_hash_replaced_in_test partial-render-table-body-bonsai_path_replaced_in_test"
             bounds-change=<opaque>
             style={
               height: 2px;
             }>
          <div class="body_hash_replaced_in_test" style={ padding-top: 0px; padding-bottom: 0px; }>
            <div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                       display: none;
                     }> </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 0 </div>
              </div>
              <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                   style={
                     height: 1px;
                     width: 0.00px;
                     display: flex;
                     flex-direction: row;
                     flex-wrap: nowrap;
                   }>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                     }> 1. </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       width: 0.00px;
                       min-width: 0.00px;
                       max-width: 0.00px;
                       display: none;
                     }> </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
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
            </div>
          </div>
        </div>
      </div>


      ===== Autosize =====
      <div class="partial_render_table_container_hash_replaced_in_test table_hash_replaced_in_test"
           custom-css-vars=((--row-odd-fg_hash_replaced_in_test black)(--row-odd-bg_hash_replaced_in_test white)(--row-focused-fg_hash_replaced_in_test black)(--row-focused-border_hash_replaced_in_test #0a90bf)(--row-focused-bg_hash_replaced_in_test #e0f7ff)(--row-even-fg_hash_replaced_in_test black)(--row-even-bg_hash_replaced_in_test #e6e6e6)(--header-header-border_hash_replaced_in_test grey)(--header-fg_hash_replaced_in_test white)(--header-body-border_hash_replaced_in_test grey)(--header-bg_hash_replaced_in_test black)(--fg_hash_replaced_in_test black)(--cell-focused-fg_hash_replaced_in_test black)(--cell-focused-bg_hash_replaced_in_test #e0f7ff)(--body-body-border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <div class="partial-render-table-body-bonsai_path_replaced_in_test"
             bounds-change=<opaque>
             style={
               height: 2px;
             }>
          <thead class="header_hash_replaced_in_test partial_render_table_header_hash_replaced_in_test"
                 bounds-change=<opaque>>
            <tr class="header_row_hash_replaced_in_test">
              <th class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test"> Edges </th>
              <th class="header_cell_hash_replaced_in_test"> </th>
            </tr>
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                    display: none;
                  }> max_edge </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> bsize </th>
            </tr>
          </thead>
          <div class="body_hash_replaced_in_test"
               style={
                 display: table-row-group;
                 position: relative;
               }>
            <div @key=top_padding style={ height: 0px; }> </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       display: none;
                     }> </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 0 </div>
              </div>
            </div>
            <div class="body_row_hash_replaced_in_test row_hash_replaced_in_test"
                 style={
                   height: 1px;
                   display: table-row;
                 }>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1. </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                       display: none;
                     }> </div>
              </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test">
                <div class="body_cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 1px;
                       min-height: 1px;
                       max-height: 1px;
                     }> 1 </div>
              </div>
            </div>
            <div @key=bottom_border class="body_row_hash_replaced_in_test">
              <div class="autosize_table_bottom_border_element_hash_replaced_in_test body_cell_hash_replaced_in_test"> </div>
            </div>
            <div @key=bottom_padding style={ height: 0px; }> </div>
          </div>
        </div>
      </div>
      |}]
  in
  List.iter Shared_with_bench.Config.all_grouped ~f
;;
