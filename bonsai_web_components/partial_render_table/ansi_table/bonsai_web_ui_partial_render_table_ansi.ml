open! Core
module Table = Bonsai_web_ui_partial_render_table

let table_to_string
  ~include_stats
  ?(include_num_column = true)
  ?(selected_header = ">")
  ?(additional_summary = "")
  (for_testing : Table.For_testing.t)
  ()
  =
  let open Ascii_table_kernel in
  let module Node_h = Virtual_dom_test_helpers.Node_helpers in
  let stats =
    Ascii_table_kernel.draw
      ~limit_width_to:200
      ~prefer_split_on_spaces:false
      [ Column.create "metric" (fun (k, _) -> k)
      ; Column.create "value" (fun (_, v) -> v)
      ]
      [ "rows-before", sprintf "%d" for_testing.body.rows_before
      ; "rows-after", sprintf "%d" for_testing.body.rows_after
      ; "num-filtered", sprintf "%d" for_testing.body.num_filtered
      ; "num-unfiltered", sprintf "%d" for_testing.body.num_unfiltered
      ]
    |> Option.value_exn
    |> Ascii_table_kernel.Screen.to_string
         ~bars:`Unicode
         ~string_with_attr:(fun _attr str -> str)
  in
  let contents =
    let row_focused =
      Column.create
        selected_header
        (fun { Table.For_testing.Table_body.row_focused; _ } ->
           if row_focused then "*" else "")
    in
    let num_column =
      Column.create "#" (fun { Table.For_testing.Table_body.id; _ } ->
        Opaque_map.Key.to_string id)
    in
    let ascii_column_of_leaf i headers =
      let header =
        String.concat
          ~sep:"\n"
          (List.map headers ~f:(fun header ->
             Node_h.unsafe_convert_exn header |> Node_h.inner_text))
      in
      Column.create header (fun { Table.For_testing.Table_body.cells; _ } ->
        List.nth_exn cells i
        |> fun { Table.For_testing.Table_body.view; cell_focused; _ } ->
        let text = view |> Node_h.unsafe_convert_exn |> Node_h.inner_text in
        if cell_focused then [%string "> %{text} <"] else text)
    in
    let columns =
      match include_num_column with
      | false ->
        row_focused :: (for_testing.body.column_names |> List.mapi ~f:ascii_column_of_leaf)
      | true ->
        row_focused
        :: num_column
        :: (for_testing.body.column_names |> List.mapi ~f:ascii_column_of_leaf)
    in
    Ascii_table_kernel.draw
      columns
      for_testing.body.rows
      ~limit_width_to:3000
      ~prefer_split_on_spaces:false
    |> Option.value_exn
    |> Ascii_table_kernel.Screen.to_string
         ~bars:`Unicode
         ~string_with_attr:(fun _attr str -> str)
  in
  let result = if include_stats then stats ^ contents else contents in
  additional_summary ^ result
;;
