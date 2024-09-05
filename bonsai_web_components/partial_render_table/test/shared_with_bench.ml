open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Prt = Bonsai_web_ui_partial_render_table
module Table = Prt.Expert
open Bonsai_perf_shared

module Row = struct
  module T = struct
    type t =
      { symbol : string
      ; edge : float
      ; max_edge : float
      ; bsize : int
      ; bid : float
      ; ask : float
      ; asize : int
      }
    [@@deriving compare, fields ~fields, sexp, typed_fields]
  end

  include T
  include Comparator.Make (T)

  let of_int i =
    { symbol = [%string "JANE%{i#Int}"]
    ; edge = Float.of_int i
    ; max_edge = Float.of_int i
    ; bsize = i
    ; bid = Float.of_int i
    ; ask = Float.of_int i
    ; asize = i
    }
  ;;

  let init_rows n = List.init n ~f:(fun x -> x, of_int x) |> Map.of_alist_exn (module Int)
end

module type S = sig
  type column_id

  val first_column : column_id
  val all : (int, Row.t, column_id) Table.Columns.t
  val with_column_groups : (int, Row.t, column_id) Table.Columns.t
end

let dynamic_cells ~stateful_cells : (module S) =
  (module struct
    module type S = sig
      type t [@@deriving compare]

      val to_string : t -> string
    end

    module Column = Table.Columns.Dynamic_cells

    type column_id = Prt.Indexed_column_id.t

    let first_column = Prt.Indexed_column_id.of_int 0

    let column_helper
      (type a)
      (module M : S with type t = a)
      ?visible
      (field : (_, a) Field.t)
      =
      Column.column
        ?visible
        ~header:(return (Vdom.Node.text (Fieldslib.Field.name field)))
        ~cell:(fun ~key:_ ~data graph ->
          let%sub state =
            match stateful_cells with
            | true ->
              let state, _ = Bonsai.state () graph in
              state
            | false -> return ()
          in
          let%arr data = data
          and () = state in
          Vdom.Node.text (M.to_string (Field.get field data)))
        ()
    ;;

    let all =
      [ column_helper (module String) Row.Fields.symbol
      ; column_helper (module Float) Row.Fields.edge
      ; column_helper (module Float) Row.Fields.max_edge
      ; column_helper (module Int) Row.Fields.bsize
      ; column_helper (module Float) Row.Fields.bid
      ; column_helper (module Float) Row.Fields.ask
      ; column_helper (module Int) Row.Fields.asize
      ]
      |> Column.lift
    ;;

    let with_column_groups =
      [ Column.group
          ~label:(Vdom.Node.text "Edges" |> return)
          [ column_helper (module Float) Row.Fields.edge
          ; column_helper (module Float) Row.Fields.max_edge ~visible:(return false)
          ]
      ; column_helper (module Int) Row.Fields.bsize
      ]
      |> Column.lift
    ;;
  end)
;;

module Dynamic_columns : S = struct
  module type S = sig
    type t [@@deriving compare]

    val to_string : t -> string
  end

  module Column = Table.Columns.Dynamic_columns

  type column_id = Prt.Indexed_column_id.t

  let first_column = Prt.Indexed_column_id.of_int 0

  let column_helper
    (type a)
    (module M : S with type t = a)
    ?visible
    (field : (_, a) Field.t)
    =
    Column.column
      ?visible
      ~header:(Vdom.Node.text (Fieldslib.Field.name field))
      ~cell:(fun ~key:_ ~data -> Vdom.Node.text (M.to_string (Field.get field data)))
      ()
  ;;

  let all =
    [ column_helper (module String) Row.Fields.symbol
    ; column_helper (module Float) Row.Fields.edge
    ; column_helper (module Float) Row.Fields.max_edge
    ; column_helper (module Int) Row.Fields.bsize
    ; column_helper (module Float) Row.Fields.bid
    ; column_helper (module Float) Row.Fields.ask
    ; column_helper (module Int) Row.Fields.asize
    ]
    |> return
    |> Column.lift
  ;;

  let with_column_groups =
    [ Column.group
        ~label:(Vdom.Node.text "Edges")
        [ column_helper (module Float) Row.Fields.edge
        ; column_helper (module Float) Row.Fields.max_edge ~visible:false
        ]
    ; column_helper (module Int) Row.Fields.bsize
    ]
    |> return
    |> Column.lift
  ;;
end

let dynamic_experimental ~stateful_cells : (module S) =
  (module struct
    module Column = Table.Columns.Dynamic_experimental

    module Col_id = struct
      include Row.Typed_field.Packed
      include Comparator.Make (Row.Typed_field.Packed)
    end

    type column_id = Col_id.t

    let first_column = Row.Typed_field.Packed.all |> List.hd_exn

    let render_header col _ =
      let%arr { Row.Typed_field.Packed.f = T field } = col in
      Vdom.Node.text (Row.Typed_field.name field)
    ;;

    let render_cell col _k row graph =
      let state =
        match stateful_cells with
        | true ->
          let state, _ = Bonsai.state () graph in
          state
        | false -> return ()
      in
      let%arr { Row.Typed_field.Packed.f = T field } = col
      and row = row
      and () = state in
      let string, float, int =
        ( Vdom.Node.text
        , (fun x -> Vdom.Node.text (Float.to_string x))
        , fun x -> Vdom.Node.text (Int.to_string x) )
      in
      let value = Row.Typed_field.get field row in
      match field with
      | Symbol -> string value
      | Edge -> float value
      | Max_edge -> float value
      | Bsize -> int value
      | Bid -> float value
      | Ask -> float value
      | Asize -> int value
    ;;

    let (all : (int, Row.t, column_id) Table.Columns.t) =
      Column.build
        (module Col_id)
        ~render_header
        ~render_cell
        ~columns:(return Row.Typed_field.Packed.all)
    ;;

    let with_column_groups = all
  end)
;;

module Prt_input = struct
  type ('key, 'data, 'cmp) t =
    { filter : (key:'key -> data:'data -> bool) option
    ; order : ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    ; rank_range : int Incr_map_collate.Collate.Which_range.t
    ; key_range : 'key Incr_map_collate.Collate.Which_range.t
    ; autosize : bool
    ; map : ('key, 'data, 'cmp) Map.t
    }

  let create
    ?(filter = None)
    ?(order = Incr_map_collate.Compare.Unchanged)
    ?(rank_range = Incr_map_collate.Collate.Which_range.To 100)
    ?(key_range = Incr_map_collate.Collate.Which_range.All_rows)
    ?(autosize = false)
    map
    =
    { filter; order; rank_range; key_range; autosize; map }
  ;;

  let apply_filter t filter =
    Interaction.update_input t ~f:(fun x -> { x with filter = Some filter })
  ;;

  let clear_filter t = Interaction.update_input t ~f:(fun x -> { x with filter = None })
  let update_map t ~f = Interaction.update_input t ~f:(fun x -> { x with map = f x.map })
  let set_order t order = Interaction.update_input t ~f:(fun x -> { x with order })

  let set_rank_range t rank_range =
    Interaction.update_input t ~f:(fun x -> { x with rank_range })
  ;;

  let scroll t ~start ~stop ~window_size =
    let stride = if start > stop then -1 else 1 in
    List.range ~stride start stop
    |> List.map ~f:(fun i ->
      set_rank_range
        t
        (Incr_map_collate.Collate.Which_range.Between (i, i + window_size - 1)))
    |> Interaction.many_with_stabilizations
  ;;
end

module Action = struct
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

module Config = struct
  type input = (int, Row.t, Int.comparator_witness) Prt_input.t
  type output = Vdom.Node.t * (int Action.t -> unit Effect.t)
  type action = int Action.t

  type t =
    | Dynamic_cells of
        { use_state_in_cells : bool
        ; col_groups : bool
        }
    | Dynamic_cols of { col_groups : bool }
    | Dynamic_experimental of { use_state_in_cells : bool }
  [@@deriving equal, compare, sexp_of, enumerate, hash]

  let name = function
    | Dynamic_cells { use_state_in_cells; col_groups } ->
      "dyn cells"
      ^ (if use_state_in_cells then " (state)" else " (stateless)")
      ^ if col_groups then " (groups)" else "(flat)"
    | Dynamic_cols { col_groups } ->
      "dyn cols" ^ if col_groups then " (groups)" else "(flat)"
    | Dynamic_experimental { use_state_in_cells } ->
      "dyn-exp" ^ if use_state_in_cells then "(state)" else " (stateless)"
  ;;

  let group_vs_flat_partition =
    List.partition_tf all ~f:(function
      | Dynamic_experimental _ -> false
      | Dynamic_cols { col_groups; _ } -> col_groups
      | Dynamic_cells { col_groups; _ } -> col_groups)
  ;;

  let all_flat = snd group_vs_flat_partition
  let all_grouped = fst group_vs_flat_partition

  module For_component = struct
    type t =
      | T :
          { columns : (int, Row.t, 'column_id) Table.Columns.t
          ; first_column : 'column_id
          }
          -> t

    let build = function
      | Dynamic_experimental { use_state_in_cells } ->
        let module Table = (val dynamic_experimental ~stateful_cells:use_state_in_cells)
        in
        T { first_column = Table.first_column; columns = Table.all }
      | Dynamic_cols { col_groups } ->
        T
          { first_column = Dynamic_columns.first_column
          ; columns =
              (if col_groups
               then Dynamic_columns.with_column_groups
               else Dynamic_columns.all)
          }
      | Dynamic_cells { use_state_in_cells; col_groups } ->
        let module Table = (val dynamic_cells ~stateful_cells:use_state_in_cells) in
        T
          { first_column = Table.first_column
          ; columns = (if col_groups then Table.with_column_groups else Table.all)
          }
    ;;
  end

  let computation config input graph =
    let (T { columns; first_column }) = For_component.build config in
    let%sub { Prt_input.map; autosize; _ } = input in
    let%sub collate, key_rank =
      let collate =
        let%arr { filter; order; key_range; rank_range; autosize = _; map = _ } = input in
        { Incr_map_collate.Collate.filter; order; key_range; rank_range }
      in
      Table.collate
        ~filter_equal:phys_equal
        ~filter_to_predicate:Fn.id
        ~order_equal:phys_equal
        ~order_to_compare:Fn.id
        map
        collate
        graph
    in
    let%sub { view; focus; _ } =
      Table.component
        (module Int)
        ~autosize
        ~focus:
          (Table.Focus.By_cell
             { on_change = return (fun _ -> Effect.Ignore)
             ; compute_presence = (fun x _ -> return x)
             ; key_rank
             })
        ~row_height:(return (`Px 1))
        ~columns
        collate
        graph
    in
    let inject =
      let%arr focus = focus in
      let module Focus_control = Table.Focus.By_cell in
      function
      | Action.Unfocus -> Focus_control.unfocus focus
      | Focus_up -> Focus_control.focus_up focus
      | Focus_down -> Focus_control.focus_down focus
      | Focus_left -> Focus_control.focus_left focus
      | Focus_right -> Focus_control.focus_right focus
      | Page_up -> Focus_control.page_up focus
      | Page_down -> Focus_control.page_down focus
      | Focus_first_column key -> (Focus_control.focus focus) key first_column
    in
    Bonsai.both view inject
  ;;

  let get_inject (_, inject) = inject
end

module Scenarios = struct
  let focus_and_unfocus ~size ~in_range =
    let starting_map = Row.init_rows size in
    let not_ = if in_range then "" else "not " in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name =
        [%string
          "Focus by key (key %{not_}present) and unfocus in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          let index = if in_range then 1 else size + 1 in
          [ Interaction.inject (Action.Focus_first_column index)
          ; Interaction.inject Action.Unfocus
          ; Interaction.reset_model
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let focus_up_and_down ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Focus up and down in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Action.Focus_down; Interaction.inject Action.Focus_up ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  (* It doesn't make a lot of sense to have a really large number of columns for left/right
     focus benchmarks, so we just do it within the same size table as everything else which
     seems more realistic. *)
  let focus_left_and_right ~num_rows =
    let starting_map = Row.init_rows num_rows in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Focus left and right in a map with %{num_rows#Int} rows"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Action.Focus_left; Interaction.inject Action.Focus_right ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let page_up_and_down ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Page up and down in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Action.Page_down; Interaction.inject Action.Page_up ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let scroll ~size ~start ~stop ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name =
        [%string
          "Scroll %{window_size#Int}-wide window from %{start#Int} to %{stop#Int} and \
           back in %{size#Int} element map"]
    ; interaction =
        (fun input ->
          [ Prt_input.scroll input ~start ~stop ~window_size
          ; Prt_input.scroll input ~start:stop ~stop:start ~window_size
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let apply_filters ~size ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Apply 4 filters and clear with %{size#Int} element map using \
           %{window_size#Int} window"]
    ; interaction =
        (fun input ->
          [ Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 2 = 0)
          ; Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 3 = 0)
          ; Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 4 = 0)
          ; Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 5 = 0)
          ; Prt_input.clear_filter input
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let invert_ordering ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Invert ordering of %{size#Int} element map"]
    ; interaction =
        (fun input ->
          [ Prt_input.set_order
              input
              (Custom_by_key_and_value
                 { compare =
                     (fun (_, a) (_, b) -> [%compare: int] a.Row.asize b.Row.asize)
                 })
          ; Prt_input.set_order
              input
              (Custom_by_key_and_value
                 { compare =
                     (fun (_, a) (_, b) -> [%compare: int] b.Row.asize a.Row.asize)
                 })
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  (* [set_map] sets performs [num_sets * batch_size] map sets in total, with stabilization
     happening every [batch_size] changes. [window_size] specifies the size of the window,
     and the sets wrap around it. *)
  let set_map ~size ~num_sets ~batch_size ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Perform %{num_sets#Int} sets of %{batch_size#Int} items in a %{size#Int} \
           element map with %{window_size#Int}-wide window "]
    ; interaction =
        (fun input ->
          [ List.init num_sets ~f:(fun set_num ->
              Prt_input.update_map input ~f:(fun current_map ->
                let new_map, _ =
                  Fn.apply_n_times
                    ~n:batch_size
                    (fun (m, i) ->
                      let index = (set_num * batch_size) + i in
                      let new_ =
                        Map.set
                          m
                          ~key:(index mod window_size)
                          ~data:(Row.of_int (index + size))
                      in
                      new_, i + 1)
                    (current_map, 0)
                in
                new_map))
            |> Interaction.many_with_stabilizations
          ; Prt_input.update_map input ~f:(fun _ -> starting_map)
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;
end

let scenarios =
  let open Scenarios in
  [ focus_and_unfocus ~size:10 ~in_range:false
  ; focus_and_unfocus ~size:100 ~in_range:false
  ; focus_and_unfocus ~size:101 ~in_range:false
  ; focus_and_unfocus ~size:1000 ~in_range:false
  ; focus_and_unfocus ~size:10000 ~in_range:false
  ; focus_and_unfocus ~size:10 ~in_range:true
  ; focus_and_unfocus ~size:100 ~in_range:true
  ; focus_and_unfocus ~size:101 ~in_range:true
  ; focus_and_unfocus ~size:1000 ~in_range:true
  ; focus_and_unfocus ~size:10000 ~in_range:true
  ; focus_up_and_down ~size:10
  ; focus_up_and_down ~size:100
  ; focus_up_and_down ~size:101
  ; focus_up_and_down ~size:1000
  ; focus_up_and_down ~size:10000
  ; focus_left_and_right ~num_rows:10
  ; focus_left_and_right ~num_rows:100
  ; focus_left_and_right ~num_rows:101
  ; focus_left_and_right ~num_rows:1000
  ; focus_left_and_right ~num_rows:10000
  ; page_up_and_down ~size:10
  ; page_up_and_down ~size:100
  ; page_up_and_down ~size:101
  ; page_up_and_down ~size:1000
  ; page_up_and_down ~size:10000
  ; scroll ~size:100 ~start:0 ~stop:9 ~window_size:1
  ; scroll ~size:100 ~start:0 ~stop:9 ~window_size:10
  ; scroll ~size:1000 ~start:0 ~stop:9 ~window_size:1
  ; scroll ~size:1000 ~start:0 ~stop:9 ~window_size:10
  ; scroll ~size:1000 ~start:0 ~stop:9 ~window_size:100
  ; apply_filters ~size:100 ~window_size:10
  ; apply_filters ~size:101 ~window_size:10
  ; apply_filters ~size:1000 ~window_size:10
  ; apply_filters ~size:1000 ~window_size:50
  ; apply_filters ~size:10000 ~window_size:50
  ; apply_filters ~size:10000 ~window_size:100
  ; invert_ordering ~size:10
  ; invert_ordering ~size:100
  ; invert_ordering ~size:101
  ; invert_ordering ~size:1000
  ; set_map ~size:10 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:10 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:11 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:11 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:100 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:100 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:1000 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:1000 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:1000 ~num_sets:10 ~batch_size:10 ~window_size:100
  ]
;;
