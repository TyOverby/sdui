open! Core

type 'a t =
  { main_interval : 'a Int.Map.t
  ; end_of_parse : (int * 'a) option
  }
[@@deriving sexp_of]

let to_string t f =
  let alist_to_string entries =
    List.map entries ~f:(fun (k, v) -> [%string "%{k#Int}: %{f v#String}"])
    |> String.concat_lines
  in
  let main_string = Map.to_alist t.main_interval |> alist_to_string in
  let end_of_parse_string = Option.to_list t.end_of_parse |> alist_to_string in
  main_string ^ end_of_parse_string
;;

let empty = { main_interval = Int.Map.empty; end_of_parse = None }

let add_regular_checkpoint map ~pos ~data =
  match Map.add map ~key:pos ~data with
  | `Ok map' -> Or_error.return map'
  | `Duplicate ->
    Or_error.error_s [%message "Checkpoint already exists at this location" (pos : int)]
;;

let add_checkpoint t ~pos ~data ?(at_end_of_parse = false) () =
  match at_end_of_parse with
  | true -> Or_error.return { t with end_of_parse = Some (pos, data) }
  | false ->
    let%map.Or_error t' = add_regular_checkpoint t.main_interval ~pos ~data in
    { t with main_interval = t' }
;;

let compare ~key ~data:_ = Int.compare key

let get_last_checkpoint t ?(before = Int.max_value) () =
  Map.binary_search t.main_interval ~compare `Last_less_than_or_equal_to before
;;

let discard_checkpoints t ~after =
  { main_interval = Map.split_lt_ge t.main_interval after |> fst; end_of_parse = None }
;;

let combine_end_of_parse t =
  match t.end_of_parse with
  | None -> t.main_interval
  | Some (pos, data) ->
    add_regular_checkpoint t.main_interval ~pos ~data
    |> Or_error.ok
    |> Option.value ~default:t.main_interval
;;

let get_checkpoints_around_range t ~min ~max =
  let map = combine_end_of_parse t in
  let get_neighbor_index which pos =
    Map.binary_search map ~compare which pos
    |> Option.map ~f:fst
    |> Option.value ~default:pos
  in
  let min = get_neighbor_index `Last_less_than_or_equal_to min in
  let max = get_neighbor_index `First_greater_than_or_equal_to max in
  Map.range_to_alist map ~min ~max
;;

let%test_module "checkpoint tests" =
  (module struct
    open Or_error.Let_syntax

    (* inclusive of both bounds *)
    let create_many_checkpoints ~from ~to_ ~interval ~f =
      let rec add t pos =
        if pos <= to_
        then add (add_checkpoint t ~pos ~data:(f pos) () |> ok_exn) (pos + interval)
        else t
      in
      add empty from
    ;;

    let checkpoint_to_string (a, b) = [%string "(%{a#Int}, %{b#Int})"]

    let checkpoint_option_to_string = function
      | None -> "-"
      | Some x -> checkpoint_to_string x
    ;;

    let range_to_string range =
      String.concat ~sep:", " (List.map range ~f:checkpoint_to_string)
    ;;

    let%expect_test "adding checkpoints" =
      let t0 = empty in
      let _ : unit Or_error.t =
        let%bind t1 = add_checkpoint t0 ~pos:5 ~data:"a" () in
        let%bind t2 = add_checkpoint t1 ~pos:2 ~data:"b" () in
        let%bind t3 = add_checkpoint t2 ~pos:4 ~data:"c" () in
        List.iteri [ t0; t1; t2; t3 ] ~f:(fun i t ->
          print_endline [%string "After adding %{i#Int} times:"];
          to_string t String.to_string |> print_endline);
        return ()
      in
      [%expect
        {|
        After adding 0 times:

        After adding 1 times:
        5: a

        After adding 2 times:
        2: b
        5: a

        After adding 3 times:
        2: b
        4: c
        5: a
        |}]
    ;;

    let%expect_test "disgarding checkpoints" =
      let big_checkpoints =
        create_many_checkpoints ~from:0 ~to_:20 ~interval:1 ~f:Fn.id
      in
      List.iter [ -1; 0; 1; 10; 19; 20; 21 ] ~f:(fun i ->
        let discarded = discard_checkpoints big_checkpoints ~after:i in
        print_endline [%string "After discarding at %{i#Int}:"];
        to_string discarded Int.to_string |> print_endline);
      [%expect
        {|
        After discarding at -1:

        After discarding at 0:

        After discarding at 1:
        0: 0

        After discarding at 10:
        0: 0
        1: 1
        2: 2
        3: 3
        4: 4
        5: 5
        6: 6
        7: 7
        8: 8
        9: 9

        After discarding at 19:
        0: 0
        1: 1
        2: 2
        3: 3
        4: 4
        5: 5
        6: 6
        7: 7
        8: 8
        9: 9
        10: 10
        11: 11
        12: 12
        13: 13
        14: 14
        15: 15
        16: 16
        17: 17
        18: 18

        After discarding at 20:
        0: 0
        1: 1
        2: 2
        3: 3
        4: 4
        5: 5
        6: 6
        7: 7
        8: 8
        9: 9
        10: 10
        11: 11
        12: 12
        13: 13
        14: 14
        15: 15
        16: 16
        17: 17
        18: 18
        19: 19

        After discarding at 21:
        0: 0
        1: 1
        2: 2
        3: 3
        4: 4
        5: 5
        6: 6
        7: 7
        8: 8
        9: 9
        10: 10
        11: 11
        12: 12
        13: 13
        14: 14
        15: 15
        16: 16
        17: 17
        18: 18
        19: 19
        20: 20
        |}]
    ;;

    let%expect_test "getting individual checkpoints" =
      let big_checkpoints =
        create_many_checkpoints ~from:0 ~to_:20 ~interval:2 ~f:Fn.id
      in
      print_endline "Individual:";
      List.iter [ -1; 0; 1; 19; 20; 21 ] ~f:(fun i ->
        let checkpoint = get_last_checkpoint big_checkpoints ~before:i () in
        let checkpoint_string = checkpoint_option_to_string checkpoint in
        print_endline [%string "%{i#Int} --> %{checkpoint_string}"]);
      let last_checkpoint = get_last_checkpoint big_checkpoints () in
      let last_checkpoint_string = checkpoint_option_to_string last_checkpoint in
      print_endline "";
      print_endline [%string "Last: %{last_checkpoint_string}"];
      [%expect
        {|
        Individual:
        -1 --> -
        0 --> (0, 0)
        1 --> (0, 0)
        19 --> (18, 18)
        20 --> (20, 20)
        21 --> (20, 20)

        Last: (20, 20)
        |}]
    ;;

    let%expect_test "getting ranges of checkpoints" =
      let big_checkpoints =
        create_many_checkpoints ~from:0 ~to_:20 ~interval:2 ~f:Fn.id
      in
      List.iter
        [ 0, 0; -5, -1; 0, 1; 1, 1; 3, 5; -1, 21; 21, 21 ]
        ~f:(fun (i1, i2) ->
          let range = get_checkpoints_around_range big_checkpoints ~min:i1 ~max:i2 in
          let range_string = range_to_string range in
          print_endline [%string "(%{i1#Int}, %{i2#Int}) --> %{range_string}"]);
      [%expect
        {|
        (0, 0) --> (0, 0)
        (-5, -1) --> (0, 0)
        (0, 1) --> (0, 0), (2, 2)
        (1, 1) --> (0, 0), (2, 2)
        (3, 5) --> (2, 2), (4, 4), (6, 6)
        (-1, 21) --> (0, 0), (2, 2), (4, 4), (6, 6), (8, 8), (10, 10), (12, 12), (14, 14), (16, 16), (18, 18), (20, 20)
        (21, 21) --> (20, 20)
        |}]
    ;;

    let%expect_test "empty cases" =
      print_s
        [%message
          (discard_checkpoints empty ~after:0 : unit t)
            (get_last_checkpoint empty () : (int * unit) option)
            (get_checkpoints_around_range empty ~min:0 ~max:1 : (int * unit) list)];
      [%expect
        {|
        (("discard_checkpoints empty ~after:0"
          ((main_interval ()) (end_of_parse ())))
         ("get_last_checkpoint empty ()" ())
         ("get_checkpoints_around_range empty ~min:0 ~max:1" ()))
        |}]
    ;;

    let%expect_test "end-of-parse checkpoints are used in getting ranges but not \
                     individual checkpoints"
      =
      let main = create_many_checkpoints ~from:0 ~to_:10 ~interval:5 ~f:Fn.id in
      let _ : unit Or_error.t =
        let%bind end_added =
          add_checkpoint main ~pos:3 ~data:3 ~at_end_of_parse:true ()
        in
        let individual_checkpoint = get_last_checkpoint end_added ~before:4 () in
        let range = get_checkpoints_around_range end_added ~min:2 ~max:4 in
        print_endline
          [%string "Individual: %{checkpoint_option_to_string individual_checkpoint}"];
        print_endline [%string "Range: %{range_to_string range}"];
        return ()
      in
      [%expect
        {|
        Individual: (0, 0)
        Range: (0, 0), (3, 3), (5, 5)
        |}]
    ;;
  end)
;;
