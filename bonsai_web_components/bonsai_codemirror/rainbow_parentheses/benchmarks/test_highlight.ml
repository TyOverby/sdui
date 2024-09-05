open! Core
open Codemirror_rainbow_parentheses_kernel

module Rose_tree = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a t list
  [@@deriving sexp_of]

  let rec generate ~depth ~width =
    match depth with
    | 0 -> Leaf "test(();;#0"
    | _ -> List.init width ~f:(fun _ -> generate ~depth:(depth - 1) ~width) |> Node
  ;;

  let pp_tree t = sexp_of_t String.sexp_of_t t |> Sexp.to_string_hum
end

let%expect_test "test that generated strings look right" =
  Rose_tree.generate ~depth:2 ~width:2 |> Rose_tree.pp_tree |> print_endline;
  [%expect
    {|
    (Node
     ((Node ((Leaf "test(();;#0") (Leaf "test(();;#0")))
      (Node ((Leaf "test(();;#0") (Leaf "test(();;#0")))))
    |}]
;;

(* Original highlight function (highlighting full text):
   ┌────────────────────────────────────────────────┬──────────────────┬────────────────┬───────────────┬───────────────┬────────────┐
   │ Name                                           │         Time/Run │        mWd/Run │      mjWd/Run │      Prom/Run │ Percentage │
   ├────────────────────────────────────────────────┼──────────────────┼────────────────┼───────────────┼───────────────┼────────────┤
   │ [test_highlight.ml] shallow text highlight:1   │         757.84ns │        891.00w │               │               │            │
   │ [test_highlight.ml] shallow text highlight:4   │       8_274.36ns │      9_897.00w │         1.59w │         1.59w │            │
   │ [test_highlight.ml] shallow text highlight:16  │     119_977.25ns │    143_553.00w │       243.13w │       243.13w │      0.10% │
   │ [test_highlight.ml] shallow text highlight:64  │   2_413_536.35ns │  2_244_897.00w │    39_320.83w │    39_320.83w │      2.10% │
   │ [test_highlight.ml] shallow text highlight:256 │  39_730_228.99ns │ 35_717_793.00w │   782_739.15w │   782_739.15w │     34.60% │
   │ [test_highlight.ml] deep text highlight:1      │         995.56ns │      1_219.00w │               │               │            │
   │ [test_highlight.ml] deep text highlight:4      │      10_977.17ns │     12_285.00w │         3.30w │         3.30w │            │
   │ [test_highlight.ml] deep text highlight:16     │ 114_838_497.91ns │ 97_386_381.00w │ 2_350_336.57w │ 2_350_336.57w │    100.00% │
   └────────────────────────────────────────────────┴──────────────────┴────────────────┴───────────────┴───────────────┴────────────┘

   New highlight function:
   ┌──────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬───────────────┬───────────────┬────────────┐
   │ Name                                                         │         Time/Run │         mWd/Run │      mjWd/Run │      Prom/Run │ Percentage │
   ├──────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼───────────────┼───────────────┼────────────┤
   │ [test_highlight.ml] shallow text full highlight:1            │         944.18ns │       1_389.00w │               │               │            │
   │ [test_highlight.ml] shallow text full highlight:4            │      10_033.39ns │      14_919.00w │         2.54w │         2.54w │            │
   │ [test_highlight.ml] shallow text full highlight:16           │     145_108.11ns │     214_901.00w │       372.22w │       372.22w │      0.11% │
   │ [test_highlight.ml] shallow text full highlight:64           │   2_846_505.52ns │   3_357_283.00w │    43_966.74w │    43_966.74w │      2.14% │
   │ [test_highlight.ml] shallow text full highlight:256          │  45_413_687.78ns │  53_421_027.00w │   801_551.90w │   801_551.90w │     34.08% │
   │ [test_highlight.ml] deep text full highlight:1               │       1_248.82ns │       1_861.00w │               │               │            │
   │ [test_highlight.ml] deep text full highlight:4               │      12_723.40ns │      18_699.00w │         5.17w │         5.17w │            │
   │ [test_highlight.ml] deep text full highlight:16              │ 133_253_368.92ns │ 155_304_442.00w │ 2_421_140.67w │ 2_421_140.67w │    100.00% │
   │ [test_highlight.ml] shallow text partial highlight:0         │         233.53ns │         125.00w │               │               │            │
   │ [test_highlight.ml] shallow text partial highlight:100       │     165_465.31ns │     240_549.00w │     1_263.76w │       436.76w │      0.12% │
   │ [test_highlight.ml] shallow text partial highlight:200       │     341_721.58ns │     480_978.00w │     3_368.30w │     1_716.30w │      0.26% │
   │ [test_highlight.ml] shallow text partial highlight:400       │     730_494.18ns │     961_833.00w │    10_148.27w │     6_846.27w │      0.55% │
   │ [test_highlight.ml] shallow text partial highlight:1000      │   1_987_880.02ns │   2_400_748.00w │    36_771.65w │    28_530.65w │      1.49% │
   │ [test_highlight.ml] shallow text partial highlight:5000      │  10_325_748.27ns │  11_995_948.00w │   215_021.09w │   173_850.09w │      7.75% │
   │ [test_highlight.ml] shallow text partial highlight:10000     │  20_870_310.40ns │  23_988_035.00w │   438_065.73w │   355_736.73w │     15.66% │
   │ [test_highlight.ml] shallow text partial highlight:20000     │  40_626_398.13ns │  47_969_014.00w │   720_315.75w │   720_315.75w │     30.49% │
   │ [test_highlight.ml] deep text partial highlight:0            │         233.49ns │         125.00w │               │               │            │
   │ [test_highlight.ml] deep text partial highlight:100          │     108_299.46ns │     156_701.00w │       800.07w │       196.07w │      0.08% │
   │ [test_highlight.ml] deep text partial highlight:200          │     224_442.20ns │     316_056.00w │     1_988.86w │       770.86w │      0.17% │
   │ [test_highlight.ml] deep text partial highlight:400          │     468_958.74ns │     632_937.00w │     5_463.98w │     3_025.98w │      0.35% │
   │ [test_highlight.ml] deep text partial highlight:1000         │   1_293_376.28ns │   1_578_776.00w │    22_691.22w │    16_609.22w │      0.97% │
   │ [test_highlight.ml] deep text partial highlight:5000         │   6_903_022.05ns │   7_897_892.00w │   145_611.36w │   115_191.36w │      5.18% │
   │ [test_highlight.ml] deep text partial highlight:10000        │  13_840_533.07ns │  15_796_005.00w │   299_704.27w │   238_863.27w │     10.39% │
   │ [test_highlight.ml] deep text partial highlight:20000        │  27_276_801.66ns │  31_574_252.00w │   486_097.67w │   486_097.67w │     20.47% │
   │ [test_highlight.ml] shallow viewport highlight at line:0     │     166_877.05ns │     240_311.00w │     1_258.50w │       431.50w │      0.13% │
   │ [test_highlight.ml] shallow viewport highlight at line:5000  │     166_809.12ns │     240_493.00w │     1_262.66w │       435.66w │      0.13% │
   │ [test_highlight.ml] shallow viewport highlight at line:10000 │     167_034.99ns │     240_515.00w │     1_259.61w │       432.61w │      0.13% │
   │ [test_highlight.ml] shallow viewport highlight at line:15000 │     166_617.43ns │     240_549.00w │     1_262.38w │       435.38w │      0.13% │
   │ [test_highlight.ml] shallow viewport highlight at line:20000 │     166_571.04ns │     240_549.00w │     1_261.23w │       434.23w │      0.13% │
   │ [test_highlight.ml] deep viewport highlight at line:0        │     111_130.24ns │     159_597.00w │       810.75w │       194.75w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:20000    │     109_216.64ns │     157_703.00w │       802.83w │       193.83w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:40000    │     109_335.36ns │     157_677.00w │       803.51w │       194.51w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:60000    │     107_097.41ns │     155_369.00w │       786.54w │       186.54w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:80000    │     110_349.38ns │     159_543.00w │       813.55w │       198.55w │      0.08% │
   └──────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴───────────────┴───────────────┴────────────┘
*)

let run_highlight ?(checkpoints = Checkpoints.empty) test_string () =
  Symbolic_automaton_based.highlight
    (module String)
    test_string
    ~checkpoints
    ~combine:(fun ~index ~nesting ~decorations -> (index, nesting) :: decorations)
;;

let slice_lines text ~from ~to_ =
  String.split_lines text |> List.sub ~pos:from ~len:(to_ - from) |> String.concat_lines
;;

let run_partial_highlight_on_lines test_string ~checkpoints_until ~parse_until =
  let res = run_highlight (slice_lines test_string ~from:0 ~to_:checkpoints_until) () in
  let res_checkpoints =
    Symbolic_automaton_based.Highlight_result.get_all_checkpoints res
  in
  let test_slice = slice_lines test_string ~from:0 ~to_:parse_until in
  run_highlight test_slice ~checkpoints:res_checkpoints
;;

let%bench_fun ("shallow text full highlight" [@indexed width = [ 1; 4; 16; 64; 256 ]]) =
  Rose_tree.generate ~depth:2 ~width |> Rose_tree.pp_tree |> run_highlight
;;

let%bench_fun ("deep text full highlight" [@indexed depth = [ 1; 4; 16 ]]) =
  Rose_tree.generate ~depth ~width:2 |> Rose_tree.pp_tree |> run_highlight
;;

(* These benchmarks simulate decorating a fixed 200-line viewport with variable
   amounts of existing checkpoints (e.g. no checkpoints, checkpoints through the
   middle of the viewport, checkpoints past the viewport, etc). *)
let%bench_fun ("shallow text partial highlight" [@indexed
                                                  num_lines_to_reparse
                                                  = [ 0
                                                    ; 100
                                                    ; 200
                                                    ; 400
                                                    ; 1000
                                                    ; 5000
                                                    ; 10000
                                                    ; 20000
                                                    ]])
  =
  Rose_tree.generate ~depth:2 ~width:256
  |> Rose_tree.pp_tree
  |> run_partial_highlight_on_lines
       ~checkpoints_until:(20000 - num_lines_to_reparse)
       ~parse_until:20000
;;

let%bench_fun ("deep text partial highlight" [@indexed
                                               num_lines_to_reparse
                                               = [ 0
                                                 ; 100
                                                 ; 200
                                                 ; 400
                                                 ; 1000
                                                 ; 5000
                                                 ; 10000
                                                 ; 20000
                                                 ]])
  =
  Rose_tree.generate ~depth:16 ~width:2
  |> Rose_tree.pp_tree
  |> run_partial_highlight_on_lines
       ~checkpoints_until:(20000 - num_lines_to_reparse)
       ~parse_until:20000
;;

(* These benchmarks simulate editing in the viewport at various line locations
   in the text. It's assumed that you've generated checkpoints through the middle of the
   viewport. *)
let%bench_fun ("shallow viewport highlight at line" [@indexed
                                                      viewport_start
                                                      = [ 0; 5000; 10000; 15000; 20000 ]])
  =
  Rose_tree.generate ~depth:2 ~width:256
  |> Rose_tree.pp_tree
  |> run_partial_highlight_on_lines
       ~checkpoints_until:(viewport_start + 100)
       ~parse_until:(viewport_start + 200)
;;

let%bench_fun ("deep viewport highlight at line" [@indexed
                                                   viewport_start
                                                   = [ 0; 20000; 40000; 60000; 80000 ]])
  =
  Rose_tree.generate ~depth:16 ~width:2
  |> Rose_tree.pp_tree
  |> run_partial_highlight_on_lines
       ~checkpoints_until:(viewport_start + 100)
       ~parse_until:(viewport_start + 200)
;;
