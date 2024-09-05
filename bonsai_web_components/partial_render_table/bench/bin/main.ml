open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_bench
open! Bonsai_web_ui_partial_render_table_test.Shared_with_bench
open! Incr_map_collate

let () = print_endline "======== Startup Benchmarking ========"

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  let inputs =
    List.map [ 0; 1; 10; 100; 101; 1_000; 10_000; 100_000; 1_000_000 ] ~f:(fun n ->
      let input = Prt_input.create (Row.init_rows n) in
      Int.to_string n, input)
  in
  Bonsai_bench.benchmark_compare_startup
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    (module Config)
    ~inputs
    ~configs:Config.all
;;

let () = print_endline "======== Performance Benchmarking ========"

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark_compare_interactions
    ~print_separate_rows:true
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    (module Config)
    ~scenarios
    ~configs:Config.all
;;
