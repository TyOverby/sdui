open! Core
open Shared_with_bench
module Report = Bonsai_web_test.Computation_report

let%test_module "startup" =
  (module struct
    let test_startup configs =
      let startup_inputs =
        List.map [ 100; 100_000 ] ~f:(fun size ->
          Int.to_string size, Prt_input.create (Row.init_rows size))
      in
      Report.Startup.run_and_print_compare (module Config) startup_inputs configs
    ;;

    let%expect_test "Flat" =
      test_startup Config.all_flat;
      [%expect
        {|
        ======= Startup Incr Node Stats =======
        ┌─────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
        │                                     │ max_height │ node_count │ max_node_id │ nodes_created │
        ├─────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
        │ dyn cells (stateless)(flat): 100    │  87        │  1085      │  1521       │  1522         │
        │ dyn cells (state)(flat): 100        │ 103        │ 12334      │ 15591       │ 15592         │
        │ dyn cols(flat): 100                 │  71        │  1057      │  1493       │  1494         │
        │ dyn-exp (stateless): 100            │  90        │  5256      │  6997       │  6998         │
        │ dyn-exp(state): 100                 │  96        │  8158      │ 10497       │ 10498         │
        │ dyn cells (stateless)(flat): 100000 │  87        │  1093      │  1533       │  1534         │
        │ dyn cells (state)(flat): 100000     │ 103        │ 12454      │ 15743       │ 15744         │
        │ dyn cols(flat): 100000              │  71        │  1065      │  1505       │  1506         │
        │ dyn-exp (stateless): 100000         │  90        │  5306      │  7064       │  7065         │
        │ dyn-exp(state): 100000              │  96        │  8237      │ 10599       │ 10600         │
        └─────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘

        ======= Startup Incr Annotated Node Counts =======
        ┌────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬─────────────────────┐
        │                            │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle_apply_act │
        │                            │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ ion_pair            │
        ├────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼─────────────────────┤
        │ dyn cells                  │ 9     │  617  │  1150  │ 21        │ 103       │   21  │ 100       │   3       │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                   │
        │ (stateless)(flat): 100     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
        │ dyn cells (state)(flat):   │ 9     │ 4817  │ 10250  │ 21        │ 803       │ 1428  │ 100       │   3       │ 800       │ 800       │ 8         │ 8         │ 1         │ 1    │ 0                   │
        │ 100                        │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
        │ dyn cols(flat): 100        │ 8     │  598  │  1112  │ 22        │ 103       │   21  │ 100       │   3       │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                   │
        │ dyn-exp (stateless): 100   │ 8     │ 3597  │  6910  │ 21        │ 303       │  122  │ 100       │ 103       │ 200       │ 200       │ 2         │ 2         │ 1         │ 1    │ 0                   │
        │ dyn-exp(state): 100        │ 8     │ 4997  │ 11110  │ 21        │ 303       │ 1522  │ 100       │ 103       │ 200       │ 200       │ 2         │ 2         │ 1         │ 1    │ 0                   │
        │ dyn cells                  │ 9     │  622  │  1159  │ 21        │ 104       │   21  │ 101       │   3       │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                   │
        │ (stateless)(flat): 100000  │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
        │ dyn cells (state)(flat):   │ 9     │ 4864  │ 10350  │ 21        │ 811       │ 1442  │ 101       │   3       │ 808       │ 808       │ 8         │ 8         │ 1         │ 1    │ 0                   │
        │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
        │ dyn cols(flat): 100000     │ 8     │  603  │  1121  │ 22        │ 104       │   21  │ 101       │   3       │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                   │
        │ dyn-exp (stateless):       │ 8     │ 3632  │  6977  │ 21        │ 306       │  123  │ 101       │ 104       │ 202       │ 202       │ 2         │ 2         │ 1         │ 1    │ 0                   │
        │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
        │ dyn-exp(state): 100000     │ 8     │ 5046  │ 11219  │ 21        │ 306       │ 1537  │ 101       │ 104       │ 202       │ 202       │ 2         │ 2         │ 1         │ 1    │ 0                   │
        └────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴─────────────────────┘

        ======= Bonsai Computation Nodes =======
        ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
        │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ monito │
        │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ r_free │
        │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ _varia │
        │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ bles   │
        ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
        │ dyn cells           │ 104    │ 0      │ 1     │  8    │ 9      │ 0      │ 130 │ 0     │ 1     │ 1     │ 0      │ 7      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (stateless)(flat):  │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ 100                 │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cells           │ 146    │ 0      │ 1     │ 15    │ 9      │ 0      │ 172 │ 0     │ 1     │ 1     │ 7      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (state)(flat): 100  │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cols(flat): 100 │  98    │ 0      │ 1     │  8    │ 3      │ 0      │ 111 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ dyn-exp             │ 127    │ 0      │ 1     │  8    │ 2      │ 0      │ 138 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (stateless): 100    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn-exp(state): 100 │ 141    │ 0      │ 1     │ 15    │ 2      │ 0      │ 159 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ dyn cells           │ 104    │ 0      │ 1     │  8    │ 9      │ 0      │ 130 │ 0     │ 1     │ 1     │ 0      │ 7      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (stateless)(flat):  │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cells           │ 146    │ 0      │ 1     │ 15    │ 9      │ 0      │ 172 │ 0     │ 1     │ 1     │ 7      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (state)(flat):      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cols(flat):     │  98    │ 0      │ 1     │  8    │ 3      │ 0      │ 111 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn-exp             │ 127    │ 0      │ 1     │  8    │ 2      │ 0      │ 138 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (stateless): 100000 │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn-exp(state):     │ 141    │ 0      │ 1     │ 15    │ 2      │ 0      │ 159 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

        ======= Bonsai Value Nodes =======
        ┌─────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
        │                                     │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
        ├─────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
        │ dyn cells (stateless)(flat): 100    │ 7        │ 0          │ 3    │ 206   │ 18     │ 110  │
        │ dyn cells (state)(flat): 100        │ 7        │ 0          │ 3    │ 262   │ 18     │ 152  │
        │ dyn cols(flat): 100                 │ 7        │ 0          │ 3    │ 181   │ 18     │ 104  │
        │ dyn-exp (stateless): 100            │ 7        │ 1          │ 3    │ 217   │ 18     │ 132  │
        │ dyn-exp(state): 100                 │ 7        │ 1          │ 3    │ 238   │ 18     │ 146  │
        │ dyn cells (stateless)(flat): 100000 │ 7        │ 0          │ 3    │ 206   │ 18     │ 110  │
        │ dyn cells (state)(flat): 100000     │ 7        │ 0          │ 3    │ 262   │ 18     │ 152  │
        │ dyn cols(flat): 100000              │ 7        │ 0          │ 3    │ 181   │ 18     │ 104  │
        │ dyn-exp (stateless): 100000         │ 7        │ 1          │ 3    │ 217   │ 18     │ 132  │
        │ dyn-exp(state): 100000              │ 7        │ 1          │ 3    │ 238   │ 18     │ 146  │
        └─────────────────────────────────────┴──────────┴────────────┴──────┴───────┴────────┴──────┘
        |}]
    ;;

    let%expect_test "Grouped" =
      test_startup Config.all_grouped;
      [%expect
        {|
        ======= Startup Incr Node Stats =======
        ┌────────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
        │                                        │ max_height │ node_count │ max_node_id │ nodes_created │
        ├────────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
        │ dyn cells (stateless) (groups): 100    │ 75         │ 1065       │ 1501        │ 1502          │
        │ dyn cells (state) (groups): 100        │ 91         │ 4279       │ 5521        │ 5522          │
        │ dyn cols (groups): 100                 │ 71         │ 1057       │ 1493        │ 1494          │
        │ dyn cells (stateless) (groups): 100000 │ 75         │ 1073       │ 1513        │ 1514          │
        │ dyn cells (state) (groups): 100000     │ 91         │ 4319       │ 5573        │ 5574          │
        │ dyn cols (groups): 100000              │ 71         │ 1065       │ 1505        │ 1506          │
        └────────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘

        ======= Startup Incr Annotated Node Counts =======
        ┌──────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬───────────────────┐
        │                              │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle_apply_a │
        │                              │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ ction_pair        │
        ├──────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼───────────────────┤
        │ dyn cells (stateless)        │ 9     │  605  │ 1126   │ 20        │ 103       │  21   │ 100       │ 3         │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                 │
        │ (groups): 100                │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
        │ dyn cells (state) (groups):  │ 9     │ 1805  │ 3726   │ 20        │ 303       │ 423   │ 100       │ 3         │ 300       │ 300       │ 3         │ 3         │ 1         │ 1    │ 0                 │
        │ 100                          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
        │ dyn cols (groups): 100       │ 8     │  598  │ 1112   │ 22        │ 103       │  21   │ 100       │ 3         │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                 │
        │ dyn cells (stateless)        │ 9     │  610  │ 1135   │ 20        │ 104       │  21   │ 101       │ 3         │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                 │
        │ (groups): 100000             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
        │ dyn cells (state) (groups):  │ 9     │ 1822  │ 3761   │ 20        │ 306       │ 427   │ 101       │ 3         │ 303       │ 303       │ 3         │ 3         │ 1         │ 1    │ 0                 │
        │ 100000                       │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
        │ dyn cols (groups): 100000    │ 8     │  603  │ 1121   │ 22        │ 104       │  21   │ 101       │ 3         │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                 │
        └──────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴───────────────────┘

        ======= Bonsai Computation Nodes =======
        ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
        │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ monito │
        │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ r_free │
        │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ _varia │
        │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ bles   │
        ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
        │ dyn cells           │ 100    │ 0      │ 1     │  8    │ 6      │ 0      │ 118 │ 0     │ 1     │ 1     │ 0      │ 2      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (stateless)         │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cells (state)   │ 112    │ 0      │ 1     │ 10    │ 6      │ 0      │ 130 │ 0     │ 1     │ 1     │ 2      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cols (groups):  │  98    │ 0      │ 1     │  8    │ 3      │ 0      │ 111 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ 100                 │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cells           │ 100    │ 0      │ 1     │  8    │ 6      │ 0      │ 118 │ 0     │ 1     │ 1     │ 0      │ 2      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (stateless)         │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cells (state)   │ 112    │ 0      │ 1     │ 10    │ 6      │ 0      │ 130 │ 0     │ 1     │ 1     │ 2      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        │ dyn cols (groups):  │  98    │ 0      │ 1     │  8    │ 3      │ 0      │ 111 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
        │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
        └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

        ======= Bonsai Value Nodes =======
        ┌────────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
        │                                        │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
        ├────────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
        │ dyn cells (stateless) (groups): 100    │ 7        │ 0          │ 3    │ 190   │ 18     │ 106  │
        │ dyn cells (state) (groups): 100        │ 7        │ 0          │ 3    │ 206   │ 18     │ 118  │
        │ dyn cols (groups): 100                 │ 7        │ 0          │ 3    │ 181   │ 18     │ 104  │
        │ dyn cells (stateless) (groups): 100000 │ 7        │ 0          │ 3    │ 190   │ 18     │ 106  │
        │ dyn cells (state) (groups): 100000     │ 7        │ 0          │ 3    │ 206   │ 18     │ 118  │
        │ dyn cols (groups): 100000              │ 7        │ 0          │ 3    │ 181   │ 18     │ 104  │
        └────────────────────────────────────────┴──────────┴────────────┴──────┴───────┴────────┴──────┘
        |}]
    ;;
  end)
;;

let test_interaction configs =
  Report.Interaction.run_and_print_compare (module Config) scenarios configs
;;

let%test_module "interactions" =
  (module struct
    let%expect_test "Flat" =
      test_interaction Config.all_flat;
      [%expect
        {|
        ======= Max Height =======
        ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
        │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
        ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key not present) and unfocus in 100 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key not present) and unfocus in 101 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key not present) and unfocus in 1000 element map          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key not present) and unfocus in 10000 element map         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key present) and unfocus in 10 element map                │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key present) and unfocus in 100 element map               │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key present) and unfocus in 101 element map               │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key present) and unfocus in 1000 element map              │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus by key (key present) and unfocus in 10000 element map             │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus up and down in 10 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus up and down in 100 element map                                    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus up and down in 101 element map                                    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus up and down in 1000 element map                                   │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus up and down in 10000 element map                                  │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus left and right in a map with 10 rows                              │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus left and right in a map with 100 rows                             │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus left and right in a map with 101 rows                             │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus left and right in a map with 1000 rows                            │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Focus left and right in a map with 10000 rows                           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Page up and down in 10 element map                                      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Page up and down in 100 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Page up and down in 101 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Page up and down in 1000 element map                                    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Page up and down in 10000 element map                                   │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Apply 4 filters and clear with 100 element map using 10 window          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Apply 4 filters and clear with 101 element map using 10 window          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Invert ordering of 10 element map                                       │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Invert ordering of 100 element map                                      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Invert ordering of 101 element map                                      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Invert ordering of 1000 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
        └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘

        ======= Node Count =======
        ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
        │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
        ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Focus by key (key not present) and unfocus in 100 element map           │ 1085                        │ 12334                   │ 1057           │ 5256                │ 8158           │
        │ Focus by key (key not present) and unfocus in 101 element map           │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus by key (key not present) and unfocus in 1000 element map          │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus by key (key not present) and unfocus in 10000 element map         │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus by key (key present) and unfocus in 10 element map                │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Focus by key (key present) and unfocus in 100 element map               │ 1085                        │ 12334                   │ 1057           │ 5256                │ 8158           │
        │ Focus by key (key present) and unfocus in 101 element map               │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus by key (key present) and unfocus in 1000 element map              │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus by key (key present) and unfocus in 10000 element map             │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus up and down in 10 element map                                     │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Focus up and down in 100 element map                                    │ 1085                        │ 12334                   │ 1057           │ 5256                │ 8158           │
        │ Focus up and down in 101 element map                                    │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus up and down in 1000 element map                                   │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus up and down in 10000 element map                                  │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus left and right in a map with 10 rows                              │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Focus left and right in a map with 100 rows                             │ 1085                        │ 12334                   │ 1057           │ 5256                │ 8158           │
        │ Focus left and right in a map with 101 rows                             │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus left and right in a map with 1000 rows                            │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Focus left and right in a map with 10000 rows                           │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Page up and down in 10 element map                                      │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Page up and down in 100 element map                                     │ 1085                        │ 12334                   │ 1057           │ 5256                │ 8158           │
        │ Page up and down in 101 element map                                     │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Page up and down in 1000 element map                                    │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Page up and down in 10000 element map                                   │ 1093                        │ 12454                   │ 1065           │ 5306                │ 8237           │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  297                        │   458                   │  269           │  310                │  341           │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │  369                        │  1538                   │  341           │  760                │ 1052           │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  297                        │   458                   │  269           │  310                │  341           │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │  369                        │  1538                   │  341           │  760                │ 1052           │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1089                        │ 12338                   │ 1061           │ 5260                │ 8162           │
        │ Apply 4 filters and clear with 100 element map using 10 window          │  366                        │  1535                   │  338           │  757                │ 1049           │
        │ Apply 4 filters and clear with 101 element map using 10 window          │  366                        │  1535                   │  338           │  757                │ 1049           │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │  366                        │  1535                   │  338           │  757                │ 1049           │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │  686                        │  6335                   │  658           │ 2757                │ 4209           │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │  686                        │  6335                   │  658           │ 2757                │ 4209           │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1086                        │ 12335                   │ 1058           │ 5257                │ 8159           │
        │ Invert ordering of 10 element map                                       │  366                        │  1535                   │  338           │  757                │ 1049           │
        │ Invert ordering of 100 element map                                      │ 1086                        │ 12335                   │ 1058           │ 5257                │ 8159           │
        │ Invert ordering of 101 element map                                      │ 1094                        │ 12455                   │ 1066           │ 5307                │ 8238           │
        │ Invert ordering of 1000 element map                                     │ 1094                        │ 12455                   │ 1066           │ 5307                │ 8238           │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  365                        │  1534                   │  337           │  756                │ 1048           │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1085                        │ 12334                   │ 1057           │ 5256                │ 8158           │
        └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘

        ======= Max Node ID =======
        ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
        │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
        ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Focus by key (key not present) and unfocus in 100 element map           │ 1521                        │ 15591                   │ 1493           │  6997               │ 10497          │
        │ Focus by key (key not present) and unfocus in 101 element map           │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus by key (key not present) and unfocus in 1000 element map          │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus by key (key not present) and unfocus in 10000 element map         │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus by key (key present) and unfocus in 10 element map                │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Focus by key (key present) and unfocus in 100 element map               │ 1521                        │ 15591                   │ 1493           │  6997               │ 10497          │
        │ Focus by key (key present) and unfocus in 101 element map               │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus by key (key present) and unfocus in 1000 element map              │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus by key (key present) and unfocus in 10000 element map             │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus up and down in 10 element map                                     │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Focus up and down in 100 element map                                    │ 1521                        │ 15591                   │ 1493           │  6997               │ 10497          │
        │ Focus up and down in 101 element map                                    │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus up and down in 1000 element map                                   │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus up and down in 10000 element map                                  │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus left and right in a map with 10 rows                              │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Focus left and right in a map with 100 rows                             │ 1521                        │ 15591                   │ 1493           │  6997               │ 10497          │
        │ Focus left and right in a map with 101 rows                             │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus left and right in a map with 1000 rows                            │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Focus left and right in a map with 10000 rows                           │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Page up and down in 10 element map                                      │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Page up and down in 100 element map                                     │ 1521                        │ 15591                   │ 1493           │  6997               │ 10497          │
        │ Page up and down in 101 element map                                     │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Page up and down in 1000 element map                                    │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Page up and down in 10000 element map                                   │ 1533                        │ 15743                   │ 1505           │  7064               │ 10599          │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 1540                        │ 16954                   │ 1512           │  7768               │ 11828          │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 1730                        │ 18040                   │ 1702           │  8086               │ 12146          │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 1552                        │ 17106                   │ 1524           │  7835               │ 11930          │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 1742                        │ 18192                   │ 1714           │  8153               │ 12248          │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1742                        │ 18192                   │ 1714           │  8153               │ 12248          │
        │ Apply 4 filters and clear with 100 element map using 10 window          │  631                        │  5125                   │  603           │  2849               │  4459          │
        │ Apply 4 filters and clear with 101 element map using 10 window          │  631                        │  5125                   │  603           │  2849               │  4459          │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │  631                        │  5125                   │  603           │  2849               │  4459          │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │ 1111                        │ 24645                   │ 1083           │ 13049               │ 21659          │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │ 1111                        │ 24645                   │ 1083           │ 13049               │ 21659          │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1711                        │ 49045                   │ 1683           │ 25799               │ 43159          │
        │ Invert ordering of 10 element map                                       │  492                        │  1962                   │  464           │  1018               │  1368          │
        │ Invert ordering of 100 element map                                      │ 1572                        │ 15642                   │ 1544           │  7048               │ 10548          │
        │ Invert ordering of 101 element map                                      │ 1584                        │ 15794                   │ 1556           │  7115               │ 10650          │
        │ Invert ordering of 1000 element map                                     │ 1584                        │ 15794                   │ 1556           │  7115               │ 10650          │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  441                        │  1911                   │  413           │   967               │  1317          │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1521                        │ 15591                   │ 1493           │  6997               │ 10497          │
        └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘

        ======= Nodes Created =======
        ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
        │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
        ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key not present) and unfocus in 100 element map           │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key not present) and unfocus in 101 element map           │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key not present) and unfocus in 1000 element map          │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key not present) and unfocus in 10000 element map         │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key present) and unfocus in 10 element map                │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key present) and unfocus in 100 element map               │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key present) and unfocus in 101 element map               │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key present) and unfocus in 1000 element map              │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus by key (key present) and unfocus in 10000 element map             │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus up and down in 10 element map                                     │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus up and down in 100 element map                                    │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus up and down in 101 element map                                    │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus up and down in 1000 element map                                   │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus up and down in 10000 element map                                  │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus left and right in a map with 10 rows                              │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus left and right in a map with 100 rows                             │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus left and right in a map with 101 rows                             │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus left and right in a map with 1000 rows                            │   0                         │     0                   │   0            │     0               │     0          │
        │ Focus left and right in a map with 10000 rows                           │   0                         │     0                   │   0            │     0               │     0          │
        │ Page up and down in 10 element map                                      │   0                         │     0                   │   0            │     0               │     0          │
        │ Page up and down in 100 element map                                     │   0                         │     0                   │   0            │     0               │     0          │
        │ Page up and down in 101 element map                                     │   0                         │     0                   │   0            │     0               │     0          │
        │ Page up and down in 1000 element map                                    │   0                         │     0                   │   0            │     0               │     0          │
        │ Page up and down in 10000 element map                                   │   0                         │     0                   │   0            │     0               │     0          │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  17                         │  1361                   │  17            │   769               │  1329          │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 209                         │  2449                   │ 209            │  1089               │  1649          │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  17                         │  1361                   │  17            │   769               │  1329          │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 209                         │  2449                   │ 209            │  1089               │  1649          │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 209                         │  2449                   │ 209            │  1089               │  1649          │
        │ Apply 4 filters and clear with 100 element map using 10 window          │ 188                         │  3212                   │ 188            │  1880               │  3140          │
        │ Apply 4 filters and clear with 101 element map using 10 window          │ 188                         │  3212                   │ 188            │  1880               │  3140          │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │ 188                         │  3212                   │ 188            │  1880               │  3140          │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │ 188                         │ 16652                   │ 188            │  9400               │ 16260          │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │ 188                         │ 16652                   │ 188            │  9400               │ 16260          │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 188                         │ 33452                   │ 188            │ 18800               │ 32660          │
        │ Invert ordering of 10 element map                                       │  49                         │    49                   │  49            │    49               │    49          │
        │ Invert ordering of 100 element map                                      │  49                         │    49                   │  49            │    49               │    49          │
        │ Invert ordering of 101 element map                                      │  49                         │    49                   │  49            │    49               │    49          │
        │ Invert ordering of 1000 element map                                     │  49                         │    49                   │  49            │    49               │    49          │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │   0                         │     0                   │   0            │     0               │     0          │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │   0                         │     0                   │   0            │     0               │     0          │
        └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘
        |}]
    ;;

    let%expect_test "Grouped" =
      test_interaction Config.all_grouped;
      [%expect
        {|
        ======= Max Height =======
        ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
        │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
        ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │ 75                             │ 91                         │ 71                │
        │ Focus by key (key not present) and unfocus in 100 element map           │ 75                             │ 91                         │ 71                │
        │ Focus by key (key not present) and unfocus in 101 element map           │ 75                             │ 91                         │ 71                │
        │ Focus by key (key not present) and unfocus in 1000 element map          │ 75                             │ 91                         │ 71                │
        │ Focus by key (key not present) and unfocus in 10000 element map         │ 75                             │ 91                         │ 71                │
        │ Focus by key (key present) and unfocus in 10 element map                │ 75                             │ 91                         │ 71                │
        │ Focus by key (key present) and unfocus in 100 element map               │ 75                             │ 91                         │ 71                │
        │ Focus by key (key present) and unfocus in 101 element map               │ 75                             │ 91                         │ 71                │
        │ Focus by key (key present) and unfocus in 1000 element map              │ 75                             │ 91                         │ 71                │
        │ Focus by key (key present) and unfocus in 10000 element map             │ 75                             │ 91                         │ 71                │
        │ Focus up and down in 10 element map                                     │ 75                             │ 91                         │ 71                │
        │ Focus up and down in 100 element map                                    │ 75                             │ 91                         │ 71                │
        │ Focus up and down in 101 element map                                    │ 75                             │ 91                         │ 71                │
        │ Focus up and down in 1000 element map                                   │ 75                             │ 91                         │ 71                │
        │ Focus up and down in 10000 element map                                  │ 75                             │ 91                         │ 71                │
        │ Focus left and right in a map with 10 rows                              │ 75                             │ 91                         │ 71                │
        │ Focus left and right in a map with 100 rows                             │ 75                             │ 91                         │ 71                │
        │ Focus left and right in a map with 101 rows                             │ 75                             │ 91                         │ 71                │
        │ Focus left and right in a map with 1000 rows                            │ 75                             │ 91                         │ 71                │
        │ Focus left and right in a map with 10000 rows                           │ 75                             │ 91                         │ 71                │
        │ Page up and down in 10 element map                                      │ 75                             │ 91                         │ 71                │
        │ Page up and down in 100 element map                                     │ 75                             │ 91                         │ 71                │
        │ Page up and down in 101 element map                                     │ 75                             │ 91                         │ 71                │
        │ Page up and down in 1000 element map                                    │ 75                             │ 91                         │ 71                │
        │ Page up and down in 10000 element map                                   │ 75                             │ 91                         │ 71                │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 75                             │ 91                         │ 71                │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 75                             │ 91                         │ 71                │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 75                             │ 91                         │ 71                │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 75                             │ 91                         │ 71                │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 75                             │ 91                         │ 71                │
        │ Apply 4 filters and clear with 100 element map using 10 window          │ 75                             │ 91                         │ 71                │
        │ Apply 4 filters and clear with 101 element map using 10 window          │ 75                             │ 91                         │ 71                │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │ 75                             │ 91                         │ 71                │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │ 75                             │ 91                         │ 71                │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │ 75                             │ 91                         │ 71                │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 75                             │ 91                         │ 71                │
        │ Invert ordering of 10 element map                                       │ 75                             │ 91                         │ 71                │
        │ Invert ordering of 100 element map                                      │ 75                             │ 91                         │ 71                │
        │ Invert ordering of 101 element map                                      │ 75                             │ 91                         │ 71                │
        │ Invert ordering of 1000 element map                                     │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ 75                             │ 91                         │ 71                │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 75                             │ 91                         │ 71                │
        └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘

        ======= Node Count =======
        ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
        │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
        ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │  345                           │  679                       │  337              │
        │ Focus by key (key not present) and unfocus in 100 element map           │ 1065                           │ 4279                       │ 1057              │
        │ Focus by key (key not present) and unfocus in 101 element map           │ 1073                           │ 4319                       │ 1065              │
        │ Focus by key (key not present) and unfocus in 1000 element map          │ 1073                           │ 4319                       │ 1065              │
        │ Focus by key (key not present) and unfocus in 10000 element map         │ 1073                           │ 4319                       │ 1065              │
        │ Focus by key (key present) and unfocus in 10 element map                │  345                           │  679                       │  337              │
        │ Focus by key (key present) and unfocus in 100 element map               │ 1065                           │ 4279                       │ 1057              │
        │ Focus by key (key present) and unfocus in 101 element map               │ 1073                           │ 4319                       │ 1065              │
        │ Focus by key (key present) and unfocus in 1000 element map              │ 1073                           │ 4319                       │ 1065              │
        │ Focus by key (key present) and unfocus in 10000 element map             │ 1073                           │ 4319                       │ 1065              │
        │ Focus up and down in 10 element map                                     │  345                           │  679                       │  337              │
        │ Focus up and down in 100 element map                                    │ 1065                           │ 4279                       │ 1057              │
        │ Focus up and down in 101 element map                                    │ 1073                           │ 4319                       │ 1065              │
        │ Focus up and down in 1000 element map                                   │ 1073                           │ 4319                       │ 1065              │
        │ Focus up and down in 10000 element map                                  │ 1073                           │ 4319                       │ 1065              │
        │ Focus left and right in a map with 10 rows                              │  345                           │  679                       │  337              │
        │ Focus left and right in a map with 100 rows                             │ 1065                           │ 4279                       │ 1057              │
        │ Focus left and right in a map with 101 rows                             │ 1073                           │ 4319                       │ 1065              │
        │ Focus left and right in a map with 1000 rows                            │ 1073                           │ 4319                       │ 1065              │
        │ Focus left and right in a map with 10000 rows                           │ 1073                           │ 4319                       │ 1065              │
        │ Page up and down in 10 element map                                      │  345                           │  679                       │  337              │
        │ Page up and down in 100 element map                                     │ 1065                           │ 4279                       │ 1057              │
        │ Page up and down in 101 element map                                     │ 1073                           │ 4319                       │ 1065              │
        │ Page up and down in 1000 element map                                    │ 1073                           │ 4319                       │ 1065              │
        │ Page up and down in 10000 element map                                   │ 1073                           │ 4319                       │ 1065              │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  277                           │  323                       │  269              │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │  349                           │  683                       │  341              │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  277                           │  323                       │  269              │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │  349                           │  683                       │  341              │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1069                           │ 4283                       │ 1061              │
        │ Apply 4 filters and clear with 100 element map using 10 window          │  346                           │  680                       │  338              │
        │ Apply 4 filters and clear with 101 element map using 10 window          │  346                           │  680                       │  338              │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │  346                           │  680                       │  338              │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │  666                           │ 2280                       │  658              │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │  666                           │ 2280                       │  658              │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1066                           │ 4280                       │ 1058              │
        │ Invert ordering of 10 element map                                       │  346                           │  680                       │  338              │
        │ Invert ordering of 100 element map                                      │ 1066                           │ 4280                       │ 1058              │
        │ Invert ordering of 101 element map                                      │ 1074                           │ 4320                       │ 1066              │
        │ Invert ordering of 1000 element map                                     │ 1074                           │ 4320                       │ 1066              │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  345                           │  679                       │  337              │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  345                           │  679                       │  337              │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  345                           │  679                       │  337              │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  345                           │  679                       │  337              │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  345                           │  679                       │  337              │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  345                           │  679                       │  337              │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  345                           │  679                       │  337              │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  345                           │  679                       │  337              │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1065                           │ 4279                       │ 1057              │
        └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘

        ======= Max Node ID =======
        ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
        │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
        ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │  421                           │   841                      │  413              │
        │ Focus by key (key not present) and unfocus in 100 element map           │ 1501                           │  5521                      │ 1493              │
        │ Focus by key (key not present) and unfocus in 101 element map           │ 1513                           │  5573                      │ 1505              │
        │ Focus by key (key not present) and unfocus in 1000 element map          │ 1513                           │  5573                      │ 1505              │
        │ Focus by key (key not present) and unfocus in 10000 element map         │ 1513                           │  5573                      │ 1505              │
        │ Focus by key (key present) and unfocus in 10 element map                │  421                           │   841                      │  413              │
        │ Focus by key (key present) and unfocus in 100 element map               │ 1501                           │  5521                      │ 1493              │
        │ Focus by key (key present) and unfocus in 101 element map               │ 1513                           │  5573                      │ 1505              │
        │ Focus by key (key present) and unfocus in 1000 element map              │ 1513                           │  5573                      │ 1505              │
        │ Focus by key (key present) and unfocus in 10000 element map             │ 1513                           │  5573                      │ 1505              │
        │ Focus up and down in 10 element map                                     │  421                           │   841                      │  413              │
        │ Focus up and down in 100 element map                                    │ 1501                           │  5521                      │ 1493              │
        │ Focus up and down in 101 element map                                    │ 1513                           │  5573                      │ 1505              │
        │ Focus up and down in 1000 element map                                   │ 1513                           │  5573                      │ 1505              │
        │ Focus up and down in 10000 element map                                  │ 1513                           │  5573                      │ 1505              │
        │ Focus left and right in a map with 10 rows                              │  421                           │   841                      │  413              │
        │ Focus left and right in a map with 100 rows                             │ 1501                           │  5521                      │ 1493              │
        │ Focus left and right in a map with 101 rows                             │ 1513                           │  5573                      │ 1505              │
        │ Focus left and right in a map with 1000 rows                            │ 1513                           │  5573                      │ 1505              │
        │ Focus left and right in a map with 10000 rows                           │ 1513                           │  5573                      │ 1505              │
        │ Page up and down in 10 element map                                      │  421                           │   841                      │  413              │
        │ Page up and down in 100 element map                                     │ 1501                           │  5521                      │ 1493              │
        │ Page up and down in 101 element map                                     │ 1513                           │  5573                      │ 1505              │
        │ Page up and down in 1000 element map                                    │ 1513                           │  5573                      │ 1505              │
        │ Page up and down in 10000 element map                                   │ 1513                           │  5573                      │ 1505              │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 1520                           │  5924                      │ 1512              │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 1710                           │  6370                      │ 1702              │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 1532                           │  5976                      │ 1524              │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 1722                           │  6422                      │ 1714              │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1722                           │  6422                      │ 1714              │
        │ Apply 4 filters and clear with 100 element map using 10 window          │  611                           │  1895                      │  603              │
        │ Apply 4 filters and clear with 101 element map using 10 window          │  611                           │  1895                      │  603              │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │  611                           │  1895                      │  603              │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │ 1091                           │  7815                      │ 1083              │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │ 1091                           │  7815                      │ 1083              │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1691                           │ 15215                      │ 1683              │
        │ Invert ordering of 10 element map                                       │  472                           │   892                      │  464              │
        │ Invert ordering of 100 element map                                      │ 1552                           │  5572                      │ 1544              │
        │ Invert ordering of 101 element map                                      │ 1564                           │  5624                      │ 1556              │
        │ Invert ordering of 1000 element map                                     │ 1564                           │  5624                      │ 1556              │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  421                           │   841                      │  413              │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  421                           │   841                      │  413              │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  421                           │   841                      │  413              │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  421                           │   841                      │  413              │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  421                           │   841                      │  413              │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  421                           │   841                      │  413              │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  421                           │   841                      │  413              │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  421                           │   841                      │  413              │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1501                           │  5521                      │ 1493              │
        └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘

        ======= Nodes Created =======
        ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
        │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
        ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
        │ Focus by key (key not present) and unfocus in 10 element map            │   0                            │    0                       │   0               │
        │ Focus by key (key not present) and unfocus in 100 element map           │   0                            │    0                       │   0               │
        │ Focus by key (key not present) and unfocus in 101 element map           │   0                            │    0                       │   0               │
        │ Focus by key (key not present) and unfocus in 1000 element map          │   0                            │    0                       │   0               │
        │ Focus by key (key not present) and unfocus in 10000 element map         │   0                            │    0                       │   0               │
        │ Focus by key (key present) and unfocus in 10 element map                │   0                            │    0                       │   0               │
        │ Focus by key (key present) and unfocus in 100 element map               │   0                            │    0                       │   0               │
        │ Focus by key (key present) and unfocus in 101 element map               │   0                            │    0                       │   0               │
        │ Focus by key (key present) and unfocus in 1000 element map              │   0                            │    0                       │   0               │
        │ Focus by key (key present) and unfocus in 10000 element map             │   0                            │    0                       │   0               │
        │ Focus up and down in 10 element map                                     │   0                            │    0                       │   0               │
        │ Focus up and down in 100 element map                                    │   0                            │    0                       │   0               │
        │ Focus up and down in 101 element map                                    │   0                            │    0                       │   0               │
        │ Focus up and down in 1000 element map                                   │   0                            │    0                       │   0               │
        │ Focus up and down in 10000 element map                                  │   0                            │    0                       │   0               │
        │ Focus left and right in a map with 10 rows                              │   0                            │    0                       │   0               │
        │ Focus left and right in a map with 100 rows                             │   0                            │    0                       │   0               │
        │ Focus left and right in a map with 101 rows                             │   0                            │    0                       │   0               │
        │ Focus left and right in a map with 1000 rows                            │   0                            │    0                       │   0               │
        │ Focus left and right in a map with 10000 rows                           │   0                            │    0                       │   0               │
        │ Page up and down in 10 element map                                      │   0                            │    0                       │   0               │
        │ Page up and down in 100 element map                                     │   0                            │    0                       │   0               │
        │ Page up and down in 101 element map                                     │   0                            │    0                       │   0               │
        │ Page up and down in 1000 element map                                    │   0                            │    0                       │   0               │
        │ Page up and down in 10000 element map                                   │   0                            │    0                       │   0               │
        │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  17                            │  401                       │  17               │
        │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 209                            │  849                       │ 209               │
        │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  17                            │  401                       │  17               │
        │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 209                            │  849                       │ 209               │
        │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 209                            │  849                       │ 209               │
        │ Apply 4 filters and clear with 100 element map using 10 window          │ 188                            │ 1052                       │ 188               │
        │ Apply 4 filters and clear with 101 element map using 10 window          │ 188                            │ 1052                       │ 188               │
        │ Apply 4 filters and clear with 1000 element map using 10 window         │ 188                            │ 1052                       │ 188               │
        │ Apply 4 filters and clear with 1000 element map using 50 window         │ 188                            │ 4892                       │ 188               │
        │ Apply 4 filters and clear with 10000 element map using 50 window        │ 188                            │ 4892                       │ 188               │
        │ Apply 4 filters and clear with 10000 element map using 100 window       │ 188                            │ 9692                       │ 188               │
        │ Invert ordering of 10 element map                                       │  49                            │   49                       │  49               │
        │ Invert ordering of 100 element map                                      │  49                            │   49                       │  49               │
        │ Invert ordering of 101 element map                                      │  49                            │   49                       │  49               │
        │ Invert ordering of 1000 element map                                     │  49                            │   49                       │  49               │
        │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │   0                            │    0                       │   0               │
        │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │   0                            │    0                       │   0               │
        │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │   0                            │    0                       │   0               │
        │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │   0                            │    0                       │   0               │
        │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │   0                            │    0                       │   0               │
        │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │   0                            │    0                       │   0               │
        │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │   0                            │    0                       │   0               │
        │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │   0                            │    0                       │   0               │
        │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │   0                            │    0                       │   0               │
        └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘
        |}]
    ;;
  end)
;;
