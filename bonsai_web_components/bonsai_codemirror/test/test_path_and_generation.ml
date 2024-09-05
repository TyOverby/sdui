open! Core
open! Bonsai_web_test
open! Bonsai_web
open Bonsai.Let_syntax

module Path_and_generation =
  Bonsai_web_ui_codemirror.Private.For_tests.Path_and_generation

module Action = struct
  type t =
    | Reset
    | Toggle_scope
end

let handle () =
  let component graph =
    let scope, toggle_scope = Bonsai.toggle ~default_model:false graph in
    let result, reset =
      Bonsai.with_model_resetter
        ~f:
          (Bonsai.scope_model
             (module Bool)
             ~on:scope
             ~for_:Path_and_generation.model_resetter_generation)
        graph
    in
    let%arr toggle_scope = toggle_scope
    and result = result
    and reset = reset in
    ( result
    , function
      | Action.Reset -> reset
      | Toggle_scope -> toggle_scope )
  in
  Handle.create
    (module struct
      type t = Path_and_generation.t * (Action.t -> unit Effect.t)
      type incoming = Action.t

      let incoming (_, inject) action = inject action
      let view (view, _) = Sexp.to_string_hum (Path_and_generation.sexp_of_t view)
    end)
    component
;;

let reset handle =
  Handle.do_actions handle [ Action.Reset ];
  Handle.recompute_view handle;
  Handle.show handle;
  Handle.show handle
;;

let toggle_scope handle =
  Handle.do_actions handle [ Action.Toggle_scope ];
  Handle.show handle;
  Handle.show handle
;;

(* This test demonstrates the interaction between [with_model_resetter],
   [scope_model], and [model_resetter_generation]. There is no real
   organization to the events, so to read this, just keep a mental
   state-machine of what generation you expect each scope to have, and see if
   the test lines up with what you thought. It's not unlikely that you'll be
   wrong in your prediction, but the test should at least be reasonable. *)
let%expect_test _ =
  let handle = handle () in
  Handle.show handle;
  [%expect {| ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id -1)) |}];
  reset handle;
  [%expect
    {|
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id 1))
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id 1))
    |}];
  toggle_scope handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id -1))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 0))
    |}];
  reset handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 1))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 1))
    |}];
  reset handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 2))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 2))
    |}];
  reset handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 3))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 3))
    |}];
  reset handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 4))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 4))
    |}];
  toggle_scope handle;
  [%expect
    {|
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id -1))
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id 0))
    |}];
  toggle_scope handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id -1))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id -1))
    |}];
  toggle_scope handle;
  [%expect
    {|
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id -1))
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id -1))
    |}];
  reset handle;
  [%expect
    {|
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id 0))
    ((path_id bonsai_path_gggbgmhdgf_x_x) (generation_id 0))
    |}];
  toggle_scope handle;
  [%expect
    {|
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id -1))
    ((path_id bonsai_path_hehchfgf_x_x) (generation_id 0))
    |}]
;;
