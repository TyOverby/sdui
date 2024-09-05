open! Core
open! Bonsai_web_test
open! Bonsai_web

let handle () =
  let component graph =
    let%map.Bonsai codemirror =
      Bonsai_web_ui_codemirror.of_initial_state
        ~name:"editor"
        (Codemirror.State.Editor_state.create
           (Codemirror.State.Editor_state_config.create
              ~extensions:[ Codemirror_themes.get Basic_dark ]
              ()))
        graph
    in
    Bonsai_web_ui_codemirror.view codemirror
  in
  Handle.create (Result_spec.vdom Fn.id) component
;;

let%expect_test "codemirror with themes starts without crashing (even in wasm)" =
  let handle = handle () in
  Handle.show handle;
  [%expect
    {| <codemirror data-codemirror-editor="editor" codemirror-test-hook=<fun>>  </codemirror> |}]
;;
