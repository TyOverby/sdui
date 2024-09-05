open! Core
open Bonsai_web_test

let send_transaction handle ~get_vdom ~name transaction =
  Handle.trigger_hook
    handle
    ~get_vdom
    ~selector:[%string "[data-codemirror-editor=%{name}]"]
    ~name:"codemirror-test-hook"
    Bonsai_web_ui_codemirror.For_testing.type_id
    transaction
;;
