open! Core
open Bonsai_web_test

let run handle ~get_vdom ~name action =
  Handle.trigger_hook
    handle
    ~get_vdom
    ~selector:[%string "[data-dnd-name=%{name}]"]
    ~name:"dnd-test-hook"
    Bonsai_web_ui_drag_and_drop.For_testing.type_id
    action
;;
