open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Tabs = Bonsai_web_ui_tabs

module My_tabs = struct
  type t =
    | A
    | B
    | C
  [@@deriving sexp, compare, equal, enumerate]
end

open My_tabs

let basic_tab_component ?additional_button_attributes ?decorate ?f () graph =
  let f =
    Option.value f ~default:(fun ~change_tab:_ tab ->
      Bonsai.enum
        (module My_tabs)
        ~match_:tab
        ~with_:(fun tab _graph ->
          match tab with
          | A -> Bonsai.return (Vdom.Node.text "a")
          | B -> Bonsai.return (Vdom.Node.text "b")
          | C -> Bonsai.return (Vdom.Node.text "c")))
  in
  let state =
    Tabs.tab_state (module My_tabs) ~initial:A ~equal:[%equal: My_tabs.t] graph
  in
  let tab_result =
    Tabs.tab_ui
      ?additional_button_attributes
      ?decorate
      (module My_tabs)
      ~equal:[%equal: My_tabs.t]
      ~all_tabs:(Bonsai.return My_tabs.all)
      state
      ~f
      graph
  in
  Bonsai.map tab_result ~f:Tabs.Result.combine_trivially
;;

let%expect_test "how is it printed" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (basic_tab_component ()) in
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" class="bonsai_ui_tab selected" @on_click> A </button>
        <button name="B" class="bonsai_ui_tab" @on_click> B </button>
        <button name="C" class="bonsai_ui_tab" @on_click> C </button>
      </div>
      <div class="bonsai_ui_tab_body"> a </div>
    </div>
    |}]
;;

let%expect_test "you can click on a button to change the tab" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (basic_tab_component ()) in
  Handle.show handle;
  let before = [%expect.output] in
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"[name=B]";
  Handle.show handle;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  [%expect
    {|
    -1,8 +1,8
      <div class="bonsai_ui_tab_container">
        <div class="bonsai_ui_tab_tabs">
    -|    <button name="A" class="bonsai_ui_tab selected" @on_click> A </button>
    +|    <button name="A" class="bonsai_ui_tab" @on_click> A </button>
    -|    <button name="B" class="bonsai_ui_tab" @on_click> B </button>
    +|    <button name="B" class="bonsai_ui_tab selected" @on_click> B </button>
          <button name="C" class="bonsai_ui_tab" @on_click> C </button>
        </div>
    -|  <div class="bonsai_ui_tab_body"> a </div>
    +|  <div class="bonsai_ui_tab_body"> b </div>
      </div>
    |}]
;;

let%expect_test "you can customize the display of the tabs" =
  let decorate = function
    | A -> Vdom.Node.text "GIMME AN A!"
    | B -> Vdom.Node.text "GIMME A B!"
    | C -> Vdom.Node.text "GIMME A C!"
  in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (basic_tab_component ~decorate:(Bonsai.return decorate) ())
  in
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" class="bonsai_ui_tab selected" @on_click> GIMME AN A! </button>
        <button name="B" class="bonsai_ui_tab" @on_click> GIMME A B! </button>
        <button name="C" class="bonsai_ui_tab" @on_click> GIMME A C! </button>
      </div>
      <div class="bonsai_ui_tab_body"> a </div>
    </div>
    |}]
;;

let%expect_test "you can add more attributes to the buttons" =
  let alt text = Vdom.Attr.create "alt" text in
  let additional_attrs ~is_selected:_ = function
    | A -> alt "click on a!"
    | B -> alt "click on b!"
    | C -> alt "click on c!"
  in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (basic_tab_component
         ~additional_button_attributes:(Bonsai.return additional_attrs)
         ())
  in
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" alt="click on a!" class="bonsai_ui_tab selected" @on_click> A </button>
        <button name="B" alt="click on b!" class="bonsai_ui_tab" @on_click> B </button>
        <button name="C" alt="click on c!" class="bonsai_ui_tab" @on_click> C </button>
      </div>
      <div class="bonsai_ui_tab_body"> a </div>
    </div>
    |}]
;;

let%expect_test "you can switch the tab from inside the inner component" =
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (basic_tab_component
         ~f:(fun ~change_tab _ _graph ->
           let%arr change_tab = change_tab in
           Vdom.Node.button
             ~attrs:
               [ Vdom.Attr.many_without_merge
                   [ Vdom.Attr.id "my-button"
                   ; Vdom.Attr.on_click (fun _ -> change_tab C)
                   ]
               ]
             [ Vdom.Node.text "click to move to tab c!" ])
         ())
  in
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"#my-button";
  Handle.show handle;
  [%expect
    {|
    <div class="bonsai_ui_tab_container">
      <div class="bonsai_ui_tab_tabs">
        <button name="A" class="bonsai_ui_tab" @on_click> A </button>
        <button name="B" class="bonsai_ui_tab" @on_click> B </button>
        <button name="C" class="bonsai_ui_tab selected" @on_click> C </button>
      </div>
      <div class="bonsai_ui_tab_body">
        <button id="my-button" @on_click> click to move to tab c! </button>
      </div>
    </div>
    |}]
;;
