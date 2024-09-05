open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
module Typeahead = Bonsai_web_ui_typeahead.Typeahead

let shared_computation ?(to_string = Bonsai.return Data.to_string) () =
  Typeahead.create
    (module Data)
    ~equal:[%equal: Data.t]
    ~all_options:(Bonsai.return Data.all)
    ~placeholder:"Select a value"
    ~to_string
;;

let view_computation ?to_string () graph =
  let%sub { view; _ } = shared_computation ?to_string () graph in
  view
;;

let view_and_inject_computation graph =
  let%sub { view; set_selected = inject; _ } = shared_computation () graph in
  Bonsai.both view inject
;;

let view_and_result_computation graph =
  let%sub { view; selected = result; _ } = shared_computation () graph in
  Bonsai.both view result
;;

let%expect_test "Initial typeahead state" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (view_computation ()) in
  Handle.show handle;
  [%expect
    {|
    <div>
      <input type="text"
             list="bonsai_path_replaced_in_test"
             placeholder="Select a value"
             value=""
             #value=""
             @on_blur
             @on_change
             @on_focus
             @on_input> </input>
      <datalist id="bonsai_path_replaced_in_test">
        <option value="Option A"> Option A </option>
        <option value="Option B"> Option B </option>
        <option value="Option C"> Option C </option>
      </datalist>
    </div>
    |}]
;;

let%expect_test "Focusing and un-focusing the input shows and hides the datalist when \
                 not in tests"
  =
  let component graph =
    let%sub { view; _ } =
      Typeahead.Private.For_testing.create_with_browser_behavior_in_test
        (module Data)
        ~equal:[%equal: Data.t]
        ~all_options:(Bonsai.return Data.all)
        ~placeholder:"Select a value"
        ~to_string:(Bonsai.return Data.to_string)
        graph
    in
    view
  in
  let handle = Handle.create (Result_spec.vdom Fn.id) component in
  Handle.show handle;
  [%expect
    {|
    <div>
      <input type="text"
             list="bonsai_path_replaced_in_test"
             placeholder="Select a value"
             value=""
             #value=""
             @on_blur
             @on_change
             @on_focus
             @on_input> </input>

    </div>
    |}];
  Handle.focus handle ~get_vdom:Fn.id ~selector:"input";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
               value=""
               #value=""
               @on_blur
               @on_change
               @on_focus
               @on_input> </input>
    +|  <datalist id="bonsai_path_replaced_in_test">
    +|    <option value="Option A"> Option A </option>
    +|    <option value="Option B"> Option B </option>
    +|    <option value="Option C"> Option C </option>
    +|  </datalist>
      </div>
    |}];
  Handle.blur handle ~get_vdom:Fn.id ~selector:"input";
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
               value=""
               #value=""
               @on_blur
               @on_change
               @on_focus
               @on_input> </input>
    -|  <datalist id="bonsai_path_replaced_in_test">
    -|    <option value="Option A"> Option A </option>
    -|    <option value="Option B"> Option B </option>
    -|    <option value="Option C"> Option C </option>
    -|  </datalist>
      </div>
    |}]
;;

let%expect_test "Change typeahead contents" =
  let handle = Handle.create (Result_spec.vdom Fn.id) (view_computation ()) in
  Handle.show handle;
  let before = [%expect.output] in
  Handle.input_text
    handle
    ~get_vdom:Fn.id
    ~selector:"input"
    ~text:(Data.to_string Data.Option_C);
  Handle.show handle;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  (* Expected change: input value should change. *)
  [%expect
    {|
    -1,16 +1,16
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
    -|         value=""
    +|         value="Option C"
    -|         #value=""
    +|         #value="Option C"
               @on_blur
               @on_change
               @on_focus
               @on_input> </input>
        <datalist id="bonsai_path_replaced_in_test">
          <option value="Option A"> Option A </option>
          <option value="Option B"> Option B </option>
          <option value="Option C"> Option C </option>
        </datalist>
      </div>
    |}]
;;

let%expect_test "use setter" =
  let handle =
    Handle.create
      (module struct
        type incoming = Data.t option
        type t = Vdom.Node.t * (Data.t option -> unit Ui_effect.t)

        let view (vdom, _) =
          let module V = (val Result_spec.vdom Fn.id) in
          V.view vdom
        ;;

        let incoming (_, inject) = inject
      end)
      view_and_inject_computation
  in
  Handle.show handle;
  let _before = [%expect.output] in
  Handle.do_actions handle [ Some Data.Option_A ];
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
    -|         value=""
    +|         value="Option A"
    -|         #value=""
    +|         #value="Option A"
               @on_blur
               @on_change
               @on_focus
               @on_input> </input>
        <datalist id="bonsai_path_replaced_in_test">
          <option value="Option A"> Option A </option>
          <option value="Option B"> Option B </option>
          <option value="Option C"> Option C </option>
        </datalist>
      </div>
    |}];
  Handle.do_actions handle [ None ];
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
    -|         value="Option A"
    +|         value=""
    -|         #value="Option A"
    +|         #value=""
               @on_blur
               @on_change
               @on_focus
               @on_input> </input>
        <datalist id="bonsai_path_replaced_in_test">
          <option value="Option A"> Option A </option>
          <option value="Option B"> Option B </option>
          <option value="Option C"> Option C </option>
        </datalist>
      </div>
    |}]
;;

let%expect_test "Select element using partial input" =
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = Vdom.Node.t * Data.t option

           let sexp_of_t (_view, result) = [%sexp_of: Data.t option] result
         end))
      view_and_result_computation
  in
  Handle.show handle;
  [%expect {| () |}];
  (* "O" is not unique, nothing happens *)
  Handle.input_text handle ~get_vdom:Tuple2.get1 ~selector:"input" ~text:"O";
  Handle.show handle;
  [%expect {| () |}];
  (* 'C' is unique, use it! *)
  Handle.input_text handle ~get_vdom:Tuple2.get1 ~selector:"input" ~text:"C";
  Handle.show handle;
  [%expect {| (Option_C) |}]
;;

let%expect_test "empty string clears the selection" =
  let component =
    Typeahead.create
      (module Data)
      ~equal:[%equal: Data.t]
      ~all_options:(Bonsai.return [ Data.Option_A ])
      ~placeholder:"Select a value"
      ~to_string:(Bonsai.return Data.to_string)
  in
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = Data.t option Typeahead.t

           let sexp_of_t { Typeahead.selected; _ } = [%sexp_of: Data.t option] selected
         end))
      component
  in
  let get_vdom { Typeahead.view; _ } = view in
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"A";
  Handle.show handle;
  (* Selecting with A happens as you'd expect *)
  [%expect {| (Option_A) |}];
  (* Z does not match anything, so this should be empty *)
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"Z";
  Handle.show handle;
  [%expect {| () |}];
  (* Deleting the Z continues to select nothing. *)
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"";
  Handle.show handle;
  [%expect {| () |}]
;;

(* This behaviour exists for two reasons:
   1. It's the legacy behaviour and it would be a bit tricky to figure out whether any
   apps rely on it.
   2. It can be useful in niche scenarios when there is data being populated from the
   server that may not match the set of options presented to the user.

   Adding a flag to configure what to do when a value not in [all_options] is set seems
   reasonable, and easy enough to do if someone ever asks for it. *)
let%expect_test "setting a value that isn't present in [all_options] still sets the \
                 typeahead to that value"
  =
  let handle =
    Handle.create
      (module struct
        type incoming = Data.t
        type t = Data.t option Typeahead.t

        let view ({ view; selected; _ } : t) =
          let module V = (val Result_spec.vdom Fn.id) in
          [ [%sexp_of: Data.t option] selected |> Sexp.to_string_hum
          ; "========="
          ; V.view view
          ]
          |> String.concat ~sep:"\n"
        ;;

        let incoming ({ set_selected; _ } : t) data = set_selected (Some data)
      end)
      (Typeahead.create
         (module Data)
         ~equal:[%equal: Data.t]
         ~all_options:(Bonsai.return [ Data.Option_A ])
         ~placeholder:"Select a value"
         ~to_string:(Bonsai.return Data.to_string))
  in
  Handle.show handle;
  [%expect
    {|
    ()
    =========
    <div>
      <input type="text"
             list="bonsai_path_replaced_in_test"
             placeholder="Select a value"
             value=""
             #value=""
             @on_blur
             @on_change
             @on_focus
             @on_input> </input>
      <datalist id="bonsai_path_replaced_in_test">
        <option value="Option A"> Option A </option>
      </datalist>
    </div>
    |}];
  Handle.do_actions handle [ Data.Option_B ];
  Handle.show handle;
  [%expect
    {|
    (Option_B)
    =========
    <div>
      <input type="text"
             list="bonsai_path_replaced_in_test"
             placeholder="Select a value"
             value="Option B"
             #value="Option B"
             @on_blur
             @on_change
             @on_focus
             @on_input> </input>
      <datalist id="bonsai_path_replaced_in_test">
        <option value="Option A"> Option A </option>
      </datalist>
    </div>
    |}]
;;

let%expect_test "dynamic [to_string]." =
  let to_string_var = Bonsai.Expert.Var.create Data.to_string in
  let to_string = Bonsai.Expert.Var.value to_string_var in
  let handle = Handle.create (Result_spec.vdom Fn.id) (view_computation ~to_string ()) in
  Handle.store_view handle;
  Bonsai.Expert.Var.set to_string_var (fun data -> Data.to_string data ^ "!");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
               value=""
               #value=""
               @on_blur
               @on_change
               @on_focus
               @on_input> </input>
        <datalist id="bonsai_path_replaced_in_test">
    -|    <option value="Option A"> Option A </option>
    +|    <option value="Option A!"> Option A! </option>
    -|    <option value="Option B"> Option B </option>
    +|    <option value="Option B!"> Option B! </option>
    -|    <option value="Option C"> Option C </option>
    +|    <option value="Option C!"> Option C! </option>
        </datalist>
      </div>
    |}]
;;
