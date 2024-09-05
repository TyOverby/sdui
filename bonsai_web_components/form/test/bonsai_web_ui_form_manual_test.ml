open! Core
open! Bonsai_web
open! Bonsai_web_test.Experimental
open! Bonsai.Let_syntax
open Js_of_ocaml
module Form = Bonsai_web_ui_form.With_manual_view

(* This ensures that any source positions printed as part of tests in this file will have
   their line and column censored, reducing noise if we add tests above the ones that
   print source positions. *)
module Expect_test_config = struct
  include Expect_test_config

  let sanitize = Expect_test_helpers_base.hide_positions_in_string
end

let viewless_form_result_spec (type a) sexp_of_a : ((a, unit) Form.t, a) Result_spec.t =
  (module struct
    type t = (a, unit) Form.t
    type incoming = a

    let view form =
      Form.value form
      |> [%sexp_of: a Or_error.t]
      |> Expect_test_helpers_base.sexp_to_string
    ;;

    let to_vdom _ = Vdom.Node.none_deprecated [@alert "-deprecated"]
    let incoming = Form.set
  end)
;;

let form_result_spec (type a) ?filter_printed_attributes ?censor_paths sexp_of_a
  : ((a, Vdom.Node.t) Form.t, a) Result_spec.t
  =
  (module struct
    type t = (a, Vdom.Node.t) Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes ?censor_paths ()) in
      let vdom = Form.view form in
      let vdom = V.view vdom in
      let value =
        Form.value form
        |> [%sexp_of: a Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let to_vdom = Form.view
    let incoming = Form.set
  end)
;;

let list_form_result_spec (type a) ?filter_printed_attributes ?censor_paths sexp_of_a
  : ( (a list, (a, Vdom.Node.t) Form.Elements.Multiple.t) Form.t
      , [ `Set of a list | `Remove of int | `Append ] )
      Result_spec.t
  =
  (module struct
    type t = (a list, (a, Vdom.Node.t) Form.Elements.Multiple.t) Form.t

    type incoming =
      [ `Set of a list
      | `Remove of int
      | `Append
      ]

    let to_vdom form =
      let { Form.Elements.Multiple.items; add_element = _ } = Form.view form in
      Vdom.Node.div (List.map items ~f:(fun { form; remove = _ } -> Form.view form))
    ;;

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes ?censor_paths ()) in
      let { Form.Elements.Multiple.items; add_element = _ } = Form.view form in
      let vdom =
        List.map items ~f:(fun { form; remove = _ } -> V.view (Form.view form))
        |> String.concat ~sep:"\n--------------\n"
      in
      let value =
        Form.value form
        |> [%sexp_of: a list Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let incoming ({ value = _; view; set } : _ Form.t) = function
      | `Set list -> set list
      | `Remove i ->
        let ({ items; add_element = _ } : _ Form.Elements.Multiple.t) = view in
        let ({ form = _; remove } : _ Form.Elements.Multiple.item) =
          List.nth_exn items i
        in
        remove
      | `Append ->
        let ({ items = _; add_element } : _ Form.Elements.Multiple.t) = view in
        add_element
    ;;
  end)
;;

let nonempty_list_form_result_spec
  (type a)
  ?filter_printed_attributes
  ?censor_paths
  sexp_of_a
  : ( (a Nonempty_list.t, (a, Vdom.Node.t) Form.Elements.Multiple.nonempty_t) Form.t
      , [ `Set of a Nonempty_list.t | `Remove of int | `Append ] )
      Result_spec.t
  =
  (module struct
    type t =
      (a Nonempty_list.t, (a, Vdom.Node.t) Form.Elements.Multiple.nonempty_t) Form.t

    type incoming =
      [ `Set of a Nonempty_list.t
      | `Remove of int
      | `Append
      ]

    let to_vdom form =
      let { Form.Elements.Multiple.hd = hd_form; tl; add_element = _ } = Form.view form in
      let tl_forms = List.map tl ~f:(fun { form; remove = _ } -> form) in
      Vdom.Node.div (List.map (hd_form :: tl_forms) ~f:Form.view)
    ;;

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes ?censor_paths ()) in
      let { Form.Elements.Multiple.hd = hd_form; tl; add_element = _ } = Form.view form in
      let vdom =
        let tl_forms = List.map tl ~f:(fun { form; remove = _ } -> form) in
        List.map (hd_form :: tl_forms) ~f:(Fn.compose V.view Form.view)
        |> String.concat ~sep:"\n--------------\n"
      in
      let value =
        Form.value form
        |> [%sexp_of: a Nonempty_list.t Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let incoming ({ value = _; view; set } : _ Form.t) = function
      | `Set list -> set list
      | `Remove i ->
        let ({ hd = _; tl; add_element = _ } : _ Form.Elements.Multiple.nonempty_t) =
          view
        in
        let ({ form = _; remove } : _ Form.Elements.Multiple.item) =
          List.nth_exn tl (i - 1)
        in
        remove
      | `Append ->
        let ({ hd = _; tl = _; add_element } : _ Form.Elements.Multiple.nonempty_t) =
          view
        in
        add_element
    ;;
  end)
;;

let%expect_test "placeholders" =
  let placeholder_var = Bonsai.Expert.Var.create "placeholder1" in
  let component =
    Form.Elements.Textbox.string
      ~placeholder:(Bonsai.Expert.Var.value placeholder_var)
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder="placeholder1"
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Bonsai.Expert.Var.set placeholder_var "placeholder2";
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder="placeholder2"
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}]
;;

let%expect_test "setting a constant form does nothing" =
  let component =
    Form.Elements.Non_interactive.constant
      (Bonsai.return (Vdom.Node.text "test"))
      (Bonsai.return (Ok "test"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok test)

    ==============
    test
    |}];
  Handle.do_actions handle [ "not test" ];
  Handle.show_diff handle
;;

let%expect_test "typing into a string textbox" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             @on_input> </input>
    |}]
;;

let%expect_test "typing into a string password textbox" =
  let component = Form.Elements.Password.string ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="password"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="password"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             @on_input> </input>
    |}]
;;

let%expect_test "initially dropdown set to value not in initial set" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "a"; "b" ])
      ~init:`Empty
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.do_actions handle [ "c" ];
  Handle.show handle;
  [%expect
    {|
    (Ok c)

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}]
;;

let%expect_test "mandatory dropdown with initial value not in [all-options] set" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "a"; "b" ])
      ~init:(`This (Bonsai.return "c"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok c)

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false"> a </option>
      <option value="1" #selected="false"> b </option>
    </select>
    |}]
;;

let%expect_test "mandatory dropdown with initial value not in [all-options] set, being \
                 initalized to something else that isn't in the set"
  =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "a"; "b" ])
      ~init:(`This (Bonsai.return "c"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.do_actions handle [ "d" ];
  Handle.show handle;
  [%expect
    {|
    (Ok d)

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false"> a </option>
      <option value="1" #selected="false"> b </option>
    </select>
    |}]
;;

let%expect_test "mandatory dropdown starting empty" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "hello"; "world" ])
      ~init:`Empty
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="false"> world </option>
    </select>
    |}];
  Handle.change handle ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok hello)

      ==============
      <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
    -|  <option value="0" #selected="true">  </option>
    +|  <option value="0" #selected="false">  </option>
    -|  <option value="1" #selected="false"> hello </option>
    +|  <option value="1" #selected="true"> hello </option>
        <option value="2" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "optional dropdown with initial value not in [all-options] set" =
  let component =
    Form.Elements.Dropdown.list_opt
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "a"; "b" ])
      ~init:(`This (Bonsai.return "c"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string option]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok (c))

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}]
;;

let%expect_test "optional dropdown with initial value not in [all-options] set, being \
                 initalized to something else that isn't in the set"
  =
  let component =
    Form.Elements.Dropdown.list_opt
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "a"; "b" ])
      ~init:(`This (Bonsai.return "c"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string option]) component in
  Handle.do_actions handle [ Some "d" ];
  Handle.show handle;
  [%expect
    {|
    (Ok (d))

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}]
;;

let%expect_test "optional dropdown starting empty" =
  let component =
    Form.Elements.Dropdown.list_opt
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "hello"; "world" ])
      ~init:`Empty
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string option]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="false"> world </option>
    </select>
    |}];
  Handle.change handle ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (hello))

      ==============
      <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
    -|  <option value="0" #selected="true">  </option>
    +|  <option value="0" #selected="false">  </option>
    -|  <option value="1" #selected="false"> hello </option>
    +|  <option value="1" #selected="true"> hello </option>
        <option value="2" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown with default value" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "hello"; "world" ])
      ~init:(`This (Bonsai.return "world"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok world)

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false"> hello </option>
      <option value="1" #selected="true"> world </option>
    </select>
    |}];
  Handle.change handle ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok world)
    +|(Ok hello)

      ==============
      <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
    -|  <option value="0" #selected="false"> hello </option>
    +|  <option value="0" #selected="true"> hello </option>
    -|  <option value="1" #selected="true"> world </option>
    +|  <option value="1" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown_opt with default value" =
  let component =
    Form.Elements.Dropdown.list_opt
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "hello"; "world" ])
      ~init:(`This (Bonsai.return "world"))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string option]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok (world))

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="false"> hello </option>
      <option value="2" #selected="true"> world </option>
    </select>
    |}];
  Handle.change handle ~selector:"select" ~value:"0";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (world))
    +|(Ok ())

      ==============
      <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
    -|  <option value="0" #selected="false">  </option>
    +|  <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> hello </option>
    -|  <option value="2" #selected="true"> world </option>
    +|  <option value="2" #selected="false"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown" =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [ "hello"; "world" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok hello)

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="true"> hello </option>
      <option value="1" #selected="false"> world </option>
    </select>
    |}];
  Handle.change handle ~selector:"select" ~value:"1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok hello)
    +|(Ok world)

      ==============
      <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
    -|  <option value="0" #selected="true"> hello </option>
    +|  <option value="0" #selected="false"> hello </option>
    -|  <option value="1" #selected="false"> world </option>
    +|  <option value="1" #selected="true"> world </option>
      </select>
    |}]
;;

let%expect_test "dropdown but without any elements to pick from " =
  let component =
    Form.Elements.Dropdown.list
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return [])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
      <option value="0" #selected="true">  </option>
    </select>
    |}]
;;

let%expect_test "setting into a string textbox" =
  let component = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ "hello world" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             @on_input> </input>
    |}]
;;

let%expect_test "typing into a int textbox" =
  let component = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok 123)

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=""
    +|       value:normalized=123
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 123)
    +|(Error "Expected an integer")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=123
    +|       value:normalized="hello world"
             @on_input> </input>
    |}]
;;

let%expect_test "setting into a int textbox" =
  let component = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok 123)

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=""
    +|       value:normalized=123
             @on_input> </input>
    |}]
;;

let%expect_test "typing into a paired string textbox * int textbox " =
  let component graph =
    let string_form =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
    in
    let int_form =
      Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
    in
    let%arr string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
    |> Form.map_view ~f:(fun (a, b) -> Vdom.Node.div [ a; b ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=""
             @on_input> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~selector:"input:nth-child(1)" ~text:"hello world";
  Handle.input_text handle ~selector:"input:nth-child(2)" ~text:"123";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               @on_input> </input>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
    -|         value:normalized=""
    +|         value:normalized=123
               @on_input> </input>
      </div>
    |}]
;;

let%expect_test "setting into a paired string textbox * int textbox " =
  let component graph =
    let string_form =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
    in
    let int_form =
      Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
    in
    let%arr string_form = string_form
    and int_form = int_form in
    Form.both string_form int_form
    |> Form.map_view ~f:(fun (a, b) -> Vdom.Node.div [ a; b ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string * int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=""
             @on_input> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.do_actions handle [ "hello world", 123 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "Expected an integer")
    +|(Ok ("hello world" 123))

      ==============
      <div>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
    -|         value:normalized=""
    +|         value:normalized="hello world"
               @on_input> </input>
        <input @key=bonsai_path_replaced_in_test
               type="text"
               placeholder=""
               spellcheck="false"
    -|         value:normalized=""
    +|         value:normalized=123
               @on_input> </input>
      </div>
    |}]
;;

let%test_module "Form.all" =
  (module struct
    let make_handle () =
      let component graph =
        let string_forms =
          Bonsai.all
            (List.init 3 ~f:(fun _ ->
               Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph))
        in
        let%arr string_forms = string_forms in
        Form.all string_forms |> Form.map_view ~f:(fun l -> Vdom.Node.div l)
      in
      Handle.create (form_result_spec [%sexp_of: string list]) component
    ;;

    let%expect_test "typing into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok ("" "" ""))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
        </div>
        |}];
      Handle.input_text handle ~selector:"input:nth-child(1)" ~text:"hello world";
      Handle.input_text handle ~selector:"input:nth-child(2)" ~text:"quack";
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok ("" "" ""))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
        </div>
        |}];
      Handle.do_actions handle [ [ "hello world"; "quack"; "" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more values than forms)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions handle [ [ "hello world"; "quack"; ""; "oh no" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set called on result of Form.all with a list value whose length doesn't match the number of forms "
         "more values than forms" (form_count 3) (edits_count 4)
         "dropping left-over values")

        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more forms than values)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions handle [ [ "hello world"; "quack" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set called on result of Form.all with a list value whose length doesn't match the number of forms "
         "more forms than values" (form_count 3) (edits_count 2)
         "not setting left-over forms")

        -|(Ok ("" "" ""))
        +|(Ok ("hello world" quack ""))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;
  end)
;;

let%test_module "Form.all_map" =
  (module struct
    let make_handle () =
      let component graph =
        let string_forms =
          Bonsai.all_map
            (List.init 3 ~f:(fun i ->
               ( i
               , fun graph ->
                   Form.Elements.Textbox.string
                     ~allow_updates_when_focused:`Never
                     ()
                     graph ))
             |> Int.Map.of_alist_exn)
            graph
        in
        let%arr string_forms = string_forms in
        Form.all_map string_forms
        |> Form.map_view ~f:(fun map -> Vdom.Node.div (Map.data map))
      in
      Handle.create (form_result_spec [%sexp_of: string Int.Map.t]) component
    ;;

    let%expect_test "typing into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok (
          (0 "")
          (1 "")
          (2 "")))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
        </div>
        |}];
      Handle.input_text handle ~selector:"input:nth-child(1)" ~text:"hello world";
      Handle.input_text handle ~selector:"input:nth-child(2)" ~text:"quack";
      Handle.show_diff handle;
      [%expect
        {|
          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes " =
      let handle = make_handle () in
      Handle.show handle;
      [%expect
        {|
        (Ok (
          (0 "")
          (1 "")
          (2 "")))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="text"
                 placeholder=""
                 spellcheck="false"
                 value:normalized=""
                 @on_input> </input>
        </div>
        |}];
      Handle.do_actions
        handle
        [ Int.Map.of_alist_exn [ 0, "hello world"; 1, "quack"; 2, "" ] ];
      Handle.show_diff handle;
      [%expect
        {|
          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more values than forms)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions
        handle
        [ Int.Map.of_alist_exn [ 0, "hello world"; 1, "quack"; 2, ""; 3, "oh no" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set on the result of Form.all_map has mismatched keys"
         "update contains key not present in active forms" (key 3))

          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;

    let%expect_test "setting into a list of string textboxes (more forms than values)" =
      let handle = make_handle () in
      Handle.store_view handle;
      Handle.do_actions handle [ Int.Map.of_alist_exn [ 0, "hello world"; 1, "quack" ] ];
      Handle.show_diff handle;
      [%expect
        {|
        ("WARNING: Form.set on the result of Form.all_map has mismatched keys"
         "update is missing key present in active form" (key 2))

          (Ok (
        -|  (0 "")
        +|  (0 "hello world")
        -|  (1 "")
        +|  (1 quack)
            (2 "")))

          ==============
          <div>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized="hello world"
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
        -|         value:normalized=""
        +|         value:normalized=quack
                   @on_input> </input>
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
          </div>
        |}]
    ;;
  end)
;;

let%expect_test "typing into a local datetime textbox" =
  let component =
    Form.Elements.Date_time.datetime_local ~allow_updates_when_focused:`Never ()
  in
  let handle =
    Handle.create (form_result_spec [%sexp_of: Time_ns.Alternate_sexp.t]) component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <input type="datetime-local" placeholder="" spellcheck="false" value:normalized="" @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"1969-12-31T19:00";
  Handle.show handle;
  [%expect
    {|
    (Ok "1969-12-31 19:00:00Z")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           value:normalized=1969-12-31T19:00:00
           @on_input> </input>
    |}]
;;

let%expect_test "typing into a time span textbox" =
  let component =
    Form.Elements.Date_time.time_span ~allow_updates_when_focused:`Never ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Time_ns.Span.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized="" @on_input> </input>
      <select class="widget-dropdown" @on_change>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"24";
  Handle.show handle;
  [%expect
    {|
    (Ok 24s)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 @on_input> </input>
      <select class="widget-dropdown" @on_change>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.change handle ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok 24m)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 @on_input> </input>
      <select class="widget-dropdown" @on_change>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="false"> s </option>
        <option value="2" #selected="true"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}]
;;

let%expect_test "setting into a time span textbox" =
  let component =
    Form.Elements.Date_time.time_span ~allow_updates_when_focused:`Never ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Time_ns.Span.t]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized="" @on_input> </input>
      <select class="widget-dropdown" @on_change>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.do_actions handle [ Time_ns.Span.of_sec 24. ];
  Handle.show handle;
  [%expect
    {|
    (Ok 24s)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 @on_input> </input>
      <select class="widget-dropdown" @on_change>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="true"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="false"> h </option>
      </select>
    </div>
    |}];
  Handle.do_actions handle [ Time_ns.Span.of_hr 24. ];
  Handle.show handle;
  [%expect
    {|
    (Ok 1d)

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=24 @on_input> </input>
      <select class="widget-dropdown" @on_change>
        <option value="0" #selected="false"> ms </option>
        <option value="1" #selected="false"> s </option>
        <option value="2" #selected="false"> m </option>
        <option value="3" #selected="true"> h </option>
      </select>
    </div>
    |}]
;;

let%expect_test "typing into a time range textbox, with strict inequality required" =
  let component =
    Form.Elements.Date_time.Range.time ~allow_updates_when_focused:`Never ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~text:"11:11 AM" ~selector:"input:nth-child(1)";
  Handle.show handle;
  [%expect
    {|
    (Error "A value is required for the end of this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~text:"10:00 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Error "Start time must be strictly before the end time.")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~text:"11:11 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Error "Start time must be strictly before the end time.")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~text:"11:12 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Ok (11:11:00.000000000 11:12:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:12:00.000
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "typing into a time range textbox, with equality allowed" =
  let component =
    Form.Elements.Date_time.Range.time
      ~allow_equal:true
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~text:"11:11 AM" ~selector:"input:nth-child(1)";
  Handle.input_text handle ~text:"10:00 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Error "Start time must be before or the same as the end time.")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~text:"11:11 AM" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    (Ok (11:11:00.000000000 11:11:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:11:00.000
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "setting into a date range, with strict inequality required" =
  let component =
    Form.Elements.Date_time.Range.time ~allow_updates_when_focused:`Never ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  let ten_am = Time_ns.Ofday.of_string "10:00 AM" in
  let eleven_am = Time_ns.Ofday.of_string "11:00 AM" in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  (* Somehow, a bad range got set, so no setting should happen *)
  Handle.do_actions handle [ eleven_am, ten_am ];
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  (* A range where the endpoints are equal is not allowed in this case *)
  Handle.do_actions handle [ eleven_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  (* Finally, a good range! *)
  Handle.do_actions handle [ ten_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Ok (10:00:00.000000000 11:00:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "setting into a date range, with equality allowed" =
  let component =
    Form.Elements.Date_time.Range.time
      ~allow_equal:true
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: Time_ns.Ofday.t * Time_ns.Ofday.t])
      component
  in
  let ten_am = Time_ns.Ofday.of_string "10:00 AM" in
  let eleven_am = Time_ns.Ofday.of_string "11:00 AM" in
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.do_actions handle [ eleven_am, ten_am ];
  Handle.show handle;
  [%expect
    {|
    (Error "Values are required for this range")

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.do_actions handle [ eleven_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Ok (11:00:00.000000000 11:00:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             @on_input> </input>
    </div>
    |}];
  Handle.do_actions handle [ ten_am, eleven_am ];
  Handle.show handle;
  [%expect
    {|
    (Ok (10:00:00.000000000 11:00:00.000000000))

    ==============
    <div>
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=10:00:00.000
             @on_input> </input>
       -
      <input type="time"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=11:00:00.000
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "using the same component twice" =
  let component graph =
    let textbox =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
    in
    let%arr textbox = textbox in
    Form.both textbox textbox |> Form.map_view ~f:(fun (a, b) -> Vdom.Node.div [ a; b ])
  in
  let handle =
    Handle.create
      (form_result_spec ~censor_paths:false [%sexp_of: string * string])
      component
  in
  Handle.do_actions handle [ "a", "b" ];
  Handle.show handle;
  (* The real bug on display here is that two nodes have the same [key].
     This crashes the vdom library. *)
  [%expect
    {|
    (Ok (b b))

    ==============
    <div>
      <input @key=bonsai_path
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=b
             @on_input> </input>
      <input @key=bonsai_path
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=b
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "typing into an int number element (no default)" =
  let component =
    Form.Elements.Number.int
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "value not specified")

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           min="-1"
           max="10"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"10";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "value not specified")
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=""
    +|       value:normalized=10
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10)
    +|(Error "value not specified")

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=""
             @on_input> </input>
    |}]
;;

let%expect_test "typing into an int number element" =
  let component =
    Form.Elements.Number.int
      ~default:0
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           min="-1"
           max="10"
           value:normalized=0
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"10";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=0
    +|       value:normalized=10
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"-1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=-1
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"11";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 11) "higher than allowed threshold" (max 10)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=-1
    +|       value:normalized=11
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"-2";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 11) "higher than allowed threshold" (max 10)))
    +|(Error ((value -2) "lower than allowed threshold" (min -1)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=11
    +|       value:normalized=-2
             @on_input> </input>
    |}]
;;

let%expect_test "setting into an int number element (no default)" =
  let component =
    Form.Elements.Number.int
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "value not specified")

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           min="-1"
           max="10"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ 10 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "value not specified")
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=""
    +|       value:normalized=10
             @on_input> </input>
    |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.int
      ~default:0
      ~step:1
      ~min:(-1)
      ~max:10
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           min="-1"
           max="10"
           value:normalized=0
           @on_input> </input>
    |}];
  Handle.do_actions handle [ 10 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=0
    +|       value:normalized=10
             @on_input> </input>
    |}];
  Handle.do_actions handle [ -1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=10
    +|       value:normalized=-1
             @on_input> </input>
    |}];
  Handle.do_actions handle [ 11 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 11) "higher than allowed threshold" (max 10)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=-1
    +|       value:normalized=11
             @on_input> </input>
    |}];
  Handle.do_actions handle [ -2 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 11) "higher than allowed threshold" (max 10)))
    +|(Error ((value -2) "lower than allowed threshold" (min -1)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10"
    -|       value:normalized=11
    +|       value:normalized=-2
             @on_input> </input>
    |}]
;;

let%expect_test "typing into a float number element" =
  let component =
    Form.Elements.Number.float
      ~default:0.
      ~step:1.
      ~min:(-1.)
      ~max:10.1
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: float]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           min="-1"
           max="10.1"
           value:normalized=0
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"10.1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10.1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=0
    +|       value:normalized=10.1
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"-1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10.1)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=10.1
    +|       value:normalized=-1
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"10.2";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=-1
    +|       value:normalized=10.2
             @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"-1.1";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))
    +|(Error ((value -1.1) "lower than allowed threshold" (min -1)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=10.2
    +|       value:normalized=-1.1
             @on_input> </input>
    |}]
;;

let%expect_test "setting into an int number element" =
  let component =
    Form.Elements.Number.float
      ~default:0.
      ~step:1.
      ~min:(-1.)
      ~max:10.1
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle = Handle.create (form_result_spec [%sexp_of: float]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           min="-1"
           max="10.1"
           value:normalized=0
           @on_input> </input>
    |}];
  Handle.do_actions handle [ 10.1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 0)
    +|(Ok 10.1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=0
    +|       value:normalized=10.1
             @on_input> </input>
    |}];
  Handle.do_actions handle [ -1. ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok 10.1)
    +|(Ok -1)

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=10.1
    +|       value:normalized=-1
             @on_input> </input>
    |}];
  Handle.do_actions handle [ 10.2 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok -1)
    +|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=-1
    +|       value:normalized=10.2
             @on_input> </input>
    |}];
  Handle.do_actions handle [ -1.1 ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error ((value 10.2) "higher than allowed threshold" (max 10.1)))
    +|(Error ((value -1.1) "lower than allowed threshold" (min -1)))

      ==============
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             min="-1"
             max="10.1"
    -|       value:normalized=10.2
    +|       value:normalized=-1.1
             @on_input> </input>
    |}]
;;

let%expect_test "setting option that doesn't exist" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
      ~layout:`Vertical
      (Bonsai.return [ "a"; "b" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.do_actions handle [ "c" ];
  Handle.show handle;
  [%expect
    {|
    (Ok c)

    ==============
    <ul class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          a
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          b
        </label>
      </li>
    </ul>
    |}]
;;

let%expect_test "clicking on radio buttons" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
      ~layout:`Vertical
      (Bonsai.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          third
        </label>
      </li>
    </ul>
    |}];
  Handle.click_on handle ~selector:"label:nth-child(1) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok first)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    |}];
  Handle.click_on handle ~selector:"li:nth-child(2) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok first)
    +|(Ok second)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="true"
    +|             #checked="false"
                   @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   @on_click> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "setting into radio buttons" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
      ~layout:`Vertical
      (Bonsai.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          third
        </label>
      </li>
    </ul>
    |}];
  Handle.do_actions handle [ "first" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok first)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    |}];
  Handle.do_actions handle [ "second" ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok first)
    +|(Ok second)

      ==============
      <ul class="radio-button-container widget-radio-buttons"
          style={
            list-style: none;
            margin-left: 0px;
          }>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="true"
    +|             #checked="false"
                   @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
    -|             #checked="false"
    +|             #checked="true"
                   @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="radio"
                   name="bonsai_path_replaced_in_test"
                   class="radio-button"
                   #checked="false"
                   @on_click> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "horizontal radio buttons render with correct styles applied" =
  let component =
    Form.Elements.Radio_buttons.list
      ~to_string:Fn.id
      (module String)
      ~equal:[%equal: String.t]
      ~layout:`Horizontal
      (Bonsai.return [ "first" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <ul class="radio-button-container widget-radio-buttons"
        style={
          list-style: none;
          margin-left: 0px;
        }>
      <li style={ display: inline-block; }>
        <label>
          <input type="radio"
                 name="bonsai_path_replaced_in_test"
                 class="radio-button"
                 #checked="false"
                 @on_click> </input>
          first
        </label>
      </li>
    </ul>
    |}]
;;

let%expect_test "setting a checklist to a value not in the input" =
  let component =
    Form.Elements.Checkbox.set ~to_string:Fn.id (module String) (Bonsai.return [ "a" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Set.M(String).t]) component in
  Handle.do_actions handle [ String.Set.of_list [ "b" ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (b))

    ==============
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          a
        </label>
      </li>
    </ul>
    |}];
  (* [a] is in the set, but [b] isn't *)
  Handle.do_actions handle [ String.Set.of_list [ "a"; "b" ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (a b))

    ==============
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="true" @on_click> </input>
          a
        </label>
      </li>
    </ul>
    |}]
;;

let%expect_test "clicking a set checklist" =
  let component =
    Form.Elements.Checkbox.set
      ~to_string:Fn.id
      (module String)
      (Bonsai.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Set.M(String).t]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          third
        </label>
      </li>
    </ul>
    |}];
  Handle.click_on handle ~selector:"li:nth-child(1) input";
  Handle.recompute_view handle;
  Handle.click_on handle ~selector:"li:nth-child(2) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (first second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="false" @on_click> </input>
    +|      <input type="checkbox" #checked="true" @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="false" @on_click> </input>
    +|      <input type="checkbox" #checked="true" @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" @on_click> </input>
            third
          </label>
        </li>
      </ul>
    |}];
  Handle.click_on handle ~selector:"li:nth-child(1) input";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (first second))
    +|(Ok (second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="true" @on_click> </input>
    +|      <input type="checkbox" #checked="false" @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="true" @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" @on_click> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "setting into a set checklist" =
  let component =
    Form.Elements.Checkbox.set
      ~to_string:Fn.id
      (module String)
      (Bonsai.return [ "first"; "second"; "third" ])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Set.M(String).t]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          second
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          third
        </label>
      </li>
    </ul>
    |}];
  Handle.do_actions handle [ Set.of_list (module String) [ "first"; "second" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok ())
    +|(Ok (first second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="false" @on_click> </input>
    +|      <input type="checkbox" #checked="true" @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="false" @on_click> </input>
    +|      <input type="checkbox" #checked="true" @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" @on_click> </input>
            third
          </label>
        </li>
      </ul>
    |}];
  Handle.do_actions handle [ Set.of_list (module String) [ "second" ] ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok (first second))
    +|(Ok (second))

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
        <li style={ display: block; }>
          <label>
    -|      <input type="checkbox" #checked="true" @on_click> </input>
    +|      <input type="checkbox" #checked="false" @on_click> </input>
            first
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="true" @on_click> </input>
            second
          </label>
        </li>
        <li style={ display: block; }>
          <label>
            <input type="checkbox" #checked="false" @on_click> </input>
            third
          </label>
        </li>
      </ul>
    |}]
;;

let%test_module "file pickers" =
  (module struct
    let mock_js_file ~name : File.file Js.t =
      Js.Unsafe.coerce
        (object%js
           val name = Js.string name
        end)
    ;;

    let mock_bonsai_file ~name : Bonsai_web_ui_file.t =
      Bonsai_web_ui_file.For_testing.(
        create (Test_data.create_static ~filename:name ~contents:""))
    ;;

    let%expect_test "single: selecting a file" =
      let component =
        Form.Elements.File_select.single_opt
          ~accept:[ `Extension ".txt"; `Extension "png"; `Mimetype "image/jpeg" ]
          ()
      in
      let handle =
        Handle.create (form_result_spec [%sexp_of: Bonsai_web_ui_file.t option]) component
      in
      Handle.show handle;
      [%expect
        {|
        (Ok ())

        ==============
        <input type="file" accept=".txt,.png,image/jpeg" #value="" @on_input> </input>
        |}];
      Handle.input_files handle ~selector:"input" ~files:[ mock_js_file ~name:"test.txt" ];
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ())
        +|(Ok ("<file test.txt>"))

          ==============
        -|<input type="file" accept=".txt,.png,image/jpeg" #value="" @on_input> </input>
        +|<input type="file" accept=".txt,.png,image/jpeg" @on_input> </input>
        |}]
    ;;

    let%expect_test "single: setting an empty value" =
      let component =
        Form.Elements.File_select.single_opt
          ~accept:[ `Extension ".txt"; `Extension "png"; `Mimetype "image/jpeg" ]
          ()
      in
      let handle =
        Handle.create (form_result_spec [%sexp_of: Bonsai_web_ui_file.t option]) component
      in
      Handle.input_files handle ~selector:"input" ~files:[ mock_js_file ~name:"test.txt" ];
      Handle.show handle;
      [%expect
        {|
        (Ok ("<file test.txt>"))

        ==============
        <input type="file" accept=".txt,.png,image/jpeg" @on_input> </input>
        |}];
      Handle.do_actions handle [ None ];
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ("<file test.txt>"))
        +|(Ok ())

          ==============
        -|<input type="file" accept=".txt,.png,image/jpeg" @on_input> </input>
        +|<input type="file" accept=".txt,.png,image/jpeg" #value="" @on_input> </input>
        |}]
    ;;

    let%expect_test "single: setting a non-empty value emits a warning and is ignored" =
      let component =
        Form.Elements.File_select.single_opt
          ~accept:[ `Extension ".txt"; `Extension "png"; `Mimetype "image/jpeg" ]
          ()
      in
      let handle =
        Handle.create (form_result_spec [%sexp_of: Bonsai_web_ui_file.t option]) component
      in
      Handle.input_files handle ~selector:"input" ~files:[ mock_js_file ~name:"test.txt" ];
      Handle.show handle;
      [%expect
        {|
        (Ok ("<file test.txt>"))

        ==============
        <input type="file" accept=".txt,.png,image/jpeg" @on_input> </input>
        |}];
      Handle.do_actions handle [ Some (mock_bonsai_file ~name:"foo.txt") ];
      Handle.show handle;
      (* Note: A warning is emitted and the value is unchanged. *)
      [%expect
        {|
        ("WARNING: Attempted to set the value of a file select to a value other than [None]. This is prohibited by the browser and therefore ignored."
         (filename foo.txt))
        (Ok ("<file test.txt>"))

        ==============
        <input type="file" accept=".txt,.png,image/jpeg" @on_input> </input>
        |}]
    ;;

    let%expect_test "list: selecting multiple files" =
      let component =
        Form.Elements.File_select.multiple
          ~accept:[ `Extension ".doc"; `Extension "docx" ]
          ()
      in
      let handle =
        Handle.create
          (form_result_spec [%sexp_of: Bonsai_web_ui_file.t Filename.Map.t])
          component
      in
      Handle.show handle;
      [%expect
        {|
        (Ok ())

        ==============
        <input type="file" accept=".doc,.docx" multiple="" #value="" @on_input> </input>
        |}];
      Handle.input_files
        handle
        ~selector:"input"
        ~files:[ mock_js_file ~name:"test1.doc"; mock_js_file ~name:"test2.doc" ];
      Handle.show_diff handle;
      [%expect
        {|
        -|(Ok ())
        +|(Ok (
        +|  (test1.doc "<file test1.doc>")
        +|  (test2.doc "<file test2.doc>")))

          ==============
        -|<input type="file" accept=".doc,.docx" multiple="" #value="" @on_input> </input>
        +|<input type="file" accept=".doc,.docx" multiple="" @on_input> </input>
        |}]
    ;;

    let%expect_test "list: setting an empty map clears the input" =
      let component =
        Form.Elements.File_select.multiple
          ~accept:[ `Extension ".doc"; `Extension "docx" ]
          ()
      in
      let handle =
        Handle.create
          (form_result_spec [%sexp_of: Bonsai_web_ui_file.t Filename.Map.t])
          component
      in
      Handle.input_files
        handle
        ~selector:"input"
        ~files:[ mock_js_file ~name:"test1.doc"; mock_js_file ~name:"test2.doc" ];
      Handle.show handle;
      [%expect
        {|
        (Ok (
          (test1.doc "<file test1.doc>")
          (test2.doc "<file test2.doc>")))

        ==============
        <input type="file" accept=".doc,.docx" multiple="" @on_input> </input>
        |}];
      Handle.do_actions handle [ Filename.Map.empty ];
      Handle.show handle;
      [%expect
        {|
        (Ok ())

        ==============
        <input type="file" accept=".doc,.docx" multiple="" #value="" @on_input> </input>
        |}]
    ;;

    let%expect_test "list: setting a non-empty input emits a warning and is ignored" =
      let component =
        Form.Elements.File_select.multiple
          ~accept:[ `Extension ".doc"; `Extension "docx" ]
          ()
      in
      let handle =
        Handle.create
          (form_result_spec [%sexp_of: Bonsai_web_ui_file.t Filename.Map.t])
          component
      in
      Handle.input_files
        handle
        ~selector:"input"
        ~files:[ mock_js_file ~name:"test1.doc"; mock_js_file ~name:"test2.doc" ];
      Handle.show handle;
      [%expect
        {|
        (Ok (
          (test1.doc "<file test1.doc>")
          (test2.doc "<file test2.doc>")))

        ==============
        <input type="file" accept=".doc,.docx" multiple="" @on_input> </input>
        |}];
      Handle.do_actions
        handle
        [ Filename.Map.of_alist_exn
            [ "foo.docx", mock_bonsai_file ~name:"foo.docx"
            ; "bar.docx", mock_bonsai_file ~name:"bar.docx"
            ]
        ];
      Handle.show handle;
      (* Note: A warning is emitted and the value is unchanged. *)
      [%expect
        {|
        ("WARNING: Attempted to set the value of a file select to a value other than [Map.empty]. This is prohibited by the browser and therefore ignored."
         (files ((bar.docx "<file bar.docx>") (foo.docx "<file foo.docx>"))))
        (Ok (
          (test1.doc "<file test1.doc>")
          (test2.doc "<file test2.doc>")))

        ==============
        <input type="file" accept=".doc,.docx" multiple="" @on_input> </input>
        |}]
    ;;
  end)
;;

let%expect_test "on_change handler should fire when input is changed" =
  let component graph =
    let input =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
    in
    let%sub () =
      Form.Dynamic.on_change
        ~sexp_of_model:[%sexp_of: String.t]
        ~equal:[%equal: String.t]
        ~f:
          (Bonsai.return
           @@ fun new_value ->
           Ui_effect.print_s [%message "the input changed to" (new_value : string)])
        input
        graph
    in
    input
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>

    ("the input changed to" (new_value ""))
    |}];
  Handle.input_text handle ~selector:"input" ~text:"hello world";
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok "")
    +|(Ok "hello world")

      ==============
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
    -|       value:normalized=""
    +|       value:normalized="hello world"
             @on_input> </input>
    ("the input changed to" (new_value "hello world"))
    |}]
;;

let submit_test_attrs = function
  | "onsubmit" | "onclick" | "disabled" | "value:normalized" -> true
  | _ -> false
;;

let%expect_test "form validated with an effect" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f = tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.return in
  let component graph =
    let textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph in
    Form.Dynamic.validate_via_effect
      ~sexp_of_model:[%sexp_of: Int.t]
      textbox
      ~f
      ~equal:[%equal: Int.t]
      graph
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key:_ ~data:_ ->
         false))
      component
  in
  Handle.input_text handle ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok 2)

    ==============
    <input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"5";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~selector:"input" ~text:"-3";
  Handle.recompute_view_until_stable handle;
  print_queued ();
  [%expect {| (-3 5) |}];
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | 5 -> Respond (Ok ())
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect
    {|
    (Error "not positive")

    ==============
    <input> </input>
    |}]
;;

let%expect_test "form validated with an effect with one_at_at_time" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f = tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.return in
  let component graph =
    let textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph in
    Form.Dynamic.validate_via_effect
      ~sexp_of_model:[%sexp_of: Int.t]
      textbox
      ~f
      ~one_at_a_time:true
      ~equal:[%equal: Int.t]
      graph
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key:_ ~data:_ ->
         false))
      component
  in
  Handle.input_text handle ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok 2)

    ==============
    <input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"5";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~selector:"input" ~text:"20";
  Handle.recompute_view_until_stable handle;
  Handle.input_text handle ~selector:"input" ~text:"-3";
  Handle.recompute_view_until_stable handle;
  print_queued ();
  [%expect {| (5) |}];
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Q.maybe_respond tracker ~f:(function
    | 5 -> Respond (Ok ())
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (-3) |}];
  Q.maybe_respond tracker ~f:(function
    | -3 -> Respond not_positive
    | _ -> No_response_yet);
  Handle.show handle;
  [%expect
    {|
    (Error "not positive")

    ==============
    <input> </input>
    |}]
;;

let%expect_test "form validated with an effect and debounced" =
  let module Q = Effect.For_testing.Query_response_tracker in
  let tracker = Q.create () in
  let print_queued () = print_s [%sexp (Q.queries_pending_response tracker : int list)] in
  let not_positive = Error (Error.of_string "not positive") in
  let f = tracker |> Effect.For_testing.of_query_response_tracker |> Bonsai.return in
  let component graph =
    let textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph in
    Form.Dynamic.validate_via_effect
      ~debounce_ui:(Time_ns.Span.of_sec 1.0)
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      textbox
      ~f
      graph
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key:_ ~data:_ ->
         false))
      component
  in
  Handle.input_text handle ~selector:"input" ~text:"2";
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  print_queued ();
  [%expect {| (2) |}];
  Q.maybe_respond tracker ~f:(fun i ->
    Respond (if Int.is_positive i then Ok () else not_positive));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  (* this should be Ok, but it isn't due to the debounce *)
  [%expect
    {|
    (Error validating...)

    ==============
    <input> </input>
    |}];
  Handle.advance_clock_by handle (Time_ns.Span.of_sec 2.0);
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  (* now it's good! *)
  [%expect
    {|
    (Ok 2)

    ==============
    <input> </input>
    |}]
;;

let%expect_test "extending a projection with an error" =
  let component graph =
    let textbox =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
    in
    let%arr textbox = textbox in
    Form.project
      textbox
      ~parse_exn:Int.of_string
      ~unparse:Int.to_string
      ~extend_view_with_error:(fun _ error ->
        Vdom.Node.sexp_for_debugging
          [%message "An error! Hide the textbox" (error : Error.t)])
  in
  let handle = Handle.create (form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect
    {|
    (Error (Failure "Int.of_string: \"\""))

    ==============
    <pre> ("An error! Hide the textbox" (error (Failure "Int.of_string: \"\""))) </pre>
    |}];
  Handle.do_actions handle [ 1 ];
  Handle.show handle;
  [%expect
    {|
    (Ok 1)

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=1
           @on_input> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"not an int";
  Handle.show handle;
  [%expect
    {|
    (Error (Failure "Int.of_string: \"not an int\""))

    ==============
    <pre>
      ("An error! Hide the textbox"
     (error (Failure "Int.of_string: \"not an int\"")))
    </pre>
    |}]
;;

let%expect_test _ =
  let module Foo = struct
    type t =
      { x : int
      ; y : string
      }
    [@@deriving sexp_of, typed_fields]
  end
  in
  let form graph =
    Form.Typed.Record.make
      (module struct
        include Foo

        type field_view = Vdom.Node.t
        type resulting_view = Vdom.Node.t

        let form_for_field
          : type a. a Typed_field.t -> Bonsai.graph -> (a, field_view) Form.t Bonsai.t
          =
          fun typed_field graph ->
          match typed_field with
          | X ->
            Form.Elements.Number.int ~step:1 ~allow_updates_when_focused:`Never () graph
          | Y -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
        ;;

        type form_of_field_fn =
          { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Bonsai.t }

        let finalize_view { f } _graph =
          let view_x =
            let%arr x = f X in
            Form.view x
          in
          let view_y =
            let%arr y = f Y in
            Form.view y
          in
          let%arr view_x = view_x
          and view_y = view_y in
          Vdom.Node.div [ view_x; view_y ]
        ;;
      end)
      graph
  in
  let handle = Handle.create (form_result_spec [%sexp_of: Foo.t]) form in
  Handle.show handle;
  [%expect
    {|
    (Error ("in field x" "value not specified"))

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized="" @on_input> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=""
             @on_input> </input>
    </div>
    |}];
  Handle.do_actions handle [ { x = 1; y = "hello" } ];
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (x 1)
      (y hello)))

    ==============
    <div>
      <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=1 @on_input> </input>
      <input @key=bonsai_path_replaced_in_test
             type="text"
             placeholder=""
             spellcheck="false"
             value:normalized=hello
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "slider input" =
  let component =
    Form.Elements.Range.int
      ~min:0
      ~max:100
      ~default:0
      ~step:1
      ~allow_updates_when_focused:`Never
      ()
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: int] ~filter_printed_attributes:(fun ~key ~data:_ ->
         submit_test_attrs key))
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input value:normalized=0> </input>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"20";
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    (Ok 20)

    ==============
    <input value:normalized=20> </input>
    |}]
;;

let%expect_test "query box" =
  let var =
    Bonsai.Expert.Var.create (String.Map.of_alist_exn [ "abc", "abc"; "def", "def" ])
  in
  let value = Bonsai.Expert.Var.value var in
  let component graph =
    Form.Elements.Query_box.create
      (module String)
      ~selection_to_string:(Bonsai.return Fn.id)
      ~f:(fun query _graph ->
        let%arr query = query
        and value = value in
        Map.filter_map value ~f:(fun data ->
          if String.is_prefix ~prefix:query data then Some (Vdom.Node.text data) else None))
      ()
      graph
  in
  let handle =
    Handle.create
      (form_result_spec [%sexp_of: string] ~filter_printed_attributes:(fun ~key ~data:_ ->
         match key with
         | "style.color" -> true
         | _ -> false))
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <input> </input>
      <div>
        <div> </div>
      </div>
    </div>
    |}];
  Handle.input_text handle ~selector:"input" ~text:"a";
  Handle.recompute_view handle;
  Handle.keydown handle ~selector:"input" ~key:Enter;
  Handle.show_diff handle;
  [%expect
    {|
    ("default prevented" (key Enter))

    -|(Error "a value is required")
    +|(Ok abc)

      ==============
      <div>
        <input> </input>
        <div>
          <div> </div>
        </div>
      </div>
    |}];
  Bonsai.Expert.Var.update var ~f:(fun map -> Map.remove (map : _ String.Map.t) "abc");
  Handle.show_diff handle;
  [%expect {| |}]
;;

let%test_module "Typed" =
  (module struct
    let%test_module "Record" =
      (module struct
        let%expect_test "basic record" =
          let module T = struct
            type t =
              { a : int
              ; b : string
              ; c : bool
              }
            [@@deriving typed_fields, sexp]
          end
          in
          let component =
            Form.Typed.Record.make
              (module struct
                module Typed_field = T.Typed_field

                type field_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_field
                  : type a.
                    a Typed_field.t -> Bonsai.graph -> (a, field_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | A ->
                    Form.Elements.Number.int
                      ~step:1
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                  | B ->
                    Form.Elements.Textbox.string
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                  | C -> Form.Elements.Checkbox.bool ~default:false () graph
                ;;

                type form_of_field_fn =
                  { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Bonsai.t }

                let finalize_view { f } _graph =
                  let%arr a = f A
                  and b = f B
                  and c = f C in
                  let label_and_attr label form =
                    Vdom.Node.label
                      [ Vdom.Node.text label
                      ; Vdom.Node.span
                          ~attrs:[ Vdom.Attr.create "test" label ]
                          [ Form.view form ]
                      ]
                  in
                  Vdom.Node.div
                    [ label_and_attr "a" a; label_and_attr "b" b; label_and_attr "c" c ]
                ;;
              end)
          in
          let handle = Handle.create (form_result_spec [%sexp_of: T.t]) component in
          Handle.show handle;
          [%expect
            {|
            (Error ("in field a" "value not specified"))

            ==============
            <div>
              <label>
                a
                <span test="a">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
              <label>
                c
                <span test="c">
                  <input @key=bonsai_path_replaced_in_test
                         type="checkbox"
                         #checked="false"
                         @on_click
                         style={
                           margin-left: 0px;
                         }> </input>
                </span>
              </label>
            </div>
            |}];
          Handle.input_text handle ~selector:"[test=a] input" ~text:"3";
          Handle.input_text handle ~selector:"[test=b] input" ~text:"text";
          Handle.set_checkbox handle ~selector:"[test=c] input" ~checked:true;
          Handle.show handle;
          [%expect
            {|
            (Ok (
              (a 3)
              (b text)
              (c true)))

            ==============
            <div>
              <label>
                a
                <span test="a">
                  <input type="number" step="1" placeholder="" spellcheck="false" value:normalized=3 @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=text
                         @on_input> </input>
                </span>
              </label>
              <label>
                c
                <span test="c">
                  <input @key=bonsai_path_replaced_in_test
                         type="checkbox"
                         #checked="true"
                         @on_click
                         style={
                           margin-left: 0px;
                         }> </input>
                </span>
              </label>
            </div>
            |}];
          Handle.do_actions handle [ { a = 10; b = "hi there"; c = false } ];
          Handle.show handle;
          [%expect
            {|
            (Ok (
              (a 10)
              (b "hi there")
              (c false)))

            ==============
            <div>
              <label>
                a
                <span test="a">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=10
                         @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized="hi there"
                         @on_input> </input>
                </span>
              </label>
              <label>
                c
                <span test="c">
                  <input @key=bonsai_path_replaced_in_test
                         type="checkbox"
                         #checked="false"
                         @on_click
                         style={
                           margin-left: 0px;
                         }> </input>
                </span>
              </label>
            </div>
            |}]
        ;;

        let%expect_test "monomorphize record" =
          let module T = struct
            type 'a t = { a : 'a } [@@deriving typed_fields, sexp]
          end
          in
          let component =
            Form.Typed.Record.make
              (module struct
                module Typed_field = Typed_fields_lib.S_of_S1 (T.Typed_field) (Int)

                type field_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_field
                  : type a.
                    a Typed_field.t -> Bonsai.graph -> (a, field_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | A ->
                    Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
                ;;

                type form_of_field_fn =
                  { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Bonsai.t }

                let finalize_view { f } _graph =
                  let%arr a = f A in
                  Form.view a
                ;;
              end)
          in
          let handle = Handle.create (form_result_spec [%sexp_of: int T.t]) component in
          Handle.show handle;
          [%expect
            {|
            (Error ("in field a" "Expected an integer"))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
            |}];
          Handle.do_actions handle [ { a = 10 } ];
          Handle.show handle;
          [%expect
            {|
            (Ok ((a 10)))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=10
                   @on_input> </input>
            |}]
        ;;

        let%expect_test "calling [form_of_field_fn] multiple times gives the same value \
                         back"
          =
          let module T = struct
            type t =
              { a : int
              ; b : string
              }
            [@@deriving typed_fields, sexp]
          end
          in
          let component =
            Form.Typed.Record.make
              (module struct
                module Typed_field = T.Typed_field

                type field_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_field
                  : type a.
                    a Typed_field.t -> Bonsai.graph -> (a, field_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | A ->
                    Form.Elements.Number.int
                      ~step:1
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                  | B ->
                    Form.Elements.Textbox.string
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                ;;

                type form_of_field_fn =
                  { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Bonsai.t }

                let finalize_view { f } _graph =
                  let%arr a1 = f A
                  and a2 = f A
                  and b = f B in
                  let label_and_attr label form =
                    Vdom.Node.label
                      [ Vdom.Node.text label
                      ; Vdom.Node.span
                          ~attrs:[ Vdom.Attr.create "test" label ]
                          [ Form.view form ]
                      ]
                  in
                  Vdom.Node.div
                    [ label_and_attr "a1" a1
                    ; label_and_attr "a2" a2
                    ; label_and_attr "b" b
                    ]
                ;;
              end)
          in
          let handle = Handle.create (form_result_spec [%sexp_of: T.t]) component in
          Handle.show handle;
          [%expect
            {|
            (Error ("in field a" "value not specified"))

            ==============
            <div>
              <label>
                a1
                <span test="a1">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
              <label>
                a2
                <span test="a2">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
            </div>
            |}];
          (* Typing into either "a" textbox influences both; as does setting the form *)
          Handle.input_text handle ~selector:"[test=a1] input" ~text:"1234";
          Handle.show handle;
          [%expect
            {|
            (Ok (
              (a 1234)
              (b "")))

            ==============
            <div>
              <label>
                a1
                <span test="a1">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=1234
                         @on_input> </input>
                </span>
              </label>
              <label>
                a2
                <span test="a2">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=1234
                         @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
            </div>
            |}];
          Handle.input_text handle ~selector:"[test=a2] input" ~text:"4321";
          Handle.show handle;
          [%expect
            {|
            (Ok (
              (a 4321)
              (b "")))

            ==============
            <div>
              <label>
                a1
                <span test="a1">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=4321
                         @on_input> </input>
                </span>
              </label>
              <label>
                a2
                <span test="a2">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=4321
                         @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=""
                         @on_input> </input>
                </span>
              </label>
            </div>
            |}];
          Handle.do_actions handle [ { a = 1423; b = "foo" } ];
          Handle.show handle;
          [%expect
            {|
            (Ok (
              (a 1423)
              (b foo)))

            ==============
            <div>
              <label>
                a1
                <span test="a1">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=1423
                         @on_input> </input>
                </span>
              </label>
              <label>
                a2
                <span test="a2">
                  <input type="number"
                         step="1"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=1423
                         @on_input> </input>
                </span>
              </label>
              <label>
                b
                <span test="b">
                  <input @key=bonsai_path_replaced_in_test
                         type="text"
                         placeholder=""
                         spellcheck="false"
                         value:normalized=foo
                         @on_input> </input>
                </span>
              </label>
            </div>
            |}]
        ;;
      end)
    ;;

    let%test_module "Variant" =
      (module struct
        let%expect_test "basic variant - defaults to first option" =
          let module T = struct
            type t =
              | Foo of int
              | Bar of string
            [@@deriving typed_variants, sexp]
          end
          in
          let component =
            Form.Typed.Variant.make
              (module struct
                module Typed_variant = T.Typed_variant

                type picker_view = Vdom.Node.t
                type variant_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_picker =
                  Form.Elements.Dropdown.enumerable
                    (module struct
                      type t = Typed_variant.Packed.t
                      [@@deriving enumerate, equal, sexp, compare]
                    end)
                ;;

                let form_for_variant
                  : type a.
                    a Typed_variant.t -> Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | Foo ->
                    Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
                  | Bar ->
                    Form.Elements.Textbox.string
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                ;;

                let finalize_view picker_view selected _graph =
                  let inner_view =
                    match%arr selected with
                    | Error _ -> Vdom.Node.text ""
                    | Ok (_, inner) -> Form.view inner
                  in
                  let%arr inner_view = inner_view
                  and picker_view = picker_view in
                  Vdom.Node.div [ picker_view; inner_view ]
                ;;
              end)
          in
          let handle = Handle.create (form_result_spec [%sexp_of: T.t]) component in
          Handle.show handle;
          [%expect
            {|
            (Error "Expected an integer")

            ==============
            <div>
              <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
                <option value="0" #selected="true"> foo </option>
                <option value="1" #selected="false"> bar </option>
              </select>
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     value:normalized=""
                     @on_input> </input>
            </div>
            |}];
          Handle.input_text handle ~selector:"input" ~text:"1234";
          Handle.show handle;
          [%expect
            {|
            (Ok (Foo 1234))

            ==============
            <div>
              <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
                <option value="0" #selected="true"> foo </option>
                <option value="1" #selected="false"> bar </option>
              </select>
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     value:normalized=1234
                     @on_input> </input>
            </div>
            |}];
          Handle.change handle ~selector:"select" ~value:"1";
          Handle.recompute_view_until_stable handle;
          Handle.input_text handle ~selector:"input" ~text:"hi!";
          Handle.show handle;
          [%expect
            {|
            (Ok (Bar hi!))

            ==============
            <div>
              <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
                <option value="0" #selected="false"> foo </option>
                <option value="1" #selected="true"> bar </option>
              </select>
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     value:normalized=hi!
                     @on_input> </input>
            </div>
            |}]
        ;;

        let%expect_test "basic variant - can also default to no starting option selected" =
          let module T = struct
            type t =
              | Foo of int
              | Bar of string
            [@@deriving typed_variants, sexp]
          end
          in
          let component graph =
            Form.Typed.Variant.make
              (module struct
                module Typed_variant = T.Typed_variant

                type picker_view = Vdom.Node.t
                type variant_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_picker graph =
                  let picker =
                    Form.Elements.Dropdown.enumerable_opt
                      (module struct
                        type t = Typed_variant.Packed.t
                        [@@deriving enumerate, equal, sexp, compare]
                      end)
                      graph
                  in
                  let%arr picker = picker in
                  let value =
                    match Form.value picker with
                    | Error err -> Error err
                    | Ok None -> Or_error.error_s [%message "a value is required"]
                    | Ok (Some value) -> Ok value
                  in
                  let set x = Form.set picker (Some x) in
                  let view = Form.view picker in
                  { Form.value; view; set }
                ;;

                let form_for_variant
                  : type a.
                    a Typed_variant.t -> Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | Foo ->
                    Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
                  | Bar ->
                    Form.Elements.Textbox.string
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                ;;

                let finalize_view picker_view selected _graph =
                  let inner_view =
                    match%arr selected with
                    | Error _ -> Vdom.Node.text ""
                    | Ok (_, inner) -> Form.view inner
                  in
                  let%arr picker_view = picker_view
                  and inner_view = inner_view in
                  Vdom.Node.div [ picker_view; inner_view ]
                ;;
              end)
              graph
          in
          let handle = Handle.create (form_result_spec [%sexp_of: T.t]) component in
          Handle.show handle;
          [%expect
            {|
            (Error "a value is required")

            ==============
            <div>
              <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
                <option value="0" #selected="true">  </option>
                <option value="1" #selected="false"> foo </option>
                <option value="2" #selected="false"> bar </option>
              </select>

            </div>
            |}];
          Handle.change handle ~selector:"select" ~value:"1";
          Handle.recompute_view_until_stable handle;
          Handle.input_text handle ~selector:"input" ~text:"1234";
          Handle.show handle;
          [%expect
            {|
            (Ok (Foo 1234))

            ==============
            <div>
              <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
                <option value="0" #selected="false">  </option>
                <option value="1" #selected="true"> foo </option>
                <option value="2" #selected="false"> bar </option>
              </select>
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     value:normalized=1234
                     @on_input> </input>
            </div>
            |}];
          Handle.change handle ~selector:"select" ~value:"2";
          Handle.recompute_view_until_stable handle;
          Handle.input_text handle ~selector:"input" ~text:"hi!";
          Handle.show handle;
          [%expect
            {|
            (Ok (Bar hi!))

            ==============
            <div>
              <select @key=bonsai_path_replaced_in_test class="widget-dropdown" @on_change>
                <option value="0" #selected="false">  </option>
                <option value="1" #selected="false"> foo </option>
                <option value="2" #selected="true"> bar </option>
              </select>
              <input @key=bonsai_path_replaced_in_test
                     type="text"
                     placeholder=""
                     spellcheck="false"
                     value:normalized=hi!
                     @on_input> </input>
            </div>
            |}]
        ;;

        let%expect_test "monomorphize variant" =
          let module T = struct
            type 'a t = Foo of 'a [@@deriving typed_variants, sexp]
          end
          in
          let component =
            Form.Typed.Variant.make
              (module struct
                module Typed_variant = Typed_variants_lib.S_of_S1 (T.Typed_variant) (Int)

                type picker_view = unit
                type variant_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_picker _graph =
                  Bonsai.return (Form.return ({ f = T Foo } : Typed_variant.Packed.t))
                ;;

                let form_for_variant
                  : type a.
                    a Typed_variant.t -> Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | Foo ->
                    Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
                ;;

                let finalize_view (_ : unit Bonsai.t) foo_view _graph =
                  match%arr foo_view with
                  | Error _ -> raise_s [%message "impossible error"]
                  | Ok (_, inner) -> Form.view inner
                ;;
              end)
          in
          let handle = Handle.create (form_result_spec [%sexp_of: int T.t]) component in
          Handle.show handle;
          [%expect
            {|
            (Error "Expected an integer")

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=""
                   @on_input> </input>
            |}];
          Handle.do_actions handle [ Foo 5 ];
          Handle.show handle;
          [%expect
            {|
            ("Form.return was set, but setting is ignored."
             lib/bonsai/web_ui/form/test/bonsai_web_ui_form_manual_test.ml:LINE:COL)
            (Ok (Foo 5))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=5
                   @on_input> </input>
            |}]
        ;;

        let%expect_test "optional variant" =
          let module T = struct
            type t =
              | Foo of int
              | Bar of string
            [@@deriving typed_variants, sexp]
          end
          in
          let component =
            Form.Typed.Variant.make_optional
              (module struct
                module Typed_variant = T.Typed_variant

                type picker_view = unit
                type variant_view = Vdom.Node.t
                type resulting_view = Vdom.Node.t

                let form_for_picker =
                  Form.return_settable ~equal:[%equal: Typed_variant.Packed.t option] None
                ;;

                let form_for_variant
                  : type a.
                    a Typed_variant.t -> Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
                  =
                  fun typed_field graph ->
                  match typed_field with
                  | Foo ->
                    Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
                  | Bar ->
                    Form.Elements.Textbox.string
                      ~allow_updates_when_focused:`Never
                      ()
                      graph
                ;;

                let finalize_view (_ : unit Bonsai.t) variant_and_subform _graph =
                  match%arr variant_and_subform with
                  | Error _ -> raise_s [%message "impossible error"]
                  | Ok None -> Vdom.Node.none
                  | Ok (Some (_, inner)) -> Form.view inner
                ;;
              end)
          in
          let handle =
            Handle.create (form_result_spec [%sexp_of: T.t option]) component
          in
          Handle.show handle;
          [%expect
            {|
            (Ok ())

            ==============
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            |}];
          Handle.do_actions handle [ Some (Foo 5) ];
          Handle.show handle;
          [%expect
            {|
            (Ok ((Foo 5)))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=5
                   @on_input> </input>
            |}];
          Handle.do_actions handle [ Some (Bar "hi!") ];
          Handle.show handle;
          [%expect
            {|
            (Ok ((Bar hi!)))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="text"
                   placeholder=""
                   spellcheck="false"
                   value:normalized=hi!
                   @on_input> </input>
            |}];
          Handle.do_actions handle [ None ];
          Handle.show handle;
          [%expect
            {|
            (Ok ())

            ==============
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            |}]
        ;;
      end)
    ;;
  end)
;;

let%expect_test "difference between with_default and with_default_always" =
  let default = Bonsai.Expert.Var.create 10 in
  let should_show_form = Bonsai.Expert.Var.create true in
  let create_handle ~name with_default =
    let component graph =
      match%sub Bonsai.Expert.Var.value should_show_form with
      | true ->
        let form =
          Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
        in
        with_default (Bonsai.Expert.Var.value default) form graph
      | false ->
        Bonsai.return
          (Form.map_view (Form.return 0) ~f:(fun () ->
             Vdom.Node.none_deprecated [@alert "-deprecated"]))
    in
    Handle.create
      (Result_spec.string
         (module struct
           type t = (int, Vdom.Node.t) Form.t

           let to_string t =
             let sexp = [%sexp_of: int Or_error.t] (Form.value t) in
             [%string "%{name}: %{sexp#Sexp}"]
           ;;
         end))
      component
  in
  let with_default_handle =
    create_handle ~name:"with_default" Form.Dynamic.with_default
  in
  let with_default_always_handle =
    create_handle ~name:"with_default_always" Form.Dynamic.with_default_always
  in
  let show_both () =
    Handle.show with_default_handle;
    Handle.show with_default_always_handle
  in
  show_both ();
  [%expect
    {|
    with_default: (Error"Expected an integer")
    with_default_always: (Error"Expected an integer")
    |}];
  show_both ();
  [%expect
    {|
    with_default: (Ok 10)
    with_default_always: (Ok 10)
    |}];
  Bonsai.Expert.Var.set should_show_form false;
  show_both ();
  [%expect
    {|
    with_default: (Ok 0)
    with_default_always: (Ok 0)
    |}];
  Bonsai.Expert.Var.set default 15;
  Bonsai.Expert.Var.set should_show_form true;
  show_both ();
  [%expect
    {|
    with_default: (Ok 10)
    with_default_always: (Ok 10)
    |}];
  show_both ();
  [%expect
    {|
    with_default: (Ok 10)
    with_default_always: (Ok 15)
    |}];
  Bonsai.Expert.Var.set default 20;
  show_both ();
  [%expect
    {|
    with_default: (Ok 10)
    with_default_always: (Ok 15)
    |}];
  (* Observe that neither version updates form to match the new default value
     of 20, since they both only update the default on activate or model reset. *)
  show_both ();
  [%expect
    {|
    with_default: (Ok 10)
    with_default_always: (Ok 15)
    |}]
;;

let%expect_test "[Form.with_default] sets the form value after a model reset" =
  let default = Bonsai.Expert.Var.create 0 in
  let component graph =
    let state, reset =
      Bonsai.with_model_resetter
        ~f:(fun graph ->
          let form =
            Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
          in
          Form.Dynamic.with_default (Bonsai.Expert.Var.value default) form graph)
        graph
    in
    let%arr state = state
    and reset = reset in
    state, reset
  in
  let handle =
    Handle.create
      (module struct
        type t = (int, Vdom.Node.t) Form.t * unit Effect.t
        type incoming = unit

        let incoming (_, effect) () = effect

        let view (form, _) =
          Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
        ;;

        let to_vdom (form, _) = Form.view form
      end)
      component
  in
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "[Form.with_default_always] sets the form value after a model reset" =
  let default = Bonsai.Expert.Var.create 0 in
  let component graph =
    let state, reset =
      Bonsai.with_model_resetter
        ~f:(fun graph ->
          let form =
            Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
          in
          Form.Dynamic.with_default_always (Bonsai.Expert.Var.value default) form graph)
        graph
    in
    let%arr state = state
    and reset = reset in
    state, reset
  in
  let handle =
    Handle.create
      (module struct
        type t = (int, Vdom.Node.t) Form.t * unit Effect.t
        type incoming = unit

        let incoming (_, effect) () = effect

        let view (form, _) =
          Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
        ;;

        let to_vdom (form, _) = Form.view form
      end)
      component
  in
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}];
  Handle.do_actions handle [ () ];
  Handle.show handle;
  [%expect {| (Error "Expected an integer") |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "[Form.with_default_always] only sets the form once on first activation" =
  let default = Bonsai.Expert.Var.create 0 in
  let component graph =
    let form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph in
    let form_with_printing =
      let%arr form = form in
      { Form.view = Form.view form
      ; value = Form.value form
      ; set =
          (fun i ->
            let%bind.Effect () = Effect.print_s [%message "Form.set called"] in
            Form.set form i)
      }
    in
    Form.Dynamic.with_default_always
      (Bonsai.Expert.Var.value default)
      form_with_printing
      graph
  in
  let handle =
    Handle.create
      (module struct
        type t = (int, Vdom.Node.t) Form.t
        type incoming = Nothing.t

        let incoming _ = Nothing.unreachable_code
        let view form = Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
        let to_vdom = Form.view
      end)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (Error "Expected an integer")
    "Form.set called"
    |}];
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test {| [Form.with_default] interacts fine with [Handle.recompute_view_until_stable] |}
  =
  let default = Bonsai.Expert.Var.create 0 in
  let component graph =
    let form = Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph in
    Form.Dynamic.with_default (Bonsai.Expert.Var.value default) form graph
  in
  let handle =
    Handle.create
      (module struct
        type t = (int, Vdom.Node.t) Form.t
        type incoming = Nothing.t

        let incoming _ = Nothing.unreachable_code
        let view form = Sexp.to_string_hum ([%sexp_of: int Or_error.t] (Form.value form))
        let to_vdom = Form.view
      end)
      component
  in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "[Form.return] is not settable" =
  let component _graph = Bonsai.return (Form.return 5) in
  let handle = Handle.create (viewless_form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect {| (Ok 5) |}];
  Handle.do_actions handle [ 10 ];
  Handle.show handle;
  [%expect
    {|
    ("Form.return was set, but setting is ignored."
     lib/bonsai/web_ui/form/test/bonsai_web_ui_form_manual_test.ml:LINE:COL)
    (Ok 5)
    |}]
;;

let%expect_test "return_settable" =
  let component =
    Form.return_settable ~sexp_of_model:[%sexp_of: Int.t] 5 ~equal:[%equal: Int.t]
  in
  let handle = Handle.create (viewless_form_result_spec [%sexp_of: int]) component in
  Handle.show handle;
  [%expect {| (Ok 5) |}];
  Handle.do_actions handle [ 10 ];
  Handle.show handle;
  [%expect {| (Ok 10) |}]
;;

let%expect_test "Checkbox.set layout options" =
  let print_handle layout =
    let component =
      Form.Elements.Checkbox.set
        (module String)
        ~layout
        (Bonsai.return [ "first"; "second" ])
    in
    let handle = Handle.create (form_result_spec [%sexp_of: String.Set.t]) component in
    Handle.show handle
  in
  print_handle `Vertical;
  let print_diff = Expect_test_patdiff.diff_printer (Some [%expect.output]) in
  [%expect
    {|
    (Ok ())

    ==============
    <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          first
        </label>
      </li>
      <li style={ display: block; }>
        <label>
          <input type="checkbox" #checked="false" @on_click> </input>
          second
        </label>
      </li>
    </ul>
    |}];
  print_handle `Horizontal;
  unstage print_diff [%expect.output];
  [%expect
    {|
    -1,18 +1,18
      (Ok ())

      ==============
      <ul class="checkbox-container widget-checklist" style={ list-style: none; margin-left: 0px; }>
    -|  <li style={ display: block; }>
    +|  <li style={ display: inline-block; }>
          <label>
            <input type="checkbox" #checked="false" @on_click> </input>
            first
          </label>
        </li>
    -|  <li style={ display: block; }>
    +|  <li style={ display: inline-block; }>
          <label>
            <input type="checkbox" #checked="false" @on_click> </input>
            second
          </label>
        </li>
      </ul>
    |}]
;;

let%expect_test "query box set to value not in all_options" =
  let component =
    Form.Elements.Query_box.single
      (module String)
      ~all_options:(Bonsai.return [ "a"; "b" ])
      ~handle_unknown_option:(Bonsai.return (fun _ -> None))
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.do_actions handle [ "c" ];
  Handle.show handle;
  [%expect
    {|
    (Ok c)

    ==============
    <div>
      <input id="bonsai_path_replaced_in_test"
             type="text"
             placeholder="c"
             class="input_hash_replaced_in_test"
             #value=""
             @on_blur
             @on_focus
             @on_input
             @on_keydown> </input>
      <div data-test="query-box-item-container"
           id="bonsai_path_replaced_in_test"
           tabindex="-1"
           @on_blur
           @on_wheel
           style={
             position: relative;
           }>
        <div> </div>
      </div>
    </div>
    |}]
;;

let%expect_test "labelling a range form" =
  let label text = Some (Vdom.Node.text text) in
  let no_label = None in
  let all_options =
    [ no_label, no_label, "No labels"
    ; no_label, label "right", "Right label only"
    ; label "left", no_label, "Left label only"
    ; label "left", label "right", "Both sides labelled"
    ]
  in
  List.iter all_options ~f:(fun (left_label, right_label, description) ->
    print_endline description;
    print_endline "###############";
    let range =
      Form.Elements.Range.int
        ?left_label
        ?right_label
        ~default:0
        ~step:1
        ~allow_updates_when_focused:`Never
        ()
    in
    let handle = Handle.create (form_result_spec [%sexp_of: int]) range in
    Handle.show handle);
  [%expect
    {|
    No labels
    ###############
    (Ok 0)

    ==============
    <input type="range"
           step="1"
           placeholder=""
           spellcheck="false"
           min="0"
           max="100"
           value:normalized=0
           @on_input> </input>

    Right label only
    ###############
    (Ok 0)

    ==============
    <span style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
      <input type="range"
             step="1"
             placeholder=""
             spellcheck="false"
             min="0"
             max="100"
             value:normalized=0
             @on_input> </input>
      right
    </span>

    Left label only
    ###############
    (Ok 0)

    ==============
    <span style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
      left
      <input type="range"
             step="1"
             placeholder=""
             spellcheck="false"
             min="0"
             max="100"
             value:normalized=0
             @on_input> </input>
    </span>

    Both sides labelled
    ###############
    (Ok 0)

    ==============
    <span style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
      left
      <input type="range"
             step="1"
             placeholder=""
             spellcheck="false"
             min="0"
             max="100"
             value:normalized=0
             @on_input> </input>
      right
    </span>
    |}]
;;

let%test_module "Querybox as typeahead" =
  (module struct
    module Data = struct
      module T = struct
        type t =
          | Option_A
          | Option_B
          | Option_C
        [@@deriving variants, enumerate, sexp, equal, compare]
      end

      include T

      let to_string = function
        | Option_A -> "Option A"
        | Option_B -> "Option B"
        | Option_C -> "Option C"
      ;;

      include Comparable.Make (T)
    end

    let shared_computation ?(to_string = Bonsai.return Data.to_string) () =
      Form.Elements.Query_box.single_opt
        (module Data)
        ~all_options:(Bonsai.return Data.all)
        ~to_string
    ;;

    let view_computation ?to_string () graph =
      let form = shared_computation ?to_string () graph in
      let%arr form = form in
      Form.view form
    ;;

    let view_and_inject_computation graph =
      let form = shared_computation () graph in
      let%arr form = form in
      Form.view form, Form.set form
    ;;

    let%expect_test "Initial typeahead state" =
      let handle = Handle.create (Result_spec.vdom ()) (view_computation ()) in
      Handle.show handle;
      [%expect
        {|
        <div>
          <input id="bonsai_path_replaced_in_test"
                 type="text"
                 class="input_hash_replaced_in_test"
                 #value=""
                 @on_blur
                 @on_focus
                 @on_input
                 @on_keydown> </input>
          <div data-test="query-box-item-container"
               id="bonsai_path_replaced_in_test"
               tabindex="-1"
               @on_blur
               @on_wheel
               style={
                 position: relative;
               }>
            <div> </div>
          </div>
        </div>
        |}]
    ;;

    let%expect_test "Change typeahead contents" =
      let handle = Handle.create (Result_spec.vdom ()) (view_computation ()) in
      Handle.store_view handle;
      Handle.input_text handle ~selector:"input" ~text:(Data.to_string Data.Option_C);
      Handle.show_diff handle;
      [%expect
        {|
          <div>
            <input id="bonsai_path_replaced_in_test"
                   type="text"
                   class="input_hash_replaced_in_test"
        -|         #value=""
        +|         #value="Option C"
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
        -|    <div> </div>
        +|    <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
        +|      <div class="selected_item_hash_replaced_in_test" @on_click @on_mouseenter>
        +|        <span> Option C </span>
        +|      </div>
        +|    </div>
            </div>
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
              let module V = (val Result_spec.vdom ()) in
              V.view vdom
            ;;

            let to_vdom (vdom, _) = vdom
            let incoming (_, inject) = inject
          end)
          view_and_inject_computation
      in
      Handle.store_view handle;
      Handle.do_actions handle [ Some Data.Option_A ];
      Handle.show_diff handle;
      [%expect
        {|
          <div>
            <input id="bonsai_path_replaced_in_test"
                   type="text"
        +|         placeholder="Option A"
                   class="input_hash_replaced_in_test"
                   #value=""
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
              <div> </div>
            </div>
        |}];
      Handle.do_actions handle [ None ];
      Handle.show_diff handle;
      [%expect
        {|
          <div>
            <input id="bonsai_path_replaced_in_test"
                   type="text"
        -|         placeholder="Option A"
                   class="input_hash_replaced_in_test"
                   #value=""
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
              <div> </div>
            </div>
        |}]
    ;;

    let%expect_test "Select element using partial input" =
      let handle = Handle.create (Result_spec.vdom ()) (view_computation ()) in
      Handle.show handle;
      [%expect
        {|
        <div>
          <input id="bonsai_path_replaced_in_test"
                 type="text"
                 class="input_hash_replaced_in_test"
                 #value=""
                 @on_blur
                 @on_focus
                 @on_input
                 @on_keydown> </input>
          <div data-test="query-box-item-container"
               id="bonsai_path_replaced_in_test"
               tabindex="-1"
               @on_blur
               @on_wheel
               style={
                 position: relative;
               }>
            <div> </div>
          </div>
        </div>
        |}];
      (* "O" is not unique, all options are matched. *)
      Handle.input_text handle ~selector:"input" ~text:"O";
      Handle.show_diff handle;
      [%expect
        {|
          <div>
            <input id="bonsai_path_replaced_in_test"
                   type="text"
                   class="input_hash_replaced_in_test"
        -|         #value=""
        +|         #value="O"
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
        -|    <div> </div>
        +|    <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
        +|      <div class="selected_item_hash_replaced_in_test" @on_click @on_mouseenter>
        +|        <span> Option A </span>
        +|      </div>
        +|      <div @on_click @on_mouseenter>
        +|        <span> Option B </span>
        +|      </div>
        +|      <div @on_click @on_mouseenter>
        +|        <span> Option C </span>
        +|      </div>
        +|    </div>
            </div>
          </div>
        |}];
      Handle.input_text handle ~selector:"input" ~text:"C";
      Handle.show_diff handle;
      [%expect
        {|
          <div>
            <input id="bonsai_path_replaced_in_test"
                   type="text"
                   class="input_hash_replaced_in_test"
        -|         #value="O"
        +|         #value="C"
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
              <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
                <div class="selected_item_hash_replaced_in_test" @on_click @on_mouseenter>
        -|        <span> Option A </span>
        -|      </div>
        -|      <div @on_click @on_mouseenter>
        -|        <span> Option B </span>
        -|      </div>
        -|      <div @on_click @on_mouseenter>
                  <span> Option C </span>
                </div>
              </div>
            </div>
          </div>
        |}]
    ;;

    let%expect_test "dynamic [to_string]." =
      let to_string_var = Bonsai.Expert.Var.create Data.to_string in
      let to_string = Bonsai.Expert.Var.value to_string_var in
      let handle = Handle.create (Result_spec.vdom ()) (view_computation ~to_string ()) in
      Handle.input_text handle ~selector:"input" ~text:"";
      Handle.store_view handle;
      Bonsai.Expert.Var.set to_string_var (fun data -> Data.to_string data ^ "!");
      Handle.show_diff handle;
      [%expect
        {|
                   class="input_hash_replaced_in_test"
                   #value=""
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
              <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
                <div class="selected_item_hash_replaced_in_test" @on_click @on_mouseenter>
        -|        <span> Option A </span>
        +|        <span> Option A! </span>
                </div>
                <div @on_click @on_mouseenter>
        -|        <span> Option B </span>
        +|        <span> Option B! </span>
                </div>
                <div @on_click @on_mouseenter>
        -|        <span> Option C </span>
        +|        <span> Option C! </span>
                </div>
              </div>
            </div>
          </div>
        |}]
    ;;

    let%expect_test "Handle unknown option" =
      let computation graph =
        let form =
          Form.Elements.Query_box.single_opt
            (module Data)
            ~all_options:(Bonsai.return Data.all)
            ~handle_unknown_option:
              (Bonsai.return (fun _ ->
                 print_endline "in handle_uknown_option";
                 Some Data.Option_A))
            graph
        in
        let%arr form = form in
        Form.view form
      in
      let handle = Handle.create (Result_spec.vdom ()) computation in
      Handle.store_view handle;
      [%expect {| |}];
      Handle.input_text handle ~selector:"input" ~text:"unknown option";
      Handle.show_diff handle;
      [%expect
        {|
        in handle_uknown_option

          <div>
            <input id="bonsai_path_replaced_in_test"
                   type="text"
                   class="input_hash_replaced_in_test"
        -|         #value=""
        +|         #value="unknown option"
                   @on_blur
                   @on_focus
                   @on_input
                   @on_keydown> </input>
            <div data-test="query-box-item-container"
                 id="bonsai_path_replaced_in_test"
                 tabindex="-1"
                 @on_blur
                 @on_wheel
                 style={
                   position: relative;
                 }>
        -|    <div> </div>
        +|    <div class="list_container_hash_replaced_in_test" style={ position: absolute; }>
        +|      <div class="selected_item_hash_replaced_in_test" @on_click @on_mouseenter>
        +|        <span> Option_A </span>
        +|      </div>
        +|    </div>
            </div>
          </div>
        |}]
    ;;
  end)
;;

let%expect_test "adding to/setting/removing from a Form.Elements.Multiple.list" =
  let component =
    Form.Elements.Multiple.list
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle = Handle.create (list_form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    |}];
  Handle.do_actions handle [ `Append; `Append; `Append ];
  Handle.show handle;
  [%expect
    {|
    (Ok ("" "" ""))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ `Set [ "foo"; "bar"; "baz" ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (foo bar baz))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=foo
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=bar
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=baz
           @on_input> </input>
    |}];
  (* [`Remove 1] should get rid of the entry with "bar" in it. *)
  Handle.do_actions handle [ `Remove 1 ];
  Handle.show handle;
  [%expect
    {|
    (Ok (foo baz))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=foo
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=baz
           @on_input> </input>
    |}]
;;

let%expect_test "two setters in the same frame" =
  let component =
    Form.Elements.Multiple.list
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle = Handle.create (list_form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    |}];
  Handle.do_actions handle [ `Set [ "a"; "b" ]; `Set [] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    |}]
;;

let%expect_test "three setters in the same frame" =
  let component =
    Form.Elements.Multiple.list
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle = Handle.create (list_form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    |}];
  Handle.do_actions handle [ `Set [ "a"; "b" ]; `Set [ "c" ]; `Set [ "d"; "e"; "f" ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (d e f))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=d
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=e
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=f
           @on_input> </input>
    |}]
;;

let%expect_test "a textbox customized with themes" =
  let component =
    View.Expert.override_theme_for_computation
      ~f:(fun (module M) ->
        (module struct
          class c =
            object
              inherit M.c

              method! textbox
                ?(attrs = [])
                ?placeholder
                ?key
                ~allow_updates_when_focused:_
                ~disabled:_
                ~value
                ~set_value
                () =
                (* This is a silly implementation, for testing purposes *)
                let placeholder =
                  match placeholder with
                  | None -> Vdom.Attr.empty
                  | Some placeholder ->
                    Vdom.Attr.placeholder
                      [%string "Even better placeholder: %{placeholder}"]
                in
                View.vbox
                  [ Vdom.Node.span ~attrs [ Vdom.Node.text "Attrs here!" ]
                  ; Vdom.Node.input
                      ?key:(Option.map key ~f:(fun k -> [%string "%{k}-1"]))
                      ~attrs:
                        [ placeholder
                        ; Vdom.Attr.on_input (fun _ s ->
                            Effect.print_s [%message "ignoring input:" ~_:(s : string)])
                        ]
                      ()
                  ; Vdom.Node.input
                      ?key:(Option.map key ~f:(fun k -> [%string "%{k}-2"]))
                      ~attrs:
                        [ Vdom.Attr.on_input (fun _ s -> set_value s)
                        ; Vdom.Attr.value_prop value
                        ]
                      ()
                  ]
            end
        end))
      (Form.Elements.Textbox.string
         ~extra_attrs:(Bonsai.return [ Vdom.Attr.class_ "very-important" ])
         ~placeholder:(Bonsai.return "placeholder")
         ~allow_updates_when_focused:`Never
         ())
  in
  let handle = Handle.create (form_result_spec [%sexp_of: string]) component in
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <div style={ display: flex; flex-direction: column; }>
      <span class="very-important"> Attrs here! </span>
      <input @key=bonsai_path_replaced_in_test-1
             placeholder="Even better placeholder: placeholder"
             @on_input> </input>
      <input @key=bonsai_path_replaced_in_test-2 #value="" @on_input> </input>
    </div>
    |}];
  (* In our silly implementation, typing into the first input box does nothing! *)
  Handle.input_text handle ~text:"cool text" ~selector:"input:nth-child(2)";
  Handle.show handle;
  [%expect
    {|
    ("ignoring input:" "cool text")
    (Ok "")

    ==============
    <div style={ display: flex; flex-direction: column; }>
      <span class="very-important"> Attrs here! </span>
      <input @key=bonsai_path_replaced_in_test-1
             placeholder="Even better placeholder: placeholder"
             @on_input> </input>
      <input @key=bonsai_path_replaced_in_test-2 #value="" @on_input> </input>
    </div>
    |}];
  (* But, typing into the second input is hooked up to the state *)
  Handle.input_text handle ~text:"really cool text" ~selector:"input:nth-child(3)";
  Handle.show handle;
  [%expect
    {|
    (Ok "really cool text")

    ==============
    <div style={ display: flex; flex-direction: column; }>
      <span class="very-important"> Attrs here! </span>
      <input @key=bonsai_path_replaced_in_test-1
             placeholder="Even better placeholder: placeholder"
             @on_input> </input>
      <input @key=bonsai_path_replaced_in_test-2 #value="really cool text" @on_input> </input>
    </div>
    |}];
  (* And, setting behaves as you'd expect *)
  Handle.do_actions handle [ "the coolest text" ];
  Handle.show handle;
  [%expect
    {|
    (Ok "the coolest text")

    ==============
    <div style={ display: flex; flex-direction: column; }>
      <span class="very-important"> Attrs here! </span>
      <input @key=bonsai_path_replaced_in_test-1
             placeholder="Even better placeholder: placeholder"
             @on_input> </input>
      <input @key=bonsai_path_replaced_in_test-2 #value="the coolest text" @on_input> </input>
    </div>
    |}]
;;

let%expect_test "adding to/setting/removing from a Form.Elements.Multiple.nonempty_list" =
  let component =
    Form.Elements.Multiple.nonempty_list
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle =
    Handle.create (nonempty_list_form_result_spec [%sexp_of: string]) component
  in
  Handle.show handle;
  [%expect
    {|
    (Ok (""))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ `Append; `Append; `Append ];
  Handle.show handle;
  [%expect
    {|
    (Ok ("" "" "" ""))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ `Set (Nonempty_list.create "hi!" [ "foo"; "bar"; "baz" ]) ];
  Handle.show handle;
  [%expect
    {|
    (Ok (hi! foo bar baz))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=hi!
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=foo
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=bar
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=baz
           @on_input> </input>
    |}];
  Handle.do_actions handle [ `Remove 1 ];
  Handle.show handle;
  [%expect
    {|
    (Ok (hi! bar baz))

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=hi!
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=bar
           @on_input> </input>
    --------------
    <input @key=bonsai_path_replaced_in_test
           type="text"
           placeholder=""
           spellcheck="false"
           value:normalized=baz
           @on_input> </input>
    |}]
;;

let%test_module "Form.Typed.Record.make_table" =
  (module struct
    module T = struct
      type t =
        { a : int
        ; b : string
        }
      [@@deriving typed_fields, sexp]
    end

    let component =
      Form.Typed.Record.make_table
        (module struct
          module Typed_field = T.Typed_field

          let label_for_field : type a. a Typed_field.t -> string = function
            | A -> "Alpha"
            | B -> "Bravo"
          ;;

          let label_for_field = `Computed label_for_field

          let form_for_field
            : type a. a Typed_field.t -> Bonsai.graph -> (a, Vdom.Node.t) Form.t Bonsai.t
            =
            fun typed_field graph ->
            match typed_field with
            | A -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
            | B ->
              Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
          ;;
        end)
    ;;

    let%expect_test "interacting with the table" =
      let handle =
        Handle.create
          (form_result_spec
             ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
             [%sexp_of: T.t list])
          component
      in
      Handle.show handle;
      [%expect
        {|
        (Ok ())

        ==============
        <div>
          <table>
            <thead>
              <tr>
                <th> Alpha </th>
                <th> Bravo </th>
                <th> Remove </th>
              </tr>
            </thead>
            <tbody> </tbody>
          </table>
          <button> + </button>
        </div>
        |}];
      Handle.click_on handle ~selector:"button";
      Handle.show handle;
      [%expect
        {|
        (Error "Expected an integer")

        ==============
        <div>
          <table>
            <thead>
              <tr>
                <th> Alpha </th>
                <th> Bravo </th>
                <th> Remove </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <input> </input>
                </td>
                <td>
                  <input> </input>
                </td>
                <td>
                  <div>
                    <button> X </button>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
          <button> + </button>
        </div>
        |}];
      Handle.input_text handle ~selector:"td:nth-child(1) > input" ~text:"123";
      Handle.input_text handle ~selector:"td:nth-child(2) > input" ~text:"abc";
      Handle.show handle;
      [%expect
        {|
        (Ok ((
          (a 123)
          (b abc))))

        ==============
        <div>
          <table>
            <thead>
              <tr>
                <th> Alpha </th>
                <th> Bravo </th>
                <th> Remove </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <input> </input>
                </td>
                <td>
                  <input> </input>
                </td>
                <td>
                  <div>
                    <button> X </button>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
          <button> + </button>
        </div>
        |}]
    ;;

    let%expect_test "setting into the table" =
      let handle =
        Handle.create
          (form_result_spec
             ~filter_printed_attributes:(fun ~key:_ ~data:_ -> false)
             [%sexp_of: T.t list])
          component
      in
      Handle.do_actions
        handle
        [ [ { T.a = 1; b = "a" }; { a = 2; b = "b" }; { a = 3; b = "c" } ] ];
      Handle.show handle;
      [%expect
        {|
        (Ok (
          ((a 1) (b a))
          ((a 2) (b b))
          ((a 3) (b c))))

        ==============
        <div>
          <table>
            <thead>
              <tr>
                <th> Alpha </th>
                <th> Bravo </th>
                <th> Remove </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>
                  <input> </input>
                </td>
                <td>
                  <input> </input>
                </td>
                <td>
                  <div>
                    <button> X </button>
                  </div>
                </td>
              </tr>
              <tr>
                <td>
                  <input> </input>
                </td>
                <td>
                  <input> </input>
                </td>
                <td>
                  <div>
                    <button> X </button>
                  </div>
                </td>
              </tr>
              <tr>
                <td>
                  <input> </input>
                </td>
                <td>
                  <input> </input>
                </td>
                <td>
                  <div>
                    <button> X </button>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
          <button> + </button>
        </div>
        |}]
    ;;
  end)
;;
