open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Import

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

let sexp_form_handle
  (type a)
  ?optimize
  ?get_vdom
  ?customizations
  ?allow_duplication_of_list_items
  (module M : S with type t = a)
  =
  let form =
    Auto_generated.form
      (module M)
      ?customizations
      ?allow_duplication_of_list_items
      ~allow_updates_when_focused:`Never
  in
  Handle.create ?optimize (form_result_spec ?get_vdom M.sexp_of_t) form
;;

let%expect_test "nothing form" =
  let module T = struct
    type t = | [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "no grammars in union")

    ==============
    <div> </div>
    |}]
;;

let%expect_test "Union like Css_gen form" =
  let module T = struct
    type global =
      [ `Inherit
      | `Initial
      ]
    [@@deriving sexp, sexp_grammar]

    type t =
      [ `Var of string
      | global
      ]
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> inherit </option>
      <option value="2" #selected="false"> initial </option>
      <option value="3" #selected="false"> var </option>
    </select>
    |}]
;;

let%expect_test "setting option form" =
  let module T = struct
    type t = int option [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}];
  Handle.do_actions handle [ Some 3 ];
  Handle.show handle;
  [%expect
    {|
    (Ok (3))

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="checkbox"
             id="bonsai_path_replaced_in_test"
             #checked="true"
             @on_click
             style={
               margin-left: 0px;
             }> </input>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             @on_input> </input>
    </div>
    |}];
  Handle.do_actions handle [ None ];
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}]
;;

let%expect_test "interacting with option form" =
  let module T = struct
    type t = int option [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}];
  Handle.set_checkbox handle ~get_vdom ~selector:"input" ~checked:true;
  Handle.show handle;
  [%expect
    {|
    (Ok (0))

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="checkbox"
             id="bonsai_path_replaced_in_test"
             #checked="true"
             @on_click
             style={
               margin-left: 0px;
             }> </input>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input:nth-child(2)" ~text:"3";
  Handle.show handle;
  [%expect
    {|
    (Ok (3))

    ==============
    <div>
      <input @key=bonsai_path_replaced_in_test
             type="checkbox"
             id="bonsai_path_replaced_in_test"
             #checked="true"
             @on_click
             style={
               margin-left: 0px;
             }> </input>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "record's field order is preserved" =
  (* In this example, the fields are in alphabetical order. *)
  let module T = struct
    type t =
      { a : int
      ; b : int
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b 0)))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> b </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  (* In this example, the fields are not in alphabetical order *)
  let module T = struct
    type t =
      { b : int
      ; a : int
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (b 0)
      (a 0)))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> b </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "a tuple form gets labels on the elements" =
  let module T = struct
    type t = int * string [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (0 ""))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> 1st </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> 2nd </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "a list field within a record form gets a label" =
  let module T = struct
    type t =
      { a : int
      ; b : string list
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ((a 0) (b ())))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> b </label>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "a nested record gets a label" =
  let module T = struct
    type r =
      { c : string
      ; d : int
      }
    [@@deriving sexp, sexp_grammar]

    type t =
      { a : int
      ; b : r
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b (
        (c "")
        (d 0)))))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> a </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> b </label>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 2em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> c </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 2em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> d </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=0
                   @on_input> </input>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "record with doc comments gets a tooltip" =
  let module T = struct
    type t = { b : string (** doc comment *) }
    [@@deriving sexp, sexp_grammar ~tags_of_doc_comments]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok ((b "")))

    ==============
    <table>
      <tbody>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> b </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
              <div class="container_hash_replaced_in_test">
                <label class="label_hash_replaced_in_test" style={ color: blue; }>
                  <input type="checkbox" tabindex="-1" class="checkbox_hash_replaced_in_test"> </input>
                  <span class="span_hash_replaced_in_test"> ⓘ </span>
                  <div class="above_hash_replaced_in_test text_hash_replaced_in_test"
                       style={
                         border: 1px solid darkblue;
                         color: black;
                         background-color: azure;
                       }>  doc comment  </div>
                </label>
              </div>

            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "variant with doc comments gets a tooltip" =
  let module T = struct
    type t =
      | Foo (** This is a foo! *)
      | Bar (** This is a bar! *)
    [@@deriving sexp, sexp_grammar ~tags_of_doc_comments]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.do_actions handle [ Foo ];
  (* Note: we see there's a tooltip with "This is a foo!" inside *)
  Handle.show handle;
  [%expect
    {|
    (Ok Foo)

    ==============
    <table>
      <tbody>
        <tr>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <div style={ display: flex; }>
              <select id="bonsai_path_replaced_in_test"
                      class="widget-dropdown"
                      @on_change
                      style={
                        width: 100.00%;
                      }>
                <option value="0" #selected="false">  </option>
                <option value="1" #selected="true"> foo </option>
                <option value="2" #selected="false"> bar </option>
              </select>
              <span class="inline_padding_hash_replaced_in_test right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
                ?
                <div class="scrollable_tooltip_hash_replaced_in_test tooltip_hash_replaced_in_test"
                     custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
                  <div style={ display: flex; flex-direction: column; row-gap: 0.15rem; }>
                    <div style={ display: flex; flex-direction: column; }>
                      <span class="bold_text_hash_replaced_in_test"> Bar </span>
                       This is a bar!
                    </div>
                    <div style={ display: flex; flex-direction: column; }>
                      <span class="bold_text_hash_replaced_in_test"> Foo </span>
                       This is a foo!
                    </div>
                  </div>
                </div>
              </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.do_actions handle [ Bar ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Ok Foo)
    +|(Ok Bar)

      ==============
      <table>
        <tbody>
          <tr>
            <td style={
                  padding-left: 0em;
                  padding-right: 1em;
                  text-align: left;
                  font-weight: bold;
                  user-select: none;
                }>  </td>
            <td>
              <div style={ display: flex; }>
                <select id="bonsai_path_replaced_in_test"
                        class="widget-dropdown"
                        @on_change
                        style={
                          width: 100.00%;
                        }>
                  <option value="0" #selected="false">  </option>
    -|            <option value="1" #selected="true"> foo </option>
    +|            <option value="1" #selected="false"> foo </option>
    -|            <option value="2" #selected="false"> bar </option>
    +|            <option value="2" #selected="true"> bar </option>
                </select>
                <span class="inline_padding_hash_replaced_in_test right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
                  ?
                  <div class="scrollable_tooltip_hash_replaced_in_test tooltip_hash_replaced_in_test"
                       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
                    <div style={ display: flex; flex-direction: column; row-gap: 0.15rem; }>
                      <div style={ display: flex; flex-direction: column; }>
                        <span class="bold_text_hash_replaced_in_test"> Bar </span>
                         This is a bar!
                      </div>
                      <div style={ display: flex; flex-direction: column; }>
                        <span class="bold_text_hash_replaced_in_test"> Foo </span>
                         This is a foo!
                      </div>
                    </div>
                  </div>
    |}]
;;

let%expect_test "setting variant forms" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}];
  Handle.do_actions handle [ T.A ];
  Handle.show handle;
  [%expect
    {|
    (Ok A)

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="true"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}];
  Handle.do_actions handle [ T.B 2 ];
  Handle.show handle;
  [%expect
    {|
    (Ok (B 2))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=2
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "variants with only atom clauses and no doc comments get optimized, but \
                 look identical"
  =
  let module T = struct
    type t =
      | Foo
      | Bar
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Ok Foo)

    ==============
    <select @key=bonsai_path_replaced_in_test
            id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true"> Foo </option>
      <option value="1" #selected="false"> Bar </option>
    </select>
    |}]
;;

let%expect_test "interacting with variant forms" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok A)

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="false">  </option>
      <option value="1" #selected="true"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 0))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"3";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 3))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             @on_input> </input>
    </div>
    |}]
;;

let%expect_test "setting recursive variant form" =
  let module M = struct
    type t =
      | Empty
      | Cons of string * t
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> empty </option>
      <option value="2" #selected="false"> cons </option>
    </select>
    |}];
  Handle.do_actions handle [ Cons ("hello", Cons ("there", Cons ("world", Empty))) ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Cons hello (Cons there (Cons world Empty))))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=there @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=world @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div>
    |}]
;;

(* The difference between this test and the previous one is the order in which the
   constructors are listed. This test puts the recursive variant constructor first, while
   the previous test put it second. *)
let%expect_test "setting recursive variant form 2" =
  let module M = struct
    type t =
      | Cons of string * t
      | Empty
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> cons </option>
      <option value="2" #selected="false"> empty </option>
    </select>
    |}];
  Handle.do_actions handle [ Cons ("hello", Cons ("there", Cons ("world", Empty))) ];
  Handle.show handle;
  [%expect
    {|
    (Ok (Cons hello (Cons there (Cons world Empty))))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> cons </option>
        <option value="2" #selected="false"> empty </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> cons </option>
        <option value="2" #selected="false"> empty </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=there @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> cons </option>
        <option value="2" #selected="false"> empty </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=world @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> cons </option>
        <option value="2" #selected="true"> empty </option>
      </select>
    </div>
    |}]
;;

let%expect_test "interacting with recursive variant form" =
  let module M = struct
    type t =
      | Empty
      | Cons of string * t
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> empty </option>
      <option value="2" #selected="false"> cons </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"textarea" ~text:"hello";
  Handle.change handle ~get_vdom ~selector:"select:nth-child(3)" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="true">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"textarea:nth-child(4)" ~text:"world";
  Handle.change handle ~get_vdom ~selector:"select:nth-child(5)" ~value:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok (Cons hello (Cons world Empty)))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hello @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> empty </option>
        <option value="2" #selected="true"> cons </option>
      </select>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=world @on_input> </textarea>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> empty </option>
        <option value="2" #selected="false"> cons </option>
      </select>
    </div>
    |}]
;;

let%expect_test "setting custom time form" =
  let customize_time =
    Auto_generated.Customization.constant_form
      (module Time_ns.Alternate_sexp)
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
      (Form.Elements.Date_time.datetime_local ~allow_updates_when_focused:`Never ())
  in
  let handle =
    sexp_form_handle ~customizations:[ customize_time ] (module Time_ns.Alternate_sexp)
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.do_actions handle [ Time_ns.of_string_with_utc_offset "2022-04-05 13:31:55Z" ];
  Handle.show handle;
  [%expect
    {|
    (Ok "2022-04-05 13:31:55Z")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2022-04-05T13:31:55
           @on_input> </input>
    |}]
;;

let%expect_test "interacting with custom time form" =
  let customize_time =
    Auto_generated.Customization.constant_form
      (module Time_ns.Alternate_sexp)
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
      (Form.Elements.Date_time.datetime_local ~allow_updates_when_focused:`Never ())
  in
  let handle =
    sexp_form_handle ~customizations:[ customize_time ] (module Time_ns.Alternate_sexp)
  in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=""
           @on_input> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2022-04-05T09:31:55";
  Handle.show handle;
  [%expect
    {|
    (Ok "2022-04-05 09:31:00Z")

    ==============
    <input type="datetime-local"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2022-04-05T09:31:00
           @on_input> </input>
    |}]
;;

let%expect_test "duplicating list elements" =
  let module M = struct
    type t = int list [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.do_actions handle [ [ 1; 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (1 2))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 0 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 1 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.click_on handle ~selector:"button:nth-child(3)" ~get_vdom:get_vdom_detailed;
  (* Note that the duplicate element is added beside the one that was duplicated. *)
  Handle.show handle;
  [%expect
    {|
    (Ok (1 1 2))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 0 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 1 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 2 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

(* In case people find it too noisy to display a duplicate button on every single list
   element, or it causes slowness, they can opt-out with a flag. *)
let%expect_test "opting out of duplication in lists" =
  let module M = struct
    type t = int list [@@deriving sexp, sexp_grammar]
  end
  in
  let handle =
    sexp_form_handle
      ~allow_duplication_of_list_items:false
      ~get_vdom:get_vdom_detailed
      (module M)
  in
  Handle.do_actions handle [ [ 1; 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (1 2))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      @on_click
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              1 -
              <button type="button"
                      @on_click
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "customizing a tuple within a list" =
  let module M = struct
    module Pair = struct
      type t = (int * int[@tag Sexplib0.Sexp_grammar.type_name_tag = Atom "my_pair"])
      [@@deriving sexp, sexp_grammar]
    end

    type t = Pair.t list [@@deriving sexp, sexp_grammar]
  end
  in
  let customize_pair =
    Auto_generated.Customization.transform_form
      ~apply_to_tag:(fun ~key ~value ->
        String.equal key Sexplib0.Sexp_grammar.type_name_tag
        && Sexp.equal value ([%sexp_of: string] "my_pair"))
      (fun (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t) ~recurse graph ->
        let grammar =
          let%arr with_tag = with_tag in
          with_tag.grammar
        in
        match%sub (grammar : Sexplib0.Sexp_grammar.grammar Bonsai.t) with
        | List (Cons (first, Cons (second, Empty))) ->
          let first = recurse first graph in
          let second = recurse second graph in
          let%arr first = first
          and second = second in
          let view =
            Form.View.tuple
              [ Form.view (Form.label "Key" first); Form.view (Form.label "Data" second) ]
          in
          let value =
            match Or_error.both (Form.value first) (Form.value second) with
            | Ok (first, second) -> Ok (Sexp.List [ first; second ])
            | Error _ as err -> err
          in
          let set sexp =
            match sexp with
            | Sexp.List [ first_val; second_val ] ->
              Effect.Many [ Form.set first first_val; Form.set second second_val ]
            | _ -> Effect.Ignore
          in
          Form.Expert.create ~value ~view ~set
        | _ -> recurse grammar graph)
  in
  let handle =
    sexp_form_handle
      ~get_vdom:get_vdom_detailed
      ~customizations:[ customize_pair ]
      (module M)
  in
  Handle.do_actions handle [ [ 1, 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((1 2)))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 0 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Key </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Data </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "customizing an alist" =
  let module M = struct
    type t = (string, int) List.Assoc.t [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.do_actions handle [ [ "hi", 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((hi 2)))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      @on_click
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Key </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hi @on_input> </textarea>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Data </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "customizing a map" =
  let module M = struct
    type t = int Map.M(String).t [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.do_actions handle [ String.Map.of_alist_exn [ "hi", 2 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((hi 2)))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div>
              0 -
              <button type="button"
                      @on_click
                      style={
                        border: none;
                        cursor: pointer;
                        color: blue;
                        background: none;
                      }> [ remove ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Key </label>
          </td>
          <td>
            <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=hi @on_input> </textarea>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>
            <label for="bonsai_path_replaced_in_test" style={ display: block; }> Data </label>
          </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=2
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> Add new element </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "optional fields are not rendered at first, and the default is selected" =
  let module M = struct
    type t = { a : int [@default 0] } [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ((a 0)))

    ==============
    <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" @on_click> [default] </div>
    |}]
;;

let%expect_test "interacting with an optional field" =
  let module M = struct
    type t =
      { a : int [@default 0]
      ; b : string
      }
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b "")))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" @on_click> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"textarea" ~text:"foo";
  Handle.recompute_view handle;
  Handle.click_on handle ~get_vdom ~selector:"div > div";
  Handle.recompute_view handle;
  Handle.input_text handle ~get_vdom ~selector:"div > input" ~text:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 1)
      (b foo)))

    ==============
    <div>
      <div class="override_showing_hash_replaced_in_test override_text_hash_replaced_in_test"
           @on_click> [override] </div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=1
             @on_input> </input>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=foo @on_input> </textarea>
    </div>
    |}]
;;

let%expect_test "setting into an optional field without the value in the sexp" =
  let module M = struct
    type t =
      { a : int [@default 0]
      ; b : string
      }
    [@@deriving sexp_grammar, sexp]

    let sexp_of_t { b; _ } = Sexp.List [ List [ Atom "b"; Atom b ] ]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ((b "")))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" @on_click> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
    </div>
    |}];
  Handle.do_actions handle [ { a = 1; b = "foo" } ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((b foo)))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" @on_click> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=foo @on_input> </textarea>
    </div>
    |}]
;;

let%expect_test "setting into an optional field with the value" =
  let module M = struct
    type t =
      { a : int [@default 0]
      ; b : string
      }
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 0)
      (b "")))

    ==============
    <div>
      <div class="override_hidden_hash_replaced_in_test override_text_hash_replaced_in_test" @on_click> [default] </div>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
    </div>
    |}];
  Handle.do_actions handle [ { a = 1; b = "foo" } ];
  Handle.show handle;
  [%expect
    {|
    (Ok (
      (a 1)
      (b foo)))

    ==============
    <div>
      <div class="override_showing_hash_replaced_in_test override_text_hash_replaced_in_test"
           @on_click> [override] </div>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=1
             @on_input> </input>
      <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized=foo @on_input> </textarea>
    </div>
    |}]
;;

let%expect_test "regression test: optional groups have keys attached" =
  (* This test verifies that checkbox inputs for optional fields have a key attached to
     them when they are clicked. This prevents a bug in the vdom library's patching code
     that can trigger if there are consecutive inputs without keys. *)
  let module M = struct
    type record =
      { one : string
      ; two : string option
      ; three : string option
      }
    [@@deriving sexp, sexp_grammar]

    type t = { record : record } [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:Auto_generated.view_as_vdom (module M) in
  Handle.store_view handle;
  Handle.set_checkbox
    handle
    ~get_vdom:get_vdom_detailed
    ~selector:"tr:nth-child(4) input"
    ~checked:true;
  Handle.show_diff handle;
  (* Observe that when the checkbox is checked, there is still a key on the [td]
     surrounding it. *)
  [%expect
    {|
    -|(Ok ((
    -|  record (
    -|    (one "")
    -|    (two   ())
    -|    (three ())))))
    +|(Ok ((record ((one "") (two ()) (three (""))))))

      ==============
      <table class="form_hash_replaced_in_test">
        <tbody>
          <tr>
            <td>
              <div colspan="2" class="container_hash_replaced_in_test label_hash_replaced_in_test"> record </div>
            </td>
          </tr>
          <tr>
            <td colspan="100">
              <table class="mod_depth_2_hash_replaced_in_test nested_table_hash_replaced_in_test">
                <tr @key=bonsai_path_replaced_in_test>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">
                      <label for="bonsai_path_replaced_in_test" style={ display: block; }> one </label>

                           }> </input>
                  </td>
                </tr>
                <tr>
                  <td colspan="100">
                    <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test"> </table>
                  </td>
                </tr>
                <tr>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test"> three </div>
                  </td>
                  <td>
                    <input @key=bonsai_path_replaced_in_test
                           type="checkbox"
                           id="bonsai_path_replaced_in_test"
    -|                     #checked="false"
    +|                     #checked="true"
                           @on_click
                           style={
                             margin-left: 0px;
                           }> </input>
                  </td>
                </tr>
                <tr>
                  <td colspan="100">
    -|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test"> </table>
    +|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test">
    +|                <tr @key=bonsai_path_replaced_in_test>
    +|                  <td>
    +|                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">  </div>
    +|                  </td>
    +|                  <td>
    +|                    <textarea placeholder=""
    +|                              id="bonsai_path_replaced_in_test"
    +|                              value:normalized=""
    +|                              @on_input> </textarea>
    +|                  </td>
    +|                </tr>
    +|              </table>
                  </td>
                </tr>
              </table>
            </td>
          </tr>
        </tbody>
      </table>
    |}];
  Handle.set_checkbox
    handle
    ~get_vdom:get_vdom_detailed
    ~selector:"tr:nth-child(3) input"
    ~checked:true;
  Handle.show_diff handle;
  (* Observe that when the checkbox is checked, there is still a key on the [td]
     surrounding it. *)
  [%expect
    {|
    -|(Ok ((record ((one "") (two ()) (three (""))))))
    +|(Ok ((
    +|  record (
    +|    (one "")
    +|    (two   (""))
    +|    (three (""))))))

      ==============
      <table class="form_hash_replaced_in_test">
        <tbody>
          <tr>
            <td>
              <div colspan="2" class="container_hash_replaced_in_test label_hash_replaced_in_test"> record </div>
            </td>
          </tr>
          <tr>
            <td colspan="100">
              <table class="mod_depth_2_hash_replaced_in_test nested_table_hash_replaced_in_test">
                <tr @key=bonsai_path_replaced_in_test>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">
                      <label for="bonsai_path_replaced_in_test" style={ display: block; }> one </label>

                  </td>
                  <td>
                    <textarea placeholder=""
                              id="bonsai_path_replaced_in_test"
                              value:normalized=""
                              @on_input> </textarea>
                  </td>
                </tr>
                <tr>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test"> two </div>
                  </td>
                  <td>
                    <input @key=bonsai_path_replaced_in_test
                           type="checkbox"
                           id="bonsai_path_replaced_in_test"
    -|                     #checked="false"
    +|                     #checked="true"
                           @on_click
                           style={
                             margin-left: 0px;
                           }> </input>
                  </td>
                </tr>
                <tr>
                  <td colspan="100">
    -|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test"> </table>
    +|              <table class="mod_depth_3_hash_replaced_in_test nested_table_hash_replaced_in_test">
    +|                <tr @key=bonsai_path_replaced_in_test>
    +|                  <td>
    +|                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test">  </div>
    +|                  </td>
    +|                  <td>
    +|                    <textarea placeholder=""
    +|                              id="bonsai_path_replaced_in_test"
    +|                              value:normalized=""
    +|                              @on_input> </textarea>
    +|                  </td>
    +|                </tr>
    +|              </table>
                  </td>
                </tr>
                <tr>
                  <td>
                    <div class="container_hash_replaced_in_test label_hash_replaced_in_test"> three </div>
                  </td>
                  <td>
                    <input @key=bonsai_path_replaced_in_test
                           type="checkbox"
                           id="bonsai_path_replaced_in_test"
                           #checked="true"
                           @on_click
                           style={
                             margin-left: 0px;
                           }> </input>
                  </td>
    |}]
;;

let%expect_test "dynamic sexp_grammar form is rendered" =
  let grammar = [%sexp_grammar: int] in
  let grammar' = [%sexp_grammar: bool] in
  let grammar_var = Bonsai.Expert.Var.create grammar.untyped in
  let grammar_value = Bonsai.Expert.Var.value grammar_var in
  let form = Auto_generated.form' grammar_value ~allow_updates_when_focused:`Never in
  let handle = Handle.create (form_result_spec Fn.id) form in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           @on_input> </input>
    |}];
  Bonsai.Expert.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}]
;;

let%expect_test "interactions with dynamic sexp_grammar form" =
  let grammar = [%sexp_grammar: int] in
  let grammar' = [%sexp_grammar: bool] in
  let grammar_var = Bonsai.Expert.Var.create grammar.untyped in
  let grammar_value = Bonsai.Expert.Var.value grammar_var in
  let form = Auto_generated.form' grammar_value ~allow_updates_when_focused:`Never in
  let handle = Handle.create (form_result_spec Fn.id) form in
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           @on_input> </input>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok 2)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2
           @on_input> </input>
    |}];
  Bonsai.Expert.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}];
  Handle.set_checkbox handle ~get_vdom ~selector:"input" ~checked:true;
  Handle.show handle;
  [%expect
    {|
    (Ok true)

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="true"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}];
  (* We remember the value of the form before the grammar was changed *)
  Bonsai.Expert.Var.set grammar_var grammar.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok 2)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=2
           @on_input> </input>
    |}]
;;

let%expect_test "setting into a dynamic grammar form works, but should be done with care" =
  let grammar = [%sexp_grammar: int] in
  let grammar' = [%sexp_grammar: bool] in
  let grammar_var = Bonsai.Expert.Var.create grammar.untyped in
  let grammar_value = Bonsai.Expert.Var.value grammar_var in
  let form = Auto_generated.form' grammar_value ~allow_updates_when_focused:`Never in
  let handle = Handle.create (form_result_spec Fn.id) form in
  (* Everything is good as long as you set sexps that match the current grammar *)
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           @on_input> </input>
    |}];
  Handle.do_actions handle [ [%sexp_of: int] 5 ];
  Handle.show handle;
  [%expect
    {|
    (Ok 5)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=5
           @on_input> </input>
    |}];
  Bonsai.Expert.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="false"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}];
  Handle.do_actions handle [ [%sexp_of: bool] true ];
  Handle.show handle;
  [%expect
    {|
    (Ok true)

    ==============
    <input @key=bonsai_path_replaced_in_test
           type="checkbox"
           id="bonsai_path_replaced_in_test"
           #checked="true"
           @on_click
           style={
             margin-left: 0px;
           }> </input>
    |}];
  (* But if you set something that doesn't match the current grammar, [on_set_error] will
     be called *)
  Handle.do_actions handle [ [%sexp_of: int] 4 ];
  [%expect
    {|
    ("BUG: Sexp representation of set form value does not match sexp grammar. Does your sexp_of_t function match your sexp grammar?"
     (value 4)
     (error
      ("invalid bool"
       (reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 4))))))
    |}]
;;

let%expect_test "customizations are applied dynamically" =
  let grammar = [%sexp_grammar: int] in
  let grammar' =
    Sexplib0.Sexp_grammar.tag [%sexp_grammar: string] ~key:"key" ~value:(Sexp.List [])
  in
  let grammar_var = Bonsai.Expert.Var.create grammar.untyped in
  let grammar_value = Bonsai.Expert.Var.value grammar_var in
  let customization =
    Auto_generated.Customization.constant_form
      (module String)
      ~apply_to_tag:(fun ~key ~value:_ -> String.equal key "key")
      (Form.Elements.Textarea.string ~allow_updates_when_focused:`Never ())
  in
  let form =
    Auto_generated.form'
      ~customizations:[ customization ]
      grammar_value
      ~allow_updates_when_focused:`Never
  in
  let handle = Handle.create (form_result_spec Fn.id) form in
  (* No customizations are applied to the original grammar *)
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           @on_input> </input>
    |}];
  (* When the grammar transitions to a grammar which customizations are defined for, those
     customizations are applied *)
  Bonsai.Expert.Var.set grammar_var grammar'.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok "")

    ==============
    <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
    |}];
  (* And transitioning back still does not apply customizations *)
  Bonsai.Expert.Var.set grammar_var grammar.untyped;
  Handle.show handle;
  [%expect
    {|
    (Ok 0)

    ==============
    <input type="number"
           step="1"
           placeholder=""
           spellcheck="false"
           id="bonsai_path_replaced_in_test"
           value:normalized=0
           @on_input> </input>
    |}]
;;

let%expect_test "customizing a list to have better button text" =
  let module M = struct
    type t = (int list[@tag "grammar.add_element_text" = Atom "add new integer"])
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ())

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <button type="button" @on_click> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.do_actions handle [ [ 1 ] ];
  Handle.show handle;
  [%expect
    {|
    (Ok (1))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 0em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 0 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 1em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "customizing a list in a record to have better button text" =
  let module M = struct
    type t = { a : (int list[@tag "grammar.add_element_text" = Atom "add new integer"]) }
    [@@deriving sexp_grammar, sexp]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module M) in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (Ok ((a ())))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> a </label>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <button type="button" @on_click> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.do_actions handle [ { a = [ 1 ] } ];
  Handle.show handle;
  [%expect
    {|
    (Ok ((a (1))))

    ==============
    <table>
      <tbody>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <label style={ display: block; }> a </label>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 1em; font-weight: bold; }>
            <div style={ display: flex; column-gap: 0.50em; }>
              <span> 0 -  </span>
              <button type="button" @on_click style={ color: blue; }> [ remove ] </button>
              <button type="button" @on_click style={ color: blue; }> [ duplicate ] </button>
            </div>
          </td>
        </tr>
        <tr @key=bonsai_path_replaced_in_test>
          <td style={
                padding-left: 2em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <input type="number"
                   step="1"
                   placeholder=""
                   spellcheck="false"
                   id="bonsai_path_replaced_in_test"
                   value:normalized=1
                   @on_input> </input>
          </td>
        </tr>
        <tr>
          <td colspan="2" style={ padding-left: 2em; font-weight: bold; }>
            <button type="button" @on_click> add new integer </button>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;

let%expect_test "model state is not shared between variants even when they have \
                 identical arguments"
  =
  let module T = struct
    type t =
      | A of int
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <select id="bonsai_path_replaced_in_test"
            class="widget-dropdown"
            @on_change
            style={
              width: 100.00%;
            }>
      <option value="0" #selected="true">  </option>
      <option value="1" #selected="false"> a </option>
      <option value="2" #selected="false"> b </option>
    </select>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.recompute_view handle;
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok (A 2))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> a </option>
        <option value="2" #selected="false"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=2
             @on_input> </input>
    </div>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"2";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 0))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=0
             @on_input> </input>
    </div>
    |}];
  Handle.input_text handle ~get_vdom ~selector:"input" ~text:"3";
  Handle.show handle;
  [%expect
    {|
    (Ok (B 3))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="false"> a </option>
        <option value="2" #selected="true"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=3
             @on_input> </input>
    </div>
    |}];
  Handle.change handle ~get_vdom ~selector:"select" ~value:"1";
  Handle.show handle;
  [%expect
    {|
    (Ok (A 2))

    ==============
    <div>
      <select id="bonsai_path_replaced_in_test"
              class="widget-dropdown"
              @on_change
              style={
                width: 100.00%;
              }>
        <option value="0" #selected="false">  </option>
        <option value="1" #selected="true"> a </option>
        <option value="2" #selected="false"> b </option>
      </select>
      <input type="number"
             step="1"
             placeholder=""
             spellcheck="false"
             id="bonsai_path_replaced_in_test"
             value:normalized=2
             @on_input> </input>
    </div>
    |}]
;;

let%test_module "Stabilization tests" =
  (module struct
    let filter_don't_stabilize output =
      String.split_lines output
      |> List.filter ~f:(fun line -> String.equal line "stabilized")
      |> String.concat ~sep:"\n"
      |> print_endline
    ;;

    let%expect_test "A simple record doesn't stabilize" =
      let module T = struct
        type t =
          { a : int
          ; b : string
          ; c : bool
          }
        [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ { T.a = 10; b = "hello world"; c = true } ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {| |}]
    ;;

    let%expect_test "A list stabilizes once" =
      let module T = struct
        type t = int list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ [ 1; 2; 3; 4 ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {| stabilized |}]
    ;;

    let%expect_test "A variant stabilizes once" =
      let module T = struct
        type t =
          | A of int
          | B of bool
        [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ B false ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect {| stabilized |}]
    ;;

    let%expect_test "Nested lists and variants stabilize linearly with the depth of the \
                     type (depth 2)"
      =
      let module T = struct
        type s =
          | A of int
          | B of bool

        and t = s list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ [ A 1; B false; B true; A 4 ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        |}]
    ;;

    let%expect_test "Nested lists and variants stabilize linearly with the depth of the \
                     type (depth 3)"
      =
      let module T = struct
        type r =
          | A of int
          | B of bool

        and s = r list

        and t =
          | C of s
          | D of float
        [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ C [ A 1; B false; B true; A 4 ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        stabilized
        |}]
    ;;

    let%expect_test "Nested lists and variants stabilize linearly with the depth of the \
                     type (depth 4)"
      =
      let module T = struct
        type q =
          | A of int
          | B of bool

        and r = q list

        and s =
          | C of r
          | D of float

        and t = s list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions
        handle
        [ [ C [ A 1; B false; B true; A 4 ]; D 1.; C [ B true; B false; A 1; A 10 ] ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        stabilized
        stabilized
        |}]
    ;;

    let%expect_test "maps!" =
      let module S = struct
        module T = struct
          type t =
            | None
            | Some of t
          [@@deriving compare, sexp, sexp_grammar]
        end

        include T
        include Comparable.Make (T)
      end
      in
      let module T = struct
        type s = S.t Map.M(S).t list
        and t = s list [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      let m =
        S.Map.of_alist_exn
          S.[ None, None; Some None, Some None; Some (Some None), Some (Some None) ]
      in
      Handle.do_actions handle [ [ [ m; m ]; [ m; m ] ] ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        stabilized
        stabilized
        stabilized
        |}]
    ;;

    let%expect_test "records with defaults!" =
      let module T = struct
        type s =
          | My of int
          | Your of string
          | Our of bool

        and r =
          { b : s [@sexp.default My 10]
          ; a : bool [@sexp.default false]
          ; c : int [@sexp.default 0]
          ; aa : int [@sexp.default 3]
          }

        and t = r [@@deriving sexp, sexp_grammar]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.print_stabilizations handle;
      Handle.do_actions handle [ { c = 1; aa = 4; b = Your "hi"; a = true } ];
      Handle.recompute_view_until_stable handle;
      filter_don't_stabilize [%expect.output];
      [%expect
        {|
        stabilized
        stabilized
        |}]
    ;;
  end)
;;

(* This module attempts to exercise some of the trickier recursive cases. They are based
   on the tests in [lib/sexp_grammar/test/test_regression.ml]. *)
let%test_module "regressions" =
  (module struct
    module type S = sig
      type t [@@deriving sexp_grammar, sexp]

      val example : t
    end

    let test (module T : S) =
      let handle = sexp_form_handle (module T) in
      Handle.show handle;
      Handle.do_actions handle [ T.example ];
      Handle.show handle
    ;;

    (* Grammar validation should not fail when a type variable appears inside
       the body type expression of a recursion expression, e.g.,
       ... (Recursive (Tycon r ((Tyvar a))) ...) ... *)
    let%expect_test "tyvar inside recursion body" =
      test
        (module struct
          type 'a recursive = { self : 'a recursive option }
          [@@deriving sexp_grammar, sexp]

          type 'b recursive_with_reference =
            { this : 'b recursive_with_reference option
            ; that : 'b recursive
            }
          [@@deriving sexp_grammar, sexp]

          type t = int recursive_with_reference [@@deriving sexp_grammar, sexp]

          let example =
            { this = Some { this = None; that = { self = None } }
            ; that = { self = Some { self = None } }
            }
          ;;
        end);
      [%expect
        {|
        (Ok ((this ()) (that ((self ())))))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
        </div>

        (Ok ((this (((this ()) (that ((self ())))))) (that ((self (((self ()))))))))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="true"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="true"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
        </div>
        |}]
    ;;

    (* Grammar validation should not fail when an earlier-defined type constructor
       appears inside the body type expression of a recursion expression, e.g.,
       ... (Recursive (Tycon l ((Tycon t ()))) ...) ... *)
    let%expect_test "tycon inside recursion body" =
      test
        (module struct
          type 'a u = U of 'a u option [@@deriving quickcheck, sexp, sexp_grammar]
          type t = T of t u [@@deriving quickcheck, sexp, sexp_grammar]

          let example = T (U (Some (U None)))
        end);
      [%expect
        {|
        (Error "a value is required")

        ==============
        <select id="bonsai_path_replaced_in_test"
                class="widget-dropdown"
                @on_change
                style={
                  width: 100.00%;
                }>
          <option value="0" #selected="true">  </option>
          <option value="1" #selected="false"> t </option>
        </select>

        (Ok (T (U ((U ())))))

        ==============
        <div>
          <select id="bonsai_path_replaced_in_test"
                  class="widget-dropdown"
                  @on_change
                  style={
                    width: 100.00%;
                  }>
            <option value="0" #selected="false">  </option>
            <option value="1" #selected="true"> t </option>
          </select>
          <select id="bonsai_path_replaced_in_test"
                  class="widget-dropdown"
                  @on_change
                  style={
                    width: 100.00%;
                  }>
            <option value="0" #selected="false">  </option>
            <option value="1" #selected="true"> u </option>
          </select>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="true"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <select id="bonsai_path_replaced_in_test"
                  class="widget-dropdown"
                  @on_change
                  style={
                    width: 100.00%;
                  }>
            <option value="0" #selected="false">  </option>
            <option value="1" #selected="true"> u </option>
          </select>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
        </div>
        |}]
    ;;

    (* This test shows a case where a type can refer to another type
       of the same base name. *)
    let%expect_test "tycon inside recursion body with same base name" =
      test
        (module struct
          module T = struct
            type 'a t = { this : 'a t option } [@@deriving quickcheck, sexp, sexp_grammar]
          end

          type t = { that : t T.t } [@@deriving quickcheck, sexp, sexp_grammar]

          let example = { that = { this = Some { this = None } } }
        end);
      [%expect
        {|
        (Ok ((that ((this ())))))

        ==============
        <input @key=bonsai_path_replaced_in_test
               type="checkbox"
               id="bonsai_path_replaced_in_test"
               #checked="false"
               @on_click
               style={
                 margin-left: 0px;
               }> </input>

        (Ok ((that ((this (((this ()))))))))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="true"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
        </div>
        |}]
    ;;

    (* This test shows a case where a recursive type can transitively depend on another type
       of the same name where no explicit namespace qualification happens in the definition. *)
    let%expect_test "tycon inside recursion body with same explicitly qualified name" =
      test
        (module struct
          module T = struct
            type 'a t = { this : 'a t option } [@@deriving quickcheck, sexp, sexp_grammar]
            type 'a u = 'a t [@@deriving quickcheck, sexp, sexp_grammar]
          end

          open T

          type t = { that : t u } [@@deriving quickcheck, sexp, sexp_grammar]

          let example = { that = { this = Some { this = None } } }
        end);
      [%expect
        {|
        (Ok ((that ((this ())))))

        ==============
        <input @key=bonsai_path_replaced_in_test
               type="checkbox"
               id="bonsai_path_replaced_in_test"
               #checked="false"
               @on_click
               style={
                 margin-left: 0px;
               }> </input>

        (Ok ((that ((this (((this ()))))))))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="true"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
        </div>
        |}]
    ;;

    (* This test shows a case where a type can transitively depend on another type
       which has the same name in (essentially) the same scope. *)
    let%expect_test "tycon inside recursion body with same fully qualified name" =
      test
        (module struct
          open struct
            type 'a t = { this : 'a t option } [@@deriving quickcheck, sexp, sexp_grammar]
            type 'a u = 'a t [@@deriving quickcheck, sexp, sexp_grammar]
          end

          type t = { that : t u } [@@deriving quickcheck, sexp, sexp_grammar]

          let example = { that = { this = Some { this = None } } }
        end);
      [%expect
        {|
        (Ok ((that ((this ())))))

        ==============
        <input @key=bonsai_path_replaced_in_test
               type="checkbox"
               id="bonsai_path_replaced_in_test"
               #checked="false"
               @on_click
               style={
                 margin-left: 0px;
               }> </input>

        (Ok ((that ((this (((this ()))))))))

        ==============
        <div>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="true"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }> </input>
        </div>
        |}]
    ;;

    (* This regression test contains a grammar which contains a recursive call that would
       overwrite the value of the Tyvar "a" in the environment to be (Tyvar "a"). Interpreting
       this grammar incorrectly would lead to an infinite recursion (look up "a", which
       results in looking up "a", ad infinitum).

       Instead, we should interpret the type variables in their original environment. *)
    let%expect_test "silly list with recursive type" =
      let module T = struct
        type 'a t' =
          | Empty
          | Cons of
              { left : 'a t'
              ; value : 'a
              }
        [@@deriving sexp, sexp_grammar]

        type t = int t' [@@deriving sexp_grammar, sexp]
      end
      in
      let handle = sexp_form_handle (module T) in
      Handle.do_actions handle [ Cons { left = Empty; value = 1 } ];
      Handle.show handle;
      [%expect
        {|
        (Ok (
          Cons
          (left  Empty)
          (value 1)))

        ==============
        <div>
          <select id="bonsai_path_replaced_in_test"
                  class="widget-dropdown"
                  @on_change
                  style={
                    width: 100.00%;
                  }>
            <option value="0" #selected="false">  </option>
            <option value="1" #selected="false"> empty </option>
            <option value="2" #selected="true"> cons </option>
          </select>
          <select id="bonsai_path_replaced_in_test"
                  class="widget-dropdown"
                  @on_change
                  style={
                    width: 100.00%;
                  }>
            <option value="0" #selected="false">  </option>
            <option value="1" #selected="true"> empty </option>
            <option value="2" #selected="false"> cons </option>
          </select>
          <input type="number"
                 step="1"
                 placeholder=""
                 spellcheck="false"
                 id="bonsai_path_replaced_in_test"
                 value:normalized=1
                 @on_input> </input>
        </div>
        |}]
    ;;
  end)
;;
