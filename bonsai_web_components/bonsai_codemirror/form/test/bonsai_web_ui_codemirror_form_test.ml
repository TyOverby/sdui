open! Core
open Bonsai_web_test
open Bonsai_web
module Codemirror_form = Bonsai_web_ui_codemirror_form.With_automatic_view
module Codemirror_test = Bonsai_web_ui_codemirror_test
module Codemirror_ui = Bonsai_web_ui_codemirror
module Form = Bonsai_web_ui_form.With_automatic_view

let get_vdom form =
  match Form.View.to_vdom_plain (Form.view form) with
  | [ v ] -> v
  | other -> Vdom.Node.div other
;;

let form_result_spec
  (type a)
  ?filter_printed_attributes
  ?censor_paths
  ?(get_vdom = get_vdom)
  sexp_of_a
  : (a Form.t, a) Result_spec.t
  =
  (module struct
    type t = a Form.t
    type incoming = a

    let view form =
      let module V = (val Result_spec.vdom ?filter_printed_attributes ?censor_paths Fn.id)
      in
      let vdom = get_vdom form in
      let vdom = V.view vdom in
      let value =
        Form.value form
        |> [%sexp_of: a Or_error.t]
        |> Expect_test_helpers_base.sexp_to_string
      in
      sprintf "%s\n==============\n%s\n" value vdom
    ;;

    let incoming = Form.set
  end)
;;

module T = struct
  type t =
    | Foo of string
    | Bar of int
  [@@deriving sexp, sexp_grammar]

  let of_string s =
    match String.split s ~on:' ' with
    | "foo" :: rest -> Foo (String.concat ~sep:" " rest)
    | [ "bar"; i ] -> Bar (Int.of_string i)
    | _ -> raise_s [%message "of_string: invalid string" (s : string)]
  ;;

  let to_string = function
    | Foo s -> [%string "foo %{s}"]
    | Bar i -> [%string "bar %{i#Int}"]
  ;;
end

module Run_tests (M : sig
    val string : unit -> Bonsai.graph -> string Form.t Bonsai.t
    val stringable : unit -> Bonsai.graph -> T.t Form.t Bonsai.t
    val sexpable : unit -> Bonsai.graph -> T.t Form.t Bonsai.t
  end) =
struct
  let name = "Bonsai_web_ui_codemirror_form"

  let%expect_test "interacting with string form" =
    let handle = Handle.create (form_result_spec [%sexp_of: string]) (M.string ()) in
    Handle.show handle;
    [%expect
      {|
      (Ok "")

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>>  </codemirror>
      |}];
    Codemirror_test.send_transaction
      handle
      ~get_vdom
      ~name
      (Codemirror_ui.Transaction.set_lines [ "first line"; "second line" ]);
    Handle.show handle;
    [%expect
      {|
      (Ok "first line\nsecond line")

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>> first line
      second line </codemirror>
      |}]
  ;;

  let%expect_test "setting into string form" =
    let handle = Handle.create (form_result_spec [%sexp_of: string]) (M.string ()) in
    Handle.show handle;
    [%expect
      {|
      (Ok "")

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>>  </codemirror>
      |}];
    Handle.do_actions handle [ "first line\nsecond line which is very long" ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      (Ok "first line\nsecond line which is very long")

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>> first line
      second line which is very long </codemirror>
      |}]
  ;;

  let%expect_test "interacting with stringable form" =
    let handle = Handle.create (form_result_spec [%sexp_of: T.t]) (M.stringable ()) in
    Handle.show handle;
    [%expect
      {|
      (Error ("of_string: invalid string" (s "")))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>>  </codemirror>
      |}];
    Codemirror_test.send_transaction
      handle
      ~get_vdom
      ~name
      (Codemirror_ui.Transaction.set_lines [ "foo this long sentence" ]);
    Handle.show handle;
    [%expect
      {|
      (Ok (Foo "this long sentence"))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>> foo this long sentence </codemirror>
      |}]
  ;;

  let%expect_test "setting into a stringable form" =
    let handle = Handle.create (form_result_spec [%sexp_of: T.t]) (M.sexpable ()) in
    Handle.show handle;
    [%expect
      {|
      (Error (
        Failure
        "Sexplib.Sexp.of_string: incomplete S-expression while in state Parsing_toplevel_whitespace: "))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>>  </codemirror>
      |}];
    Handle.do_actions handle [ Foo "foo goes here" ];
    Handle.show handle;
    [%expect
      {|
      (Ok (Foo "foo goes here"))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>> (Foo "foo goes here") </codemirror>
      |}]
  ;;

  let%expect_test "interacting with sexpable form" =
    let handle = Handle.create (form_result_spec [%sexp_of: T.t]) (M.sexpable ()) in
    Handle.show handle;
    [%expect
      {|
      (Error (
        Failure
        "Sexplib.Sexp.of_string: incomplete S-expression while in state Parsing_toplevel_whitespace: "))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>>  </codemirror>
      |}];
    Codemirror_test.send_transaction
      handle
      ~get_vdom
      ~name
      (Codemirror_ui.Transaction.set_lines [ "(Foo"; "\"my foo\""; ")" ]);
    Handle.show handle;
    [%expect
      {|
      (Ok (Foo "my foo"))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>> (Foo
      "my foo"
      ) </codemirror>
      |}]
  ;;

  let%expect_test "setting into a sexpable form" =
    let handle = Handle.create (form_result_spec [%sexp_of: T.t]) (M.sexpable ()) in
    Handle.show handle;
    [%expect
      {|
      (Error (
        Failure
        "Sexplib.Sexp.of_string: incomplete S-expression while in state Parsing_toplevel_whitespace: "))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>>  </codemirror>
      |}];
    Handle.do_actions handle [ Bar 123 ];
    Handle.show handle;
    [%expect
      {|
      (Ok (Bar 123))

      ==============
      <codemirror data-codemirror-editor="Bonsai_web_ui_codemirror_form" codemirror-test-hook=<fun>> (Bar 123) </codemirror>
      |}]
  ;;
end

(* basic codemirror *)
module _ = Run_tests (struct
    let string () = Codemirror_form.Basic.string ()
    let stringable () = Codemirror_form.Basic.stringable (module T)
    let sexpable () = Codemirror_form.Basic.sexpable (module T)
  end)

module _ = Run_tests (struct
    let string () =
      Codemirror_form.Dynamic_extensions.string
        (module Unit)
        ~equal:[%equal: Unit.t]
        ~compute_extensions:(Bonsai.return (fun () -> []))
        (Bonsai.return ())
    ;;

    let stringable () =
      Codemirror_form.Dynamic_extensions.stringable
        (module Unit)
        ~equal:[%equal: Unit.t]
        (module T)
        ~compute_extensions:(Bonsai.return (fun () -> []))
        (Bonsai.return ())
    ;;

    let sexpable () =
      Codemirror_form.Dynamic_extensions.sexpable
        (module Unit)
        ~equal:[%equal: Unit.t]
        (module T)
        ~compute_extensions:(Bonsai.return (fun () -> []))
        (Bonsai.return ())
    ;;
  end)

module _ = Run_tests (struct
    let string () =
      Codemirror_form.Sexp_grammar_autocomplete.string (Bonsai.return T.t_sexp_grammar)
    ;;

    let stringable () =
      Codemirror_form.Sexp_grammar_autocomplete.stringable
        (module T)
        (Bonsai.return T.t_sexp_grammar)
    ;;

    let sexpable () =
      Codemirror_form.Sexp_grammar_autocomplete.sexpable
        (module T)
        (Bonsai.return T.t_sexp_grammar)
    ;;
  end)
