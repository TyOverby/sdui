open! Core
module Codemirror = Bonsai_web_ui_codemirror

let one_of_strings names =
  { Sexp_grammar.untyped =
      Union
        [ Variant
            { case_sensitivity = Case_insensitive
            ; clauses =
                List.map names ~f:(fun name ->
                  Sexp_grammar.No_tag { Sexp_grammar.name; clause_kind = Atom_clause })
            }
        ]
  }
;;

module A = struct
  type t = string [@@deriving sexp]

  let t_sexp_grammar =
    one_of_strings [ "this is a test"; "this isn't a test"; "thishasnospaces" ]
  ;;
end

module B = struct
  type t = A.t list [@@deriving sexp_grammar]
end

let replace_substring ~from ~to_ ~with_ string =
  let beginning = String.sub string ~pos:0 ~len:from in
  let ending = String.sub string ~pos:to_ ~len:(String.length string - to_) in
  [%string "%{beginning}%{with_}%{ending}"]
;;

(* This test utility finds the '|' character in a string and treats that as the
   cursor position. It then queries sexp-grammar completion for completion
   results just as the codemirror component would. The results are processed by
   printing the original string with the span specified by the result replaced
   with each option. Empirical observations show that this is what codemirror
   does. *)
let test grammar text =
  let cursor_position =
    match String.findi text ~f:(fun _ c -> Char.equal '|' c) with
    | Some (index, _) -> index
    | None ->
      raise_s
        [%message
          "You need to have a pipe somewhere in the string to specify where the cursor is"]
  in
  let text = String.filter text ~f:(fun c -> not (Char.equal '|' c)) in
  let completion =
    Codemirror.Private.For_tests.completions ~text ~cursor_position ~grammar
  in
  let to_ = Option.value completion.to_ ~default:(String.length text) in
  List.iter completion.options ~f:(fun option ->
    let text =
      replace_substring ~from:completion.from ~to_ ~with_:[%string "%{option}|"] text
    in
    print_endline text);
  if not completion.exhaustive then print_endline "not exhaustive"
;;

let%expect_test "quotes are added when atoms have spaces" =
  test B.t_sexp_grammar {| (th| |};
  [%expect
    {|
     ("this is a test"|
     ("this isn't a test"|
     (thishasnospaces|
    not exhaustive
    |}]
;;

let%expect_test "beginning quote is taken into account" =
  test B.t_sexp_grammar {| ("th| |};
  [%expect
    {|
     ("this is a test"|
     ("this isn't a test"|
     (thishasnospaces|
    not exhaustive
    |}]
;;

let%expect_test "quote following cursor does not get duplicated" =
  test B.t_sexp_grammar {| ("th|" |};
  [%expect
    {|
     ("this is a test"|
     ("this isn't a test"|
     (thishasnospaces|
    not exhaustive
    |}]
;;

let%expect_test "anything other than a quote following cursor does get duplicated" =
  test B.t_sexp_grammar {| ("th|i" |};
  [%expect
    {|
     ("this is a test"|i"
     ("this isn't a test"|i"
     (thishasnospaces|i"
    not exhaustive
    |}]
;;

module C = struct
  type t =
    | Nothing
    | Something of A.t
  [@@deriving sexp, sexp_grammar]
end

let%expect_test "atoms inside a new list get suggested" =
  test C.t_sexp_grammar {| (Something | |};
  [%expect
    {|
    (Something "this is a test"|
    (Something "this isn't a test"|
    (Something thishasnospaces|
    |}];
  test C.t_sexp_grammar "(Something thishasnospaces |"
;;

let%expect_test "empty string gets completions too" =
  test C.t_sexp_grammar {| | |};
  [%expect
    {|
    Nothing|
    (Something|
    |}]
;;

let%expect_test "completions get returned even when the current substring is a valid sexp"
  =
  test C.t_sexp_grammar {| No| |};
  [%expect {| Nothing| |}]
;;

let%expect_test "no completions when we are beginning the next sexp" =
  test C.t_sexp_grammar {| Nothing | |};
  [%expect {| |}];
  test C.t_sexp_grammar {| Nothing No| |}
;;

module D = struct
  module M = struct
    type t = X [@@deriving sexp, equal, sexp_grammar]
  end

  type t = M.t Blang.t [@@deriving sexp, equal, sexp_grammar]
end

let%expect_test _ =
  test D.t_sexp_grammar "(and |";
  [%expect
    {|
    (and X|
    (and false|
    (and true|
    (and (and|
    (and (if|
    (and (not|
    (and (or|
    not exhaustive
    |}]
;;
