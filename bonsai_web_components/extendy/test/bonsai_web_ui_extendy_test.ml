open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Extendy = Bonsai_web_ui_extendy

type individual = string * (string -> unit Ui_effect.t)

let sexp_of_individual (value, _) = Sexp.Atom value

module Extendy_spec = struct
  type t = individual Extendy.t

  type incoming =
    | Append
    | Set_length of int
    | Remove of Extendy.Id.t
    | Write of Extendy.Id.t * string

  let view { Extendy.contents; _ } =
    contents |> [%sexp_of: individual Extendy.Id.Map.t] |> Sexp.to_string_hum
  ;;

  let incoming { Extendy.contents; append; set_length; remove } = function
    | Append -> append
    | Set_length i -> set_length i
    | Remove id -> remove id
    | Write (id, value) ->
      let _, set = contents |> Fn.flip Map.find_exn id in
      set value
  ;;
end

let handle () =
  let component graph =
    Extendy.component
      (fun graph ->
        let state, set_state =
          Bonsai.state
            ""
            ~sexp_of_model:[%sexp_of: String.t]
            ~equal:[%equal: String.t]
            graph
        in
        let%arr state = state
        and set_state = set_state in
        state, set_state)
      graph
  in
  Handle.create (module Extendy_spec) component
;;

let%expect_test "empty" =
  let handle = handle () in
  Handle.show handle;
  [%expect {| () |}]
;;

let%expect_test "append" =
  let handle = handle () in
  Handle.do_actions handle [ Append ];
  Handle.show handle;
  [%expect {| ((0 "")) |}]
;;

let%expect_test "set length increasing" =
  let handle = handle () in
  Handle.do_actions handle [ Set_length 4 ];
  Handle.show handle;
  [%expect {| ((0 "") (1 "") (2 "") (3 "")) |}]
;;

let%expect_test "write" =
  let handle = handle () in
  let id = Extendy.Id.t_of_sexp (Sexp.Atom "0") in
  Handle.do_actions handle [ Append ];
  Handle.recompute_view handle;
  Handle.do_actions handle [ Write (id, "hello") ];
  Handle.show handle;
  [%expect {| ((0 hello)) |}]
;;

let%expect_test "delete" =
  let handle = handle () in
  Handle.do_actions handle [ Append; Append ];
  Handle.recompute_view handle;
  let id = Extendy.Id.t_of_sexp (Sexp.Atom "0") in
  Handle.do_actions handle [ Write (id, "hello") ];
  Handle.show handle;
  Handle.do_actions handle [ Remove id ];
  Handle.show handle;
  [%expect
    {|
    ((0 hello) (1 ""))
    ((1 ""))
    |}]
;;

let%expect_test "set length decreasing" =
  let handle = handle () in
  Handle.do_actions handle [ Set_length 4 ];
  Handle.show handle;
  Handle.do_actions handle [ Set_length 2; Append ];
  Handle.show handle;
  [%expect
    {|
    ((0 "") (1 "") (2 "") (3 ""))
    ((0 "") (1 "") (4 ""))
    |}]
;;
