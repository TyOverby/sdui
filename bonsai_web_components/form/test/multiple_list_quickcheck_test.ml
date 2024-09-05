open! Core
open! Bonsai_web
open! Bonsai_web_test.Experimental
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module Test_scenario = struct
  type t = string list list list [@@deriving quickcheck]
end

let%expect_test "many setters over many frames" =
  let component =
    Form.Elements.Multiple.list
      (Form.Elements.Textbox.string ~allow_updates_when_focused:`Never ())
  in
  let handle =
    Handle.create
      (Bonsai_web_ui_form_manual_test.list_form_result_spec [%sexp_of: string])
      component
  in
  let last_thing_that_got_set = ref [] in
  Quickcheck.test Test_scenario.quickcheck_generator ~f:(fun scenario ->
    List.iter scenario ~f:(fun frame ->
      (match List.last frame with
       | None -> ()
       | Some last -> last_thing_that_got_set := last);
      let to_set = List.map frame ~f:(fun l -> `Set l) in
      Handle.do_actions handle to_set;
      Handle.recompute_view handle;
      let result = Handle.result handle in
      Expect_test_helpers_base.require_equal
        (module struct
          type t = string list [@@deriving equal, sexp_of]
        end)
        (Form.value result |> Or_error.ok_exn)
        !last_thing_that_got_set))
;;
