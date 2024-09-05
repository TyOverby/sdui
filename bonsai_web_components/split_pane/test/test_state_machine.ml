open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
module Split_pane = Bonsai_web_ui_split_pane

include struct
  open Split_pane
  module Parameters = For_testing.Parameters
  module Panel_and_size = Panel_and_size
  module Constraint = Constraint
end

module Test : sig
  (* This module is a fairly thin wrapper around the Bonsai test handle.

     It probably makes sense to start with looking at how this is used first. *)
  type t

  (* Tests are run on both horizontal and vertical directions as (when the container is
     square) the logic should be symmetric; running all tests on both directions helps
     ensure that invariant *)
  val run
    :  ?initial_container_size:float
    -> ?parameters:Parameters.t
    -> (t -> unit)
    -> unit

  val show_sizes : t -> unit

  (* To make the symmetric tests work, this sets the container to a square so the
     dimensions in the Horizontal and Vertical case are the same *)
  val set_container_size : t -> float -> unit
  val simulate_drag : t -> delta:int -> unit
  val update_parameters : t -> (Parameters.t -> Parameters.t) -> unit
  val action : t -> Split_pane.For_testing.Action.t -> unit
end = struct
  module Result_spec = struct
    type t =
      { state : Split_pane.For_testing.State.t
      ; parameters : Parameters.t
      ; inject_action : Split_pane.For_testing.Action.t -> unit Effect.t
      }

    type incoming = Split_pane.For_testing.Action.t

    let view t =
      let separator_px = t.parameters.separator_size_px |> Float.of_int in
      let direction = t.parameters.direction in
      match
        Split_pane.For_testing.State.(
          Option.both (first_panel_px t.state) (container_dimensions t.state))
      with
      | None -> "No size yet"
      | Some (first_panel_px, container_dimensions) ->
        let container_px =
          Split_pane.For_testing.Container_dimensions.for_direction
            container_dimensions
            ~direction
        in
        let second_panel_px = container_px -. separator_px -. first_panel_px in
        let first_percent =
          Percent.of_mult ((first_panel_px +. (separator_px /. 2.)) /. container_px)
        in
        let second_percent =
          Percent.of_mult ((second_panel_px +. (separator_px /. 2.)) /. container_px)
        in
        let px x = [%string "%{x#Float}px"] in
        Ascii_table_kernel.to_string_noattr
          ~bars:`Unicode
          [ Ascii_table_kernel.Column.create ~align:Center "1" Tuple3.get1
          ; Ascii_table_kernel.Column.create ~align:Center "sep" Tuple3.get2
          ; Ascii_table_kernel.Column.create ~align:Center "2" Tuple3.get3
          ]
          [ px first_panel_px, px separator_px, px second_panel_px
          ; Percent.to_string first_percent, "-", Percent.to_string second_percent
          ]
    ;;

    let incoming t = t.inject_action

    let test_component ~parameters graph =
      let state, inject_action = Split_pane.For_testing.state_machine ~parameters graph in
      let%arr parameters = parameters
      and state = state
      and inject_action = inject_action in
      { state; parameters; inject_action }
    ;;
  end

  type t =
    { parameters : Parameters.t Bonsai.Expert.Var.t
    ; handle : (Result_spec.t, Result_spec.incoming) Handle.t
    }

  let create parameters =
    let parameters = Bonsai.Expert.Var.create parameters in
    let handle =
      Handle.create
        (module Result_spec)
        (Result_spec.test_component ~parameters:(Bonsai.Expert.Var.value parameters))
    in
    { parameters; handle }
  ;;

  let show_sizes t = Handle.show t.handle

  let set_container_size t size =
    Handle.do_actions
      t.handle
      [ Container_resized
          (Split_pane.For_testing.Container_dimensions.Fields.create
             ~width:size
             ~height:size)
      ]
  ;;

  let simulate_drag t ~delta =
    Handle.recompute_view t.handle;
    let state = (Handle.last_result t.handle).state in
    let first_panel_size =
      Split_pane.For_testing.State.first_panel_px state
      |> Option.value_exn
           ~message:"Cannot drag until the component has initialised itself"
    in
    (* setting the mouse pos to 0  *)
    Handle.do_actions
      t.handle
      [ Drag_start
          { container_start = 0.; separator_start = first_panel_size; mouse_pos = 0 }
      ; Drag_move { mouse_pos = delta }
      ]
  ;;

  let update_parameters t f = Bonsai.Expert.Var.update t.parameters ~f
  let action t action = Handle.do_actions t.handle [ action ]

  let run ?initial_container_size ?(parameters = Parameters.default) f =
    let do_case t =
      (match initial_container_size with
       | None -> ()
       | Some size -> set_container_size t size);
      f t
    in
    do_case (create { parameters with direction = Horizontal });
    do_case (create { parameters with direction = Vertical })
  ;;
end

let%expect_test "at startup, sizes are not calculated until the container size is known" =
  Test.run (fun t ->
    Test.show_sizes t;
    [%expect {| No size yet |}];
    Test.set_container_size t 500.;
    Test.show_sizes t;
    [%expect
      {|
      ┌────────┬───────┬────────┐
      │   1    │  sep  │   2    │
      ├────────┼───────┼────────┤
      │ 245.px │ 10.px │ 245.px │
      │  50%   │   -   │  50%   │
      └────────┴───────┴────────┘
      |}])
;;

let%expect_test "Dragging works" =
  Test.run ~initial_container_size:510. (fun t ->
    Test.show_sizes t;
    [%expect
      {|
      ┌────────┬───────┬────────┐
      │   1    │  sep  │   2    │
      ├────────┼───────┼────────┤
      │ 250.px │ 10.px │ 250.px │
      │  50%   │   -   │  50%   │
      └────────┴───────┴────────┘
      |}];
    Test.action
      t
      (Drag_start { container_start = 0.; separator_start = 250.; mouse_pos = 247 });
    Test.show_sizes t;
    [%expect
      {|
      ┌────────┬───────┬────────┐
      │   1    │  sep  │   2    │
      ├────────┼───────┼────────┤
      │ 250.px │ 10.px │ 250.px │
      │  50%   │   -   │  50%   │
      └────────┴───────┴────────┘
      |}];
    (* Nothing changes yet, but as we move the sizes update *)
    Test.action t (Drag_move { mouse_pos = 237 });
    Test.show_sizes t;
    [%expect
      {|
      ┌──────────┬───────┬──────────┐
      │    1     │  sep  │    2     │
      ├──────────┼───────┼──────────┤
      │  240.px  │ 10.px │  260.px  │
      │ 48.0392% │   -   │ 51.9608% │
      └──────────┴───────┴──────────┘
      |}];
    Test.action t (Drag_move { mouse_pos = 257 });
    Test.show_sizes t;
    [%expect
      {|
      ┌──────────┬───────┬──────────┐
      │    1     │  sep  │    2     │
      ├──────────┼───────┼──────────┤
      │  260.px  │ 10.px │  240.px  │
      │ 51.9608% │   -   │ 48.0392% │
      └──────────┴───────┴──────────┘
      |}];
    (* When we finish the drag, it updates one last time, but after that any other drag
       move actions are ignored (the mouse move event handler isn't removed until the next
       frame, so can still fire after mouse up) *)
    let final_sizes () =
      Test.show_sizes t;
      [%expect
        {|
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  268.px  │ 10.px │  232.px  │
        │ 53.5294% │   -   │ 46.4706% │
        └──────────┴───────┴──────────┘
        |}]
    in
    Test.action t (Drag_end { mouse_pos = 265 });
    final_sizes ();
    Test.action t (* no-op *) (Drag_move { mouse_pos = 260 });
    final_sizes ();
    ())
;;

(* This whole thing can be tested with [Test.simulate_drag] *)

let%expect_test "Dragging is subject to constraints" =
  Test.run ~initial_container_size:510. (fun t ->
    (* Even if no constraints are explicitly set, the container bounds are treated as
       constraints: *)
    Test.show_sizes t;
    [%expect
      {|
      ┌────────┬───────┬────────┐
      │   1    │  sep  │   2    │
      ├────────┼───────┼────────┤
      │ 250.px │ 10.px │ 250.px │
      │  50%   │   -   │  50%   │
      └────────┴───────┴────────┘
      |}];
    Test.simulate_drag t ~delta:10000;
    Test.show_sizes t;
    [%expect
      {|
      ┌──────────┬───────┬───────────┐
      │    1     │  sep  │     2     │
      ├──────────┼───────┼───────────┤
      │  500.px  │ 10.px │   0.px    │
      │ 99.0196% │   -   │ 98.0392bp │
      └──────────┴───────┴───────────┘
      |}];
    Test.simulate_drag t ~delta:(-20000);
    Test.show_sizes t;
    [%expect
      {|
      ┌───────────┬───────┬──────────┐
      │     1     │  sep  │    2     │
      ├───────────┼───────┼──────────┤
      │   0.px    │ 10.px │  500.px  │
      │ 98.0392bp │   -   │ 99.0196% │
      └───────────┴───────┴──────────┘
      |}];
    (* But this also works with custom constraints *)
    Test.update_parameters t (fun params ->
      { params with constraints = [ Constraint.min_px ~panel:Second 100. ] });
    Test.simulate_drag t ~delta:10000;
    Test.show_sizes t;
    [%expect
      {|
      ┌──────────┬───────┬──────────┐
      │    1     │  sep  │    2     │
      ├──────────┼───────┼──────────┤
      │  400.px  │ 10.px │  100.px  │
      │ 79.4118% │   -   │ 20.5882% │
      └──────────┴───────┴──────────┘
      |}]);
  ()
;;

let%expect_test "The initial size value should be used to set the initial size sensibly" =
  let case ?(constraints = []) initial_size ~expect =
    Test.run
      ~parameters:{ Parameters.default with constraints; initial_size }
      ~initial_container_size:510.
      (fun t ->
         Test.show_sizes t;
         expect ())
  in
  (* Absolute *)
  case (Panel_and_size.px First 50.) ~expect:(fun _ ->
    [%expect
      {|
      ┌──────────┬───────┬──────────┐
      │    1     │  sep  │    2     │
      ├──────────┼───────┼──────────┤
      │  50.px   │ 10.px │  450.px  │
      │ 10.7843% │   -   │ 89.2157% │
      └──────────┴───────┴──────────┘
      |}]);
  case (Panel_and_size.px Second 50.) ~expect:(fun () ->
    [%expect
      {|
      ┌──────────┬───────┬──────────┐
      │    1     │  sep  │    2     │
      ├──────────┼───────┼──────────┤
      │  450.px  │ 10.px │  50.px   │
      │ 89.2157% │   -   │ 10.7843% │
      └──────────┴───────┴──────────┘
      |}]);
  (* Percentage *)
  case
    (Panel_and_size.percent First (Percent.of_percentage 25.))
    ~expect:(fun () ->
      [%expect
        {|
        ┌─────────┬───────┬─────────┐
        │    1    │  sep  │    2    │
        ├─────────┼───────┼─────────┤
        │ 122.5px │ 10.px │ 377.5px │
        │   25%   │   -   │   75%   │
        └─────────┴───────┴─────────┘
        |}]);
  case
    (Panel_and_size.percent Second (Percent.of_percentage 25.))
    ~expect:(fun () ->
      [%expect
        {|
        ┌─────────┬───────┬─────────┐
        │    1    │  sep  │    2    │
        ├─────────┼───────┼─────────┤
        │ 377.5px │ 10.px │ 122.5px │
        │   75%   │   -   │   25%   │
        └─────────┴───────┴─────────┘
        |}]);
  (* Constraints can override the initial size: *)
  case
    ~constraints:[ Constraint.min_px ~panel:Second 200. ]
    (Panel_and_size.percent Second (Percent.of_percentage 25.))
    ~expect:(fun () ->
      [%expect
        {|
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  300.px  │ 10.px │  200.px  │
        │ 59.8039% │   -   │ 40.1961% │
        └──────────┴───────┴──────────┘
        |}]);
  ()
;;

let%expect_test "Resizing the window " =
  let initial_size = Panel_and_size.percent First (Percent.of_percentage 25.) in
  (* For all of these tests the initial position is this: *)
  let initial_state () =
    [%expect
      {|
      ┌────────┬───────┬─────────┐
      │   1    │  sep  │    2    │
      ├────────┼───────┼─────────┤
      │ 97.5px │ 10.px │ 302.5px │
      │  25%   │   -   │   75%   │
      └────────┴───────┴─────────┘
      |}]
  in
  let case ~on_container_resize ~expect =
    Test.run
      ~parameters:{ Parameters.default with initial_size; on_container_resize }
      ~initial_container_size:410.
      (fun t ->
         Test.show_sizes t;
         initial_state ();
         Test.set_container_size t 1020.;
         print_endline "Resizing 410 -> 810";
         Test.show_sizes t;
         Test.set_container_size t 205.;
         print_endline "Resizing 810 -> 210";
         Test.show_sizes t;
         expect ())
  in
  (* The default tries to keep the proportion *)
  case ~on_container_resize:Keep_proportion ~expect:(fun () ->
    [%expect
      {|
      Resizing 410 -> 810
      ┌────────┬───────┬────────┐
      │   1    │  sep  │   2    │
      ├────────┼───────┼────────┤
      │ 250.px │ 10.px │ 760.px │
      │  25%   │   -   │  75%   │
      └────────┴───────┴────────┘

      Resizing 810 -> 210
      ┌─────────┬───────┬──────────┐
      │    1    │  sep  │    2     │
      ├─────────┼───────┼──────────┤
      │ 46.25px │ 10.px │ 148.75px │
      │   25%   │   -   │   75%    │
      └─────────┴───────┴──────────┘
      |}]);
  (* But we can ask to try to keep the first panel... *)
  case ~on_container_resize:(Try_to_keep_panel_size First) ~expect:(fun () ->
    [%expect
      {|
      Resizing 410 -> 810
      ┌─────────┬───────┬─────────┐
      │    1    │  sep  │    2    │
      ├─────────┼───────┼─────────┤
      │ 97.5px  │ 10.px │ 912.5px │
      │ 10.049% │   -   │ 89.951% │
      └─────────┴───────┴─────────┘

      Resizing 810 -> 210
      ┌────────┬───────┬────────┐
      │   1    │  sep  │   2    │
      ├────────┼───────┼────────┤
      │ 97.5px │ 10.px │ 97.5px │
      │  50%   │   -   │  50%   │
      └────────┴───────┴────────┘
      |}]);
  (* ... or second panel instead (which isn't possible when going smaller, but it adjusts
     to fit) *)
  case ~on_container_resize:(Try_to_keep_panel_size Second) ~expect:(fun () ->
    [%expect
      {|
      Resizing 410 -> 810
      ┌──────────┬───────┬──────────┐
      │    1     │  sep  │    2     │
      ├──────────┼───────┼──────────┤
      │ 707.5px  │ 10.px │ 302.5px  │
      │ 69.8529% │   -   │ 30.1471% │
      └──────────┴───────┴──────────┘

      Resizing 810 -> 210
      ┌──────────┬───────┬─────────┐
      │    1     │  sep  │    2    │
      ├──────────┼───────┼─────────┤
      │   0.px   │ 10.px │ 195.px  │
      │ 2.43902% │   -   │ 97.561% │
      └──────────┴───────┴─────────┘
      |}]);
  ()
;;

let%expect_test "Resizing is subject to constraints" =
  Test.run
    ~parameters:
      { Parameters.default with
        constraints = [ Constraint.max_px ~panel:First 50. ]
      ; on_container_resize = Keep_proportion
      }
    ~initial_container_size:60.
    (fun t ->
      Test.show_sizes t;
      [%expect
        {|
        ┌───────┬───────┬───────┐
        │   1   │  sep  │   2   │
        ├───────┼───────┼───────┤
        │ 25.px │ 10.px │ 25.px │
        │  50%  │   -   │  50%  │
        └───────┴───────┴───────┘
        |}];
      (* Works if we resize a bit more *)
      Test.set_container_size t 110.;
      Test.show_sizes t;
      [%expect
        {|
        ┌───────┬───────┬───────┐
        │   1   │  sep  │   2   │
        ├───────┼───────┼───────┤
        │ 50.px │ 10.px │ 50.px │
        │  50%  │   -   │  50%  │
        └───────┴───────┴───────┘
        |}];
      (* But then the constraint kicks in if we go more than that and it's no longer
          possible to keep the split 50-50 *)
      Test.set_container_size t 200.;
      Test.show_sizes t;
      [%expect
        {|
        ┌───────┬───────┬────────┐
        │   1   │  sep  │   2    │
        ├───────┼───────┼────────┤
        │ 50.px │ 10.px │ 140.px │
        │ 27.5% │   -   │ 72.5%  │
        └───────┴───────┴────────┘
        |}])
;;

let show_constraints constraints ~expect =
  Test.run
    ~parameters:{ Parameters.default with constraints }
    ~initial_container_size:510.
    (fun t ->
       Test.simulate_drag t ~delta:(-1000);
       print_endline "Min:";
       Test.show_sizes t;
       print_endline "Max:";
       Test.simulate_drag t ~delta:1000;
       Test.show_sizes t;
       expect ())
;;

let%expect_test "The most restrictive constraint wins" =
  show_constraints
    [ Constraint.min_px ~panel:First 50.
    ; Constraint.min_px ~panel:First 70.
    ; Constraint.max_px ~panel:First 300.
    ; Constraint.max_px ~panel:First 350.
    ]
    (* Here, the winner is (70, 300) *)
    ~expect:(fun () ->
      [%expect
        {|
        Min:
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  70.px   │ 10.px │  430.px  │
        │ 14.7059% │   -   │ 85.2941% │
        └──────────┴───────┴──────────┘

        Max:
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  300.px  │ 10.px │  200.px  │
        │ 59.8039% │   -   │ 40.1961% │
        └──────────┴───────┴──────────┘
        |}]);
  (* This also applies to constraints from the first/second panel

     Here the first maximums of each panel end up being more restrictive
  *)
  show_constraints
    [ Constraint.min_px ~panel:First 50.
    ; Constraint.max_px ~panel:First 450.
    ; Constraint.min_px ~panel:Second 0.
    ; Constraint.max_px ~panel:Second 400.
    ]
    ~expect:(fun () ->
      [%expect
        {|
        Min:
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  100.px  │ 10.px │  400.px  │
        │ 20.5882% │   -   │ 79.4118% │
        └──────────┴───────┴──────────┘

        Max:
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  450.px  │ 10.px │  50.px   │
        │ 89.2157% │   -   │ 10.7843% │
        └──────────┴───────┴──────────┘
        |}]);
  (* This also applies to mixing percentage/absolute constraints.

     Percentage wins in the 'min first panel' direction but absolute in the 'max first
     panel/min second panel' one *)
  show_constraints
    [ Constraint.min_px ~panel:First 10.
    ; Constraint.min_percent ~panel:First (Percent.of_percentage 20.)
    ; Constraint.min_px ~panel:Second 200.
    ; Constraint.min_percent ~panel:Second (Percent.of_percentage 5.)
    ]
    ~expect:(fun () ->
      [%expect
        {|
        Min:
        ┌───────┬───────┬────────┐
        │   1   │  sep  │   2    │
        ├───────┼───────┼────────┤
        │ 97.px │ 10.px │ 403.px │
        │  20%  │   -   │  80%   │
        └───────┴───────┴────────┘

        Max:
        ┌──────────┬───────┬──────────┐
        │    1     │  sep  │    2     │
        ├──────────┼───────┼──────────┤
        │  300.px  │ 10.px │  200.px  │
        │ 59.8039% │   -   │ 40.1961% │
        └──────────┴───────┴──────────┘
        |}]);
  ()
;;
