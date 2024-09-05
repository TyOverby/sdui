open! Core
open Parsexp_symbolic_automaton

type t =
  | Base of
      { state : Automaton.State.t
      ; sexp_comment_nesting : int list
      }
  | Block_comment of
      { state : Automaton.State.t
      ; rest : t
      }
[@@deriving sexp_of]

let initial_state = Base { state = Automaton.State.initial; sexp_comment_nesting = [] }
let error_state = Base { state = Error; sexp_comment_nesting = [] }
let current (Base { state; _ } | Block_comment { state; _ }) = state

let set_current t state =
  match t with
  | Base base_state -> Base { base_state with state }
  | Block_comment comment_state -> Block_comment { comment_state with state }
;;

let handle_end_of_sexp_additional_actions t ~nesting:current_nesting =
  match t with
  | Base { sexp_comment_nesting = []; _ } | Block_comment _ -> t
  | Base { sexp_comment_nesting = nesting :: rest; state } ->
    if current_nesting = nesting then Base { state; sexp_comment_nesting = rest } else t
;;

let start_sexp_comment t ~nesting =
  match t with
  | Base { sexp_comment_nesting; state } ->
    Base { sexp_comment_nesting = nesting :: sexp_comment_nesting; state }
  | Block_comment _ -> error_state
;;

let handle_ending_block_comment = function
  | Block_comment { rest; _ } -> rest
  | Base _ -> error_state
;;

let in_sexp_comment t =
  match t with
  | Base { sexp_comment_nesting = _ :: _; _ } -> true
  | Block_comment _ | Base { sexp_comment_nesting = []; _ } -> false
;;

let handle_add_checkpoint checkpoints_or_error ~default =
  match checkpoints_or_error with
  | Ok c -> c
  | Error e ->
    print_s [%message "We should never be adding duplicate checkpoints." (e : Error.t)];
    default
;;

module Checkpoint_data = struct
  type nonrec 'decoration t = t * int * 'decoration list

  let get_decorations_since_last_checkpoint = Tuple3.get3
end

module Highlight_accumulator = struct
  (* Ideally, the type of [new_decorations] would just be a View.Decoration.t list, but
     this module can't depend on any CodeMirror modules. *)
  type nonrec 'decoration t =
    { lines_since_last_checkpoint : int
    ; nesting_level : int
    ; parser_state : t
    ; checkpoints : 'decoration Checkpoint_data.t Checkpoints.t
    ; new_decorations : 'decoration list
    }
end

module Highlight_result = struct
  type 'decoration t = 'decoration Checkpoint_data.t Checkpoints.t

  let get_all_checkpoints = Fn.id
end

module type Text = sig
  type t

  val foldi : t -> init:'acc -> f:(int -> 'acc -> char -> 'acc) -> 'acc
  val length : t -> int
  val suffix : t -> int -> t
end

let highlight_with_checkpoints_every_n_lines
  (type text)
  (module Text : Text with type t = text)
  text
  ~n
  ~checkpoints
  ~combine
  =
  let open Parsexp_symbolic_automaton in
  let open Highlight_accumulator in
  let start_index, (start_state, start_nesting, _) =
    Checkpoints.get_last_checkpoint checkpoints ()
    |> Option.value ~default:(0, (initial_state, 0, []))
  in
  let start_index = Int.min start_index (Text.length text) in
  let res =
    Text.foldi
      (Text.suffix text (Text.length text - start_index))
      ~init:
        { lines_since_last_checkpoint = 0
        ; nesting_level = start_nesting
        ; parser_state = start_state
        ; checkpoints
        ; new_decorations = []
        }
      ~f:
        (fun
          offset
          ({ lines_since_last_checkpoint
           ; nesting_level
           ; parser_state
           ; checkpoints
           ; new_decorations
           } as acc)
          char
        ->
        let lines_since_last_checkpoint =
          match char with
          | '\n' -> lines_since_last_checkpoint + 1
          | _ -> lines_since_last_checkpoint
        in
        let cur_pos = start_index + offset in
        let lines_since_last_checkpoint, checkpoints, new_decorations =
          if lines_since_last_checkpoint >= n
          then
            ( 0
            , handle_add_checkpoint
                (Checkpoints.add_checkpoint
                   checkpoints
                   ~pos:cur_pos
                   ~data:(parser_state, nesting_level, new_decorations)
                   ())
                ~default:checkpoints
            , [] )
          else lines_since_last_checkpoint, checkpoints, new_decorations
        in
        let new_state parser_state =
          { acc with
            lines_since_last_checkpoint
          ; parser_state
          ; checkpoints
          ; new_decorations
          }
        in
        let rec handle_state parser_state =
          let decorate_if_not_in_sexp_comment ~nesting =
            if in_sexp_comment parser_state
            then new_decorations
            else combine ~index:cur_pos ~nesting ~decorations:new_decorations
          in
          match Automaton.transition (current parser_state, char) with
          | T (Opening, current) ->
            { lines_since_last_checkpoint
            ; nesting_level = nesting_level + 1
            ; parser_state = set_current parser_state current
            ; checkpoints
            ; new_decorations = decorate_if_not_in_sexp_comment ~nesting:nesting_level
            }
          | T (Closing, current) ->
            let parser_state = set_current parser_state current in
            if nesting_level = 0
            then new_state parser_state
            else (
              let nesting_level = nesting_level - 1 in
              { lines_since_last_checkpoint
              ; nesting_level
              ; parser_state =
                  handle_end_of_sexp_additional_actions
                    parser_state
                    ~nesting:nesting_level
              ; checkpoints
              ; new_decorations = decorate_if_not_in_sexp_comment ~nesting:nesting_level
              })
          | T (Start_block_comment, state) ->
            new_state (Block_comment { state; rest = parser_state })
          | T (Start_sexp_comment, current) ->
            let parser_state = set_current parser_state current in
            new_state (start_sexp_comment parser_state ~nesting:nesting_level)
          | T (Push_quoted_atom, current) ->
            let parser_state = set_current parser_state current in
            new_state
              (handle_end_of_sexp_additional_actions parser_state ~nesting:nesting_level)
          | T
              ( ( Nop
                | Add_atom_char
                | Add_quoted_atom_char
                | Add_first_char
                | Add_escaped
                | Add_hex_escape_char
                | Add_dec_escape_char
                | Add_last_hex_escape_char
                | Add_last_dec_escape_char
                | Add_token_char
                | Comment_add_last_dec_escape_char
                | Start_quoted_string
                | Start_line_comment )
              , current ) -> new_state (set_current parser_state current)
          | E ((Nop | Add_first_char_hash | Add_escaped_cr | End_line_comment), current)
            ->
            (* Nothing to do for epsilon actions. However, they don't consume a character.
               Might have to do something special if we start caring about more than parentheses,
               as we might have to keep track of the state from before Add_first_char_hash.
            *)
            handle_state (set_current parser_state current)
          | E (Push_atom, current) ->
            let parser_state = set_current parser_state current in
            handle_state
              (handle_end_of_sexp_additional_actions parser_state ~nesting:nesting_level)
          | Error _ -> new_state parser_state
          | End_block_comment -> new_state (handle_ending_block_comment parser_state)
        in
        handle_state parser_state)
  in
  handle_add_checkpoint
    (Checkpoints.add_checkpoint
       res.checkpoints
       ~pos:(Text.length text)
       ~data:(res.parser_state, res.nesting_level, res.new_decorations)
       ~at_end_of_parse:true
       ())
    ~default:res.checkpoints
;;

let highlight text = highlight_with_checkpoints_every_n_lines text ~n:20

let%test_module "highlight" =
  (module struct
    type decoration =
      { index : int
      ; nesting : int
      }
    [@@deriving sexp_of]

    let test s =
      let res =
        highlight_with_checkpoints_every_n_lines
          (module String)
          s
          ~n:2
          ~checkpoints:Checkpoints.empty
          ~combine:(fun ~index ~nesting ~decorations -> { index; nesting } :: decorations)
      in
      print_endline
        (Checkpoints.to_string res (fun data ->
           let state = Tuple3.get1 data |> sexp_of_t |> Sexp.to_string in
           let nesting = Tuple3.get2 data in
           let decorations =
             List.rev (Tuple3.get3 data)
             |> List.map ~f:(fun decoration ->
               [%string "%{decoration.index#Int}: %{decoration.nesting#Int}"])
             |> String.concat ~sep:"\n"
           in
           [%string
             "\n\
              state - %{state}\n\
              nesting - %{nesting#Int}\n\
              decorations -\n\
              %{decorations}\n"]))
    ;;

    let%expect_test "normal cases" =
      test {|(foo)|};
      [%expect
        {|
        5:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        0: 0
        4: 0
        |}];
      test {|(((())))|};
      [%expect
        {|
        8:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        0: 0
        1: 1
        2: 2
        3: 3
        4: 3
        5: 2
        6: 1
        7: 0
        |}]
    ;;

    let%expect_test "unclosed" =
      test {|(((()|};
      [%expect
        {|
        5:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 3
        decorations -
        0: 0
        1: 1
        2: 2
        3: 3
        4: 3
        |}];
      test {|())))|};
      [%expect
        {|
        5:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        0: 0
        1: 0
        |}]
    ;;

    let%expect_test "only-closing results in no color" =
      (* NOTE: This test case only demonstrates existing behavior. *)
      test {|))))))|};
      [%expect
        {|
        6:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        |}]
    ;;

    let%expect_test "This only works on sexp's" =
      test {|{{[]}}|};
      [%expect
        {|
        6:
        state - (Base(state(Unquoted_string Normal))(sexp_comment_nesting()))
        nesting - 0
        decorations -
        |}]
    ;;

    let%expect_test "Handling of comments" =
      test
        {|(
        ; ((((((((((((((((((((((((((((((((((((((((((
        ())|};
      [%expect
        {|
        54:
        state - (Base(state Line_comment)(sexp_comment_nesting()))
        nesting - 1
        decorations -
        0: 0

        66:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        63: 1
        64: 1
        65: 0
        |}]
    ;;

    let%expect_test "Nestable comments are supported" =
      test
        {|(
        #; ()
        ())|};
      [%expect
        {|
        15:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 1
        decorations -
        0: 0

        27:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        24: 1
        25: 1
        26: 0
        |}];
      test
        {|(
        #;
        (

        )
        ())|};
      [%expect
        {|
        12:
        state - (Base(state Whitespace)(sexp_comment_nesting(1)))
        nesting - 1
        decorations -
        0: 0

        23:
        state - (Base(state Whitespace)(sexp_comment_nesting(1)))
        nesting - 2
        decorations -


        45:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        42: 1
        43: 1
        44: 0
        |}];
      test
        {|(
        #; ignore
        ())|};
      [%expect
        {|
        19:
        state - (Base(state(Unquoted_string Normal))(sexp_comment_nesting(1)))
        nesting - 1
        decorations -
        0: 0

        31:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        28: 1
        29: 1
        30: 0
        |}];
      test
        {|(
        #|
        (
        #| () |#
        )
        |#
        ())|};
      [%expect
        {|
        12:
        state - (Block_comment(state(Block_comment Normal))(rest(Base(state After_hash)(sexp_comment_nesting()))))
        nesting - 1
        decorations -
        0: 0

        39:
        state - (Block_comment(state(Block_comment After_hash))(rest(Base(state After_hash)(sexp_comment_nesting()))))
        nesting - 1
        decorations -


        60:
        state - (Base(state After_hash)(sexp_comment_nesting()))
        nesting - 1
        decorations -


        72:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        69: 1
        70: 1
        71: 0
        |}]
    ;;

    let%expect_test "Handling of string escaping" =
      test
        {|(
        "(((((((((((((((((((((((((((((((((((((((((("
        ())|};
      [%expect
        {|
        54:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 1
        decorations -
        0: 0

        66:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        63: 1
        64: 1
        65: 0
        |}]
    ;;

    let%expect_test "Checkpoints can each store multiple decorations" =
      test
        {|()()

          ()|};
      [%expect
        {|
        5:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        0: 0
        1: 0
        2: 0
        3: 0

        18:
        state - (Base(state Whitespace)(sexp_comment_nesting()))
        nesting - 0
        decorations -
        16: 0
        17: 0
        |}]
    ;;
  end)
;;
