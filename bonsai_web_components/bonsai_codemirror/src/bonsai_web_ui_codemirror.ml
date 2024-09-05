open! Core

module type Model = Bonsai_web.Proc.Model

open! Bonsai_web
open Virtual_dom
open Js_of_ocaml
open Codemirror
open Bonsai.Let_syntax

module Editor_state = struct
  include State.Editor_state

  type t = (State.Editor_state.t[@sexp.opaque]) [@@deriving sexp]

  let equal = phys_equal
end

let state_text state =
  String.concat ~sep:"\n" (Text.Text.to_json (State.Editor_state.doc state))
;;

module Transaction = struct
  type t = State.Editor_state.t -> State.Transaction.t [@@deriving sexp]

  let equal = phys_equal

  let set_lines new_lines state =
    let open State in
    let open Text in
    let old_text = state_text state in
    Editor_state.update
      state
      [ Transaction_spec.create
          ~changes:
            (Change_spec.single
               ~from:0
               ~to_:(Js.string old_text)##.length
               ~insert:(Text.of_ new_lines)
               ())
          ()
      ]
  ;;
end

module Editor_changer = struct
  type t =
    { send_transaction : Transaction.t -> unit
    ; modify_editor_view : (View.Editor_view.t -> unit) -> unit
    }

  let sexp_of_t = sexp_of_opaque
  let t_of_sexp _ = assert false
  let equal = phys_equal
end

module Widget_instantiation_id = Unique_id.Int ()

module Action = struct
  type t =
    | Add of Widget_instantiation_id.t * Editor_changer.t
    | Remove of Widget_instantiation_id.t
    | Reset
    | Send_transaction of Transaction.t
    | Set_state of Editor_state.t
    | Modify_editor_view of ((View.Editor_view.t -> unit)[@sexp.opaque] [@equal.ignore])
  [@@deriving sexp, equal]
end

module Model = struct
  type t =
    { send_transaction : Editor_changer.t Widget_instantiation_id.Map.t
    ; state : Editor_state.t
    }
  [@@deriving sexp, equal]
end

module Codemirror_widget = struct
  type dom = Dom_html.element

  module Input = struct
    type t =
      { state : Editor_state.t
      ; inject : Action.t -> unit Effect.t
      ; path_and_generation : Path_and_generation.t
      }

    let sexp_of_t = sexp_of_opaque
  end

  module State = struct
    type t =
      { mutable id : Widget_instantiation_id.t
      ; mutable is_receiving_transactions : bool
      ; inject : (Action.t -> unit Effect.t) ref
      ; editor_view : (View.Editor_view.t[@sexp.opaque])
      }
    [@@deriving sexp]
  end

  let name = "codemirror"

  let destroy ~prev_input ~state ~element:_ =
    state.State.is_receiving_transactions <- false;
    Effect.Expert.handle_non_dom_event_exn (prev_input.Input.inject (Remove state.id));
    View.Editor_view.destroy state.editor_view
  ;;

  let add_send_transaction
    (input : Input.t)
    ({ editor_view; is_receiving_transactions; id; _ } : State.t)
    =
    let send_transaction transaction =
      let transaction = transaction (View.Editor_view.state editor_view) in
      if is_receiving_transactions
      then View.Editor_view.dispatch editor_view transaction
      else Effect.Expert.handle_non_dom_event_exn (input.inject (Remove id))
    in
    let modify_editor_view f = f editor_view in
    Effect.Expert.handle_non_dom_event_exn
      (input.inject (Add (id, { Editor_changer.send_transaction; modify_editor_view })))
  ;;

  let create (input : Input.t) =
    let inject = ref input.inject in
    let editor_view =
      View.Editor_view.create
        (View.Config.create
           ~state:input.state
           ~dispatch:
             (Js.wrap_callback (fun transaction editor_view ->
                View.Editor_view.update editor_view [ transaction ];
                Effect.Expert.handle_non_dom_event_exn
                  (!inject (Set_state (View.Editor_view.state editor_view)))))
           ())
    in
    let id = Widget_instantiation_id.create () in
    let state = { State.id; is_receiving_transactions = true; inject; editor_view } in
    add_send_transaction input state;
    state, (View.Editor_view.dom editor_view : Dom_html.element Js.t)
  ;;

  let update
    ~prev_input:{ Input.path_and_generation = old_path_and_generation; _ }
    ~input:
      { Input.state = new_state
      ; inject
      ; path_and_generation = new_path_and_generation
      ; _
      }
    ~state:({ State.editor_view; _ } as widget_state)
    ~element:_
    =
    widget_state.inject := inject;
    let current_state = View.Editor_view.state editor_view in
    let () =
      if (not (phys_equal new_state current_state))
         || Path_and_generation.distinct old_path_and_generation new_path_and_generation
      then View.Editor_view.set_state editor_view new_state
    in
    widget_state, View.Editor_view.dom editor_view
  ;;

  let to_vdom_for_testing = `Sexp_of_input
end

let codemirror_widget = Vdom.Node.widget_of_module (module Codemirror_widget) |> unstage

module For_testing = struct
  module Inject_hook = Vdom.Attr.No_op_hook (struct
      module Input = struct
        type t = (State.Editor_state.t -> State.Transaction.t) -> unit Effect.t
        [@@deriving sexp]

        let combine _ second = second
      end

      let name = "codemirror-test-hook"
    end)

  let type_id = Inject_hook.type_id
end

type t =
  { view : Vdom.Node.t
  ; state : State.Editor_state.t
  ; send_transaction : (State.Editor_state.t -> State.Transaction.t) -> unit Effect.t
  ; execute_command : View.Command.t -> unit Effect.t
  ; focus : unit Effect.t
  ; blur : unit Effect.t
  }

let view t = t.view
let state t = t.state
let send_transaction t = t.send_transaction
let focus t = t.focus
let blur t = t.blur
let execute_command t = t.execute_command
let text { state; _ } = state_text state
let set_lines t new_lines = t.send_transaction (Transaction.set_lines new_lines)

let of_initial_state ~name initial_state graph =
  let state, inject =
    Bonsai.actor0
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model:
        { send_transaction = Widget_instantiation_id.Map.empty; state = initial_state }
      ~recv:(fun _ctx model action ->
        let model =
          match action with
          | Add (id, editor_changer) ->
            { model with
              send_transaction =
                Map.add_exn model.send_transaction ~key:id ~data:editor_changer
            }
          | Remove id ->
            { model with send_transaction = Map.remove model.send_transaction id }
          | Reset -> { model with send_transaction = Widget_instantiation_id.Map.empty }
          | Send_transaction transaction ->
            if Map.is_empty model.send_transaction
            then (
              (* If the map of transaction-dispatching functions is empty, that
                 means that this transaction would get ignored. Thus, we make a
                 new editor_view to apply the transaction to, and then throw away
                 immediately afterward. *)
              let editor_state = State.Transaction.state (transaction model.state) in
              { model with state = editor_state })
            else (
              Map.iter
                model.send_transaction
                ~f:(fun { send_transaction; modify_editor_view = _ } ->
                  try send_transaction transaction with
                  | error ->
                    eprint_s
                      [%message "failed to send codemirror transaction" (error : exn)]);
              model)
          | Set_state state -> { model with state }
          | Modify_editor_view f ->
            Map.iter
              model.send_transaction
              ~f:(fun { send_transaction = _; modify_editor_view } ->
                try
                  Effect.Expert.handle_non_dom_event_exn
                    (Effect.of_sync_fun (fun () -> modify_editor_view f) ())
                with
                | error -> eprint_s [%message "failed to read editor view" (error : exn)]);
            model
        in
        model, ())
  in
  let%sub { state; _ } = state in
  let path_and_generation = Path_and_generation.model_resetter_generation graph in
  let () =
    Bonsai.Edge.lifecycle
      ~on_deactivate:
        (let%map inject = inject in
         inject Reset)
      graph
  in
  let view =
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_benchmark ->
      let%arr state = state
      and inject = inject
      and path_and_generation = path_and_generation in
      codemirror_widget { state; inject; path_and_generation }
    | `Node | `Node_benchmark | `Node_test ->
      let send_transaction =
        let%arr inject = inject in
        fun transaction -> inject (Send_transaction transaction)
      in
      let%arr state = state
      and send_transaction = send_transaction in
      Vdom.Node.create
        "codemirror"
        ~attrs:
          [ Vdom.Attr.create "data-codemirror-editor" name
          ; For_testing.Inject_hook.attr send_transaction
          ]
        [ Vdom.Node.text (state_text state) ]
  in
  let%arr state = state
  and view = view
  and inject = inject in
  { view
  ; state
  ; send_transaction = (fun transaction -> inject (Send_transaction transaction))
  ; focus = inject (Modify_editor_view View.Editor_view.focus)
  ; blur =
      inject
        (Modify_editor_view
           (fun editor_view -> (View.Editor_view.content_dom editor_view)##blur))
  ; execute_command =
      (fun command ->
        inject
          (Modify_editor_view
             (fun editor_view -> (ignore : bool -> unit) (command editor_view))))
  }
;;

let with_dynamic_extensions
  (type a)
  (module M : Model with type t = a)
  ~equal
  ~name
  ~initial_state
  ~compute_extensions
  value
  graph
  =
  let cm = of_initial_state ~name initial_state graph in
  let callback =
    let%arr cm = cm
    and compute_extensions = compute_extensions in
    fun value ->
      let open State in
      cm.send_transaction (fun state ->
        Editor_state.update
          state
          [ Transaction_spec.create
              ~effects:
                [ State_effect_type.of_
                    State_effect.reconfigure
                    (Extension.of_list (compute_extensions value))
                ]
              ()
          ])
  in
  let () =
    Bonsai.Edge.on_change ~sexp_of_model:[%sexp_of: M.t] ~equal value ~callback graph
  in
  cm
;;

open Parsexp_prefix
module Protocol = Sexp_grammar_completion_protocol

module Completion = struct
  type t =
    { from : int
    ; to_ : int option
    ; options : string list
    ; exhaustive : bool
    }
  [@@deriving sexp_of]
end

let completions ~text ~cursor_position ~completer =
  let sexp_prefix = Sexp_prefix.of_substring ~pos:0 ~len:cursor_position text in
  match sexp_prefix with
  | None | Some (_ :: _, _) ->
    { Completion.from = cursor_position; options = []; to_ = None; exhaustive = true }
  (* The pattern below is a complement of the first pattern in this
     match-expression. In words, it matches the prefix of only the
     first sexp on a string. *)
  | Some (([], _) as sexp_prefix) ->
    let prefix, atom_prefix = Protocol.Prefix.of_sexp_prefix sexp_prefix in
    let exhaustive, options =
      match completer prefix with
      | Ok candidates ->
        let options =
          Protocol.Candidates.candidates candidates
          |> List.filter_map ~f:(fun (candidate : Protocol.Candidate.t) ->
            if Protocol.Candidate.matches_atom_prefix candidate atom_prefix
            then (
              match candidate with
              | Add_atom { atom_signified; _ } ->
                let signifier = Sexp.to_string (sexp_of_string atom_signified) in
                Some signifier
              | Enter_list -> None
              | Enter_list_and_add_atom { atom_signified; _ } ->
                let signifier = Sexp.to_string (sexp_of_string atom_signified) in
                Some ("(" ^ signifier))
            else None)
        in
        candidates.exhaustive, options
      | Error _ -> false, []
    in
    let from, to_ =
      match atom_prefix with
      | Some atom_prefix ->
        let signifier = Atom_prefix.get_signifier ~parser_input:text atom_prefix in
        let starts_with_quote = String.is_prefix signifier ~prefix:"\"" in
        let ends_with_quote =
          match String.get text cursor_position with
          | exception _ -> false
          | '"' -> true
          | _ -> false
        in
        let to_ =
          if starts_with_quote && ends_with_quote
          then cursor_position + 1
          else cursor_position
        in
        cursor_position - (Js.string signifier)##.length, to_
      | None -> cursor_position, cursor_position
    in
    { Completion.from; to_ = Some to_; options; exhaustive }
;;

let autocomplete_extension_of_sexp_grammar ?(include_non_exhaustive_hint = true) grammar =
  let completer = unstage (Sexp_grammar_completion.complete grammar) in
  let completion_source =
    Autocomplete.CompletionSource.of_sync_fun (fun context ->
      let text =
        context
        |> Autocomplete.CompletionContext.state
        |> State.Editor_state.doc
        |> Text.Text.to_json
        |> String.concat ~sep:"\n"
      in
      let cursor_position = Autocomplete.CompletionContext.pos context in
      let { Completion.from; to_; options; exhaustive } =
        completions ~text ~cursor_position ~completer
      in
      let options =
        List.map options ~f:(fun option ->
          Autocomplete.Completion.create ~label:option ())
      in
      let options =
        if (not exhaustive) && include_non_exhaustive_hint
        then (
          let last_option =
            Autocomplete.Completion.create
              ~label:"\"\""
              ~detail:"list is not exhaustive"
              ()
          in
          options @ [ last_option ])
        else options
      in
      Autocomplete.CompletionResult.create ~from ?to_ ~options ~filter:false ())
  in
  Autocomplete.autocompletion
    (Autocomplete.Config.create
       ~activate_on_typing:true
       ~override:[ completion_source ]
       ())
;;

module Grammar = struct
  type t = Sexp_grammar.grammar [@@deriving compare, equal, sexp_of]
end

let with_sexp_grammar_autocompletion
  ?(extra_extension = Basic_setup.basic_setup)
  ?include_non_exhaustive_hint
  ~name
  grammar
  =
  with_dynamic_extensions
    (module Grammar)
    ~equal:[%equal: Grammar.t]
    ~name
    ~initial_state:
      (State.Editor_state.create
         (State.Editor_state_config.create ~extensions:[ extra_extension ] ()))
    ~compute_extensions:
      (Bonsai.return (fun grammar ->
         [ autocomplete_extension_of_sexp_grammar
             ?include_non_exhaustive_hint
             { untyped = grammar }
         ; extra_extension
         ]))
    (let%map grammar = grammar in
     grammar.Sexp_grammar.untyped)
;;

module Private = struct
  module For_tests = struct
    module Completion = Completion

    let completions ~text ~cursor_position ~grammar =
      let completer = unstage (Sexp_grammar_completion.complete grammar) in
      completions ~text ~cursor_position ~completer
    ;;

    module Path_and_generation = Path_and_generation
  end
end
