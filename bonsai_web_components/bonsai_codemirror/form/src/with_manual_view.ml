open! Core

module type Bonsai_model = Bonsai_web.Proc.Model

open Bonsai.Let_syntax
module Codemirror_ui = Bonsai_web_ui_codemirror
module Form = Bonsai_web_ui_form.With_manual_view

module Make_forms (M : sig
    type t

    val create_codemirror : name:string -> t -> Bonsai.graph -> Codemirror_ui.t Bonsai.t
  end) =
struct
  let string ?(name = "Bonsai_web_ui_codemirror_form") t graph =
    let codemirror = M.create_codemirror ~name t graph in
    let%arr codemirror = codemirror in
    { Form.view = Codemirror_ui.view codemirror
    ; value = Ok (Codemirror_ui.text codemirror)
    ; set =
        (fun s ->
          Codemirror_ui.send_transaction
            codemirror
            (Codemirror_ui.Transaction.set_lines (String.split_lines s)))
    }
  ;;

  let stringable (type a) (module M : Stringable with type t = a) ?name t graph =
    let%map.Bonsai form = string ?name t graph in
    Form.project form ~parse_exn:M.of_string ~unparse:M.to_string
  ;;

  let sexpable (type a) (module M : Sexpable with type t = a) ?name t graph =
    let%map.Bonsai form = string ?name t graph in
    Form.project
      form
      ~parse_exn:(fun s -> [%of_sexp: M.t] (Sexp.of_string s))
      ~unparse:(fun t -> Sexp.to_string_hum ~indent:2 ([%sexp_of: M.t] t))
  ;;
end

let empty_state =
  Codemirror.State.Editor_state.create (Codemirror.State.Editor_state_config.create ())
;;

module Basic = struct
  module Forms = Make_forms (struct
      type t = unit

      let create_codemirror ~name () = Codemirror_ui.of_initial_state ~name empty_state
    end)

  let string = Forms.string

  let stringable (type a) ?name (module M : Stringable with type t = a) =
    Forms.stringable (module M) ?name ()
  ;;

  let sexpable (type a) ?name (module M : Sexpable with type t = a) =
    Forms.sexpable (module M) ?name ()
  ;;
end

module Dynamic_extensions = struct
  type t =
    | T :
        { model : (module Bonsai_model with type t = 'a)
        ; equal : 'a -> 'a -> bool
        ; value : 'a Bonsai.t
        ; compute_extensions : ('a -> Codemirror.State.Extension.t list) Bonsai.t
        }
        -> t

  module Forms = Make_forms (struct
      type nonrec t = t

      let create_codemirror ~name (T { model; value; compute_extensions; equal }) =
        Codemirror_ui.with_dynamic_extensions
          model
          ~equal
          ~name
          ~initial_state:empty_state
          ~compute_extensions
          value
      ;;
    end)

  let string
    (type a)
    (module M : Bonsai_model with type t = a)
    ~equal
    ?name
    ~compute_extensions
    value
    =
    Forms.string ?name (T { model = (module M); equal; value; compute_extensions })
  ;;

  let stringable
    (type model a)
    (module M : Bonsai_model with type t = model)
    (module S : Stringable with type t = a)
    ~equal
    ?name
    ~compute_extensions
    value
    =
    Forms.stringable
      (module S)
      ?name
      (T { model = (module M); equal; value; compute_extensions })
  ;;

  let sexpable
    (type model a)
    (module M : Bonsai_model with type t = model)
    (module S : Sexpable with type t = a)
    ~equal
    ?name
    ~compute_extensions
    value
    =
    Forms.sexpable
      (module S)
      ?name
      (T { model = (module M); equal; value; compute_extensions })
  ;;
end

module Sexp_grammar_autocomplete = struct
  type t =
    | T :
        { extra_extension : Codemirror.State.Extension.t option
        ; sexp_grammar : 'a Sexp_grammar.t Bonsai.t
        }
        -> t

  module Forms = Make_forms (struct
      type nonrec t = t

      let create_codemirror ~name (T { extra_extension; sexp_grammar }) =
        Codemirror_ui.with_sexp_grammar_autocompletion ?extra_extension ~name sexp_grammar
      ;;
    end)

  let string ?name ?extra_extension sexp_grammar =
    Forms.string ?name (T { extra_extension; sexp_grammar })
  ;;

  let stringable
    (type a)
    (module M : Stringable with type t = a)
    ?name
    ?extra_extension
    sexp_grammar
    =
    Forms.stringable (module M) ?name (T { extra_extension; sexp_grammar })
  ;;

  let sexpable
    (type a)
    (module M : Sexpable with type t = a)
    ?name
    ?extra_extension
    sexp_grammar
    =
    Forms.sexpable (module M) ?name (T { extra_extension; sexp_grammar })
  ;;
end
