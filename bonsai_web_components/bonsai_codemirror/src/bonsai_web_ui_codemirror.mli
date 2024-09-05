open! Core
module Bonsai_proc := Bonsai_web.Proc
open! Bonsai_web
open Virtual_dom
open Codemirror

module Transaction : sig
  type t = State.Editor_state.t -> State.Transaction.t

  val set_lines : string list -> t
end

type t

val view : t -> Vdom.Node.t
val state : t -> State.Editor_state.t
val send_transaction : t -> Transaction.t -> unit Effect.t
val focus : t -> unit Effect.t
val blur : t -> unit Effect.t
val execute_command : t -> View.Command.t -> unit Effect.t

(** A codemirror text editor component integrated into Bonsai. The codemirror
    reference manual can be found at [https://codemirror.net/6/docs/ref/].
*)
val of_initial_state
  :  name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in tests. *)
  -> State.Editor_state.t
  -> Bonsai.graph
  -> t Bonsai.t

(** Uses edge-triggering to re-configure the set of extensions whenever the
    input value changes. Any extensions specified in [initial_state] will get
    overwritten by the new set of extensions.

    If you want to fully understand what is happening, your best bet is to read
    codemirror's documentation and the source code for this function. The
    interactions between everything involved in this function is too complex to
    fully explain in a doc comment. *)
val with_dynamic_extensions
  :  (module Bonsai_proc.Model with type t = 'a)
  -> equal:('a -> 'a -> bool)
  -> name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in tests. *)
  -> initial_state:State.Editor_state.t
  -> compute_extensions:('a -> State.Extension.t list) Bonsai.t
  -> 'a Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t

(** An easy-to-use function for making a codemirror textbox with autocompletion
    based on a potentially dynamic sexp-grammar. This is a good way to get
    started using Codemirror - if you want a more complex configuration, you
    can use other functions in this module then. *)
val with_sexp_grammar_autocompletion
  :  ?extra_extension:Codemirror.State.Extension.t
       (** [extra_extension] defaults to [Basic_setup.basic_setup] *)
  -> ?include_non_exhaustive_hint:bool (** see [autocomplete_extension_of_sexp_grammar] *)
  -> name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in tests. *)
  -> 'a Sexp_grammar.t Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t

val text : t -> string
val set_lines : t -> string list -> unit Effect.t

val autocomplete_extension_of_sexp_grammar
  :  ?include_non_exhaustive_hint:bool
       (** Whether to add an extra autocomplete entry indicating that the autocomplete list is
      not exhaustive. The default is [true]. *)
  -> 'a Sexp_grammar.t
  -> State.Extension.t

module For_testing : sig
  val type_id : (Transaction.t -> unit Effect.t) Type_equal.Id.t
end

module Private : sig
  module For_tests : sig
    module Completion : sig
      type t =
        { from : int
        ; to_ : int option
        ; options : string list
        ; exhaustive : bool
        }
      [@@deriving sexp_of]
    end

    val completions
      :  text:string
      -> cursor_position:int
      -> grammar:'a Sexp_grammar.t
      -> Completion.t

    module Path_and_generation = Path_and_generation
  end
end
