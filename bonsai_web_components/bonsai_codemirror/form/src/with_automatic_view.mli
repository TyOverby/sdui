open! Core
module Bonsai_proc := Bonsai_web.Proc
open Bonsai_web
module Form := Bonsai_web_ui_form.With_automatic_view

(* Bonsai form input elements that use Codemirror as their input elements. Each module
   below provides different behaviour corresponding to the different options in
   [Bonsai_web_ui_codemirror]. *)

(* [Basic] forms use the most barebones Codemirror configuration. *)
module Basic : sig
  val string : ?name:string -> unit -> Bonsai.graph -> string Form.t Bonsai.t

  val stringable
    :  ?name:string
    -> (module Stringable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val sexpable
    :  ?name:string
    -> (module Sexpable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

(* [Dynamic_extensions] forms have the ability to dynamically choose which extensions to
   load, depending on the value of the ['model Value.t] that is supplied. *)
module Dynamic_extensions : sig
  val string
    :  (module Bonsai_proc.Model with type t = 'model)
    -> equal:('model -> 'model -> bool)
    -> ?name:string
    -> compute_extensions:('model -> Codemirror.State.Extension.t list) Bonsai.t
    -> 'model Bonsai.t
    -> Bonsai.graph
    -> string Form.t Bonsai.t

  val stringable
    :  (module Bonsai_proc.Model with type t = 'model)
    -> (module Stringable with type t = 'a)
    -> equal:('model -> 'model -> bool)
    -> ?name:string
    -> compute_extensions:('model -> Codemirror.State.Extension.t list) Bonsai.t
    -> 'model Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val sexpable
    :  (module Bonsai_proc.Model with type t = 'model)
    -> (module Sexpable with type t = 'a)
    -> equal:('model -> 'model -> bool)
    -> ?name:string
    -> compute_extensions:('model -> Codemirror.State.Extension.t list) Bonsai.t
    -> 'model Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

(* [Sexp_grammar_autocomplete] forms provide users with auto-complete results based on the
   provided Sexp_grammar. *)
module Sexp_grammar_autocomplete : sig
  val string
    :  ?name:string
    -> ?extra_extension:Codemirror.State.Extension.t
    -> _ Sexp_grammar.t Bonsai.t
    -> Bonsai.graph
    -> string Form.t Bonsai.t

  val stringable
    :  (module Stringable with type t = 'a)
    -> ?name:string
    -> ?extra_extension:Codemirror.State.Extension.t
    -> 'a Sexp_grammar.t Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val sexpable
    :  (module Sexpable with type t = 'a)
    -> ?name:string
    -> ?extra_extension:Codemirror.State.Extension.t
    -> 'a Sexp_grammar.t Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end
