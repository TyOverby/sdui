open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Codemirror = Bonsai_web_ui_codemirror

(** This is a component used for demos.

    It showcases computations alongside their source code. *)

module type Demo = sig
  val name : string
  val description : string
  val view : Bonsai.graph -> (Vdom.Node.t * string) Bonsai.t
  val selector : string option
  val filter_attrs : (string -> string -> bool) option
end

val make_demo : (module Demo) -> Bonsai.graph -> Vdom.Node.t Bonsai.t

type t =
  { code : Vdom.Node.t
  ; demo : Vdom.Node.t
  }

val make_demo'
  :  ?selector:string
  -> ?filter_attrs:(string -> string -> bool)
  -> ?ocaml_label:string option
  -> ?hide_html:bool
  -> demo:Vdom.Node.t Bonsai.t
  -> code:string Bonsai.t
  -> unit
  -> Bonsai.graph
  -> t Bonsai.t

val make_sections
  :  theme_picker:Vdom.Node.t Bonsai.t
  -> (string * string * (Bonsai.graph -> Vdom.Node.t Bonsai.t) list) list
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

val wrap_application
  :  ?attr:Vdom.Attr.t Bonsai.t
  -> theme_picker:Vdom.Node.t Bonsai.t
  -> Vdom.Node.t list Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

module Theme_picker = Theme_picker
