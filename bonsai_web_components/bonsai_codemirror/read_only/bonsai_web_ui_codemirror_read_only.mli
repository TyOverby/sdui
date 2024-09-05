open! Core
open Virtual_dom

module Language : sig
  type t =
    | Plaintext
    | OCaml
    | Diff
    | Html
    | Css
    | Python
    | Common_lisp
    | Scheme
    | Sql
    | Javascript
    | Markdown
    | Php
    | Rust
    | Xml
    | FSharp
end

module Theme : sig
  type t =
    | Basic_dark
    | Basic_light
    | Gruvbox_dark
    | Nord
    | Solarized_dark
    | Solarized_light
    | Material_dark
    | Vscode_dark
    | Vscode_light
end

val make
  :  ?extension:Codemirror.State.Extension.t
  -> ?line_numbers:bool
  -> ?line_wrapping:bool
  -> ?on_line_number_click:(int -> unit Ui_effect.t)
  -> ?scroll_to:int
  -> language:Language.t
  -> theme:Theme.t
  -> string
  -> Vdom.Node.t
