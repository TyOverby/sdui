open! Core

type t = Bonsai_web_ui_view.Expert.For_codemirror.Theme.t =
  | Basic_dark
  | Basic_light
  | Gruvbox_dark
  | Nord
  | Solarized_dark
  | Solarized_light
  | Material_dark
  | Vscode_dark
  | Vscode_light
[@@deriving compare, enumerate, equal, sexp, typed_variants]

val get : t -> Codemirror.State.Extension.t
val to_string : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp]
  end
end
