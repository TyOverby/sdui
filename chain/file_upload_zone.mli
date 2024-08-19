open! Bonsai_web.Cont
open! Js_of_ocaml

val attr
  :  ?mime_types:string list
  -> on_file_upload:(File.file Js.t list -> unit Ui_effect.t)
  -> unit
  -> Vdom.Attr.t
