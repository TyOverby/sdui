open! Core
open! Bonsai_web
open! Async_kernel
module Form := Bonsai_web_ui_form

type t =
  { form : Txt2img.Query.t Form.t
  ; width : int
  ; height : int
  }

val component : t Computation.t
