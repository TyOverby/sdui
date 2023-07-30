open! Core
open! Bonsai_web
open! Async_kernel
module Form := Bonsai_web_ui_form

type t =
  { form : Txt2img.Query.t Form.t
  ; form_view : on_submit:unit Effect.t -> Vdom.Node.t
  ; width : int
  ; height : int
  }

val component : host_and_port:string Value.t -> t Computation.t
