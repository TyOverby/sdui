open! Core
open! Bonsai_web.Cont
open! Async_kernel
module Form := Bonsai_web_ui_form.With_automatic_view

type t =
  { form : Txt2img.Query.t Form.t
  ; form_view : on_submit:unit Effect.t -> hosts_panel:Vdom.Node.t -> Vdom.Node.t
  }

val component
  :  request_host:Hosts.request_host Bonsai.t
  -> available_hosts:String.Set.t Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t
