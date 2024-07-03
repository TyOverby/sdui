open! Core
open! Bonsai_web.Cont
open! Async_kernel
module Form := Bonsai_web_ui_form.With_manual_view

type t =
  ( Txt2img.Query.t
    , on_submit:unit Effect.t -> hosts_panel:Vdom.Node.t -> Vdom.Node.t )
    Form.t

val component
  :  request_host:Hosts.request_host Bonsai.t
  -> available_hosts:Hosts.Host.Set.t Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t * (Models.t, Vdom.Node.t) Form.t Bonsai.t
