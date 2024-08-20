open! Core
open! Bonsai_web.Cont
module Form := Bonsai_web_ui_form.With_manual_view

type t =
  { image : Sd.Image.t Inc.t
  ; gallery_view : Vdom.Node.t Bonsai.t
  ; form_view : Vdom.Node.t Bonsai.t
  ; form : (Parameters.t, unit) Form.t Bonsai.t
  }

val component
  :  direction:[ `Vertical | `Horizontal ]
  -> pool:(Sd.Hosts.Host.t, _, _) Lease_pool.t
  -> prev:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> mask:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> Bonsai.graph
  -> t
