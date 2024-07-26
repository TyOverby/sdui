open! Core
open! Bonsai_web.Cont
module Form := Bonsai_web_ui_form.With_manual_view

module Parameters : sig
  type t
end

val component
  :  default_size:int
  -> pool:(Sd.Hosts.Host.t, _, _) Lease_pool.t
  -> prev:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> mask:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> Bonsai.graph
  -> Sd.Image.t Inc.Or_error_or_stale.t Bonsai.t
     * (Vdom.Node.t * Vdom.Node.t) Bonsai.t
     * (Parameters.t, unit) Form.t Bonsai.t
