open! Core
open! Bonsai_web

type lease_pool :=
  ( Sd.Hosts.Host.t
    , Sd.Hosts.Current_model.t
    , Sd.Hosts.Host.comparator_witness )
    Sd_chain.Lease_pool.t

val component
  :  lease_pool:lease_pool
  -> inject:(Sd.Image.t Or_error.t -> Sd_chain.Parameters.t -> unit Ui_effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> (view:(host_monitor:Vdom.Node.t -> Vdom.Node.t)
     * generate_action:Time_ns.Span.t option Effect.t)
       Bonsai.t
