open! Core
open! Bonsai_web

type lease_pool :=
  ( Sd.Hosts.Host.t
    , Sd.Hosts.Current_model.t
    , Sd.Hosts.Host.comparator_witness )
    Sd_chain.Lease_pool.t

val component
  :  lease_pool:lease_pool
  -> inject:(Image_tree.Action.t -> unit Ui_effect.t) Bonsai.t
  -> id:Image_tree.Unique_id.t Bonsai.t
  -> local_ Bonsai.graph
  -> ((state_tree:Vdom.Node.t -> Vdom.Node.t) * Vdom_keyboard.Keyboard_event_handler.t)
       Bonsai.t
