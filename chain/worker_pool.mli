open! Core
open! Bonsai_web.Cont

type t =
  { enqueue : 'a. Sd.Models.t -> (Sd.Hosts.Host.t -> 'a Ui_effect.t) -> 'a Ui_effect.t
  ; debug : Vdom.Node.t
  }

val component : hosts:Sd.Hosts.Host.Set.t Bonsai.t -> Bonsai.graph -> t Bonsai.t
