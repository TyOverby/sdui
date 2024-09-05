open! Core
open Bonsai_web
open Virtual_dom

val message_for_async_durable : Time_ns.Span.t -> string

(** A warning box which appears in the bottom-right corner of a web page if the
    input is true, and behaves like [Vdom.Node.none] if the input is false.

    This component comes with some default styles, but those may be overriden.
    The [create_message] function is given the length of time that the input
    value has been false (i.e. how long the server has been disconnected).

    If you are using async_durable, we recommend defaulting to
    [message_for_async_durable].
*)
val component
  :  ?styles:Vdom.Attr.t
  -> create_message:(Time_ns.Span.t -> string)
  -> bool Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** like [component], but allows for creating a more flexible vdom structure. *)
val custom
  :  create_message:(Time_ns.Span.t -> Vdom.Node.t) Bonsai.t
  -> bool Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
