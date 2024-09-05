open! Core
open! Bonsai_web

type t =
  { view : Vdom.Node.t
  ; is_open : bool
  ; open_ : unit Effect.t
  ; close : unit Effect.t
  ; toggle : unit Effect.t
  }

val component
  :  ?extra_container_attrs:Vdom.Attr.t list Bonsai.t
  -> ?extra_title_attrs:Vdom.Attr.t list Bonsai.t
  -> ?extra_content_attrs:Vdom.Attr.t list Bonsai.t
  -> starts_open:bool
  -> title:Vdom.Node.t Bonsai.t
  -> content:(Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> unit
  -> Bonsai.graph
  -> t Bonsai.t
