open! Core
open! Bonsai_web

module Pills : sig
  val of_list
    :  ?extra_container_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_pill_attr:Vdom.Attr.t Bonsai.t
    -> to_string:('a -> string) Bonsai.t
    -> inject_selected_options:('a list -> unit Effect.t) Bonsai.t
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> Vdom.Node.t Bonsai.t

  val of_set
    :  ?extra_container_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_pill_attr:Vdom.Attr.t Bonsai.t
    -> to_string:('a -> string) Bonsai.t
    -> inject_selected_options:(('a, 'cmp) Set.t -> unit Effect.t) Bonsai.t
    -> ('a, 'cmp) Set.t Bonsai.t
    -> Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end
