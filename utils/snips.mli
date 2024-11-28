open! Core
module Vdom := Virtual_dom.Vdom

type open_
type closed
type 'w t [@@deriving sexp_of]

module Length : sig
  type t =
    [ `Auto
    | `Fr of float
    | `Max_content
    | `Min_content
    | `Fit_content of Css_gen.Length.t
    | `Min_max of t * t
    | Css_gen.Length.t
    ]

  val to_string_css : t -> string
end

module Scroll_config : sig
  type t

  (** The [gutter] argument allows you to reserve space for a scrollbar to appear, even if
      there isn't enough content to display a scrollbar in an [overflow: auto] container.
      You can use this to prevent layout shifts when your content grows large enough to
      show the scrollbar. See
      https://developer.mozilla.org/en-US/docs/Web/CSS/scrollbar-gutter for more info. *)
  type gutter :=
    [ `Unstable
    | `Stable
    | `Stable_both_edges
    ]

  val no_scrolling : t
  val only_on_primary : ?gutter:gutter -> unit -> t
  val for_both : ?gutter:gutter -> unit -> t
end

val ( |+| ) : open_ t -> 'b t -> 'b t
val none : open_ t

val top
  :  ?scroll:Scroll_config.t
  -> ?size:Length.t
  -> ?attr:Vdom.Attr.t
  -> Vdom.Node.t
  -> open_ t

val right
  :  ?scroll:Scroll_config.t
  -> ?size:Length.t
  -> ?attr:Vdom.Attr.t
  -> Vdom.Node.t
  -> open_ t

val bottom
  :  ?scroll:Scroll_config.t
  -> ?size:Length.t
  -> ?attr:Vdom.Attr.t
  -> Vdom.Node.t
  -> open_ t

val left
  :  ?scroll:Scroll_config.t
  -> ?size:Length.t
  -> ?attr:Vdom.Attr.t
  -> Vdom.Node.t
  -> open_ t

val split_h : closed t list -> closed t
val split_v : closed t list -> closed t
val body : ?scroll:Scroll_config.t -> ?attr:Vdom.Attr.t -> Vdom.Node.t -> closed t
val render : ?container_attr:Vdom.Attr.t -> closed t -> Vdom.Node.t

module Infix : sig
  val ( |+| ) : open_ t -> 'b t -> 'b t
end
