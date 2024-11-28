open! Core
module Vdom = Virtual_dom.Vdom

module Style =
  [%css
  stylesheet
    {|
@layer snips {
  *:has(.root),
  :root:has(> body > .root),
  body:has(> .root),
  .root {
    margin:0;
    padding:0;
    width:100%;
    height:100%;
  }

  .root {
    overflow:hidden;
  }

  :root:has(> body > .root),
  body:has(> .root) {
    overflow:clip;
  }
}
|}]

type open_
type closed

module Scroll_config = struct
  type gutter =
    [ `Unstable
    | `Stable
    | `Stable_both_edges
    ]
  [@@deriving sexp_of]

  type t =
    | No_scrolling
    | Only_on_primary of { gutter : gutter }
    | For_both of { gutter : gutter }
  [@@deriving sexp_of]

  let no_scrolling = No_scrolling
  let only_on_primary ?(gutter = (`Unstable : gutter)) () = Only_on_primary { gutter }
  let for_both ?(gutter = (`Unstable : gutter)) () = For_both { gutter }

  let gutter_to_attr = function
    | `Unstable -> Css_gen.empty
    | `Stable -> Css_gen.create ~field:"scrollbar-gutter" ~value:"stable"
    | `Stable_both_edges ->
      Css_gen.create ~field:"scrollbar-gutter" ~value:"stable both-edges"
  ;;

  let to_attr which t =
    match t with
    | No_scrolling -> Vdom.Attr.style (Css_gen.overflow `Hidden)
    | For_both { gutter } ->
      Vdom.Attr.style Css_gen.(overflow `Auto @> gutter_to_attr gutter)
    | Only_on_primary { gutter } ->
      let primary, secondary =
        match which with
        | `Top_and_bottom -> Css_gen.overflow_x, Css_gen.overflow_y
        | `Left_and_right -> Css_gen.overflow_y, Css_gen.overflow_x
      in
      Vdom.Attr.style
        Css_gen.(primary `Auto @> secondary `Hidden @> gutter_to_attr gutter)
  ;;
end

module Length = struct
  type t =
    [ `Auto
    | `Fr of float
    | `Max_content
    | `Min_content
    | `Fit_content of Css_gen.Length.t
    | `Min_max of t * t
    | Css_gen.Length.t
    ]
  [@@deriving sexp_of]

  let rec to_string_css = function
    | #Css_gen.Length.t as t -> Css_gen.Length.to_string_css t
    | `Fit_content t -> sprintf "fit-content(%s)" (Css_gen.Length.to_string_css t)
    | `Auto -> "auto"
    | `Fr f -> Virtual_dom.Dom_float.to_string_precision 6 f
    | `Max_content -> "max-content"
    | `Min_content -> "min-content"
    | `Min_max (min, max) ->
      sprintf "minmax(%s, %s)" (to_string_css min) (to_string_css max)
  ;;
end

module Tree = struct
  type kind =
    | Top
    | Right
    | Bottom
    | Left
  [@@deriving sexp_of]

  type t =
    | Edge of
        { kind : kind
        ; view : (Vdom.Node.t[@sexp.opaque])
        ; attr : (Vdom.Attr.t[@sexp.opaque])
        ; size : Length.t
        ; scroll_config : Scroll_config.t
        ; next : t
        ; rows : int
        ; cols : int
        }
    | Split of
        { direction : [ `Horizontal | `Vertical ]
        ; children : t list
        }
    | Body of
        { view : (Vdom.Node.t[@sexp.opaque])
        ; attr : (Vdom.Attr.t[@sexp.opaque])
        ; scroll_config : Scroll_config.t
        }
  [@@deriving sexp_of]

  let rec rows = function
    | Edge { rows; _ } -> rows
    | Split { direction = `Horizontal; children = _ } -> 1
    | Split { direction = `Vertical; children } -> List.sum (module Int) children ~f:rows
    | Body _ -> 1
  ;;

  let rec cols = function
    | Edge { cols; _ } -> cols
    | Split { direction = `Vertical; children = _ } -> 1
    | Split { direction = `Horizontal; children } ->
      List.sum (module Int) children ~f:cols
    | Body _ -> 1
  ;;
end

module Builder = struct
  type 'w t =
    | Continuation : (Tree.t -> Tree.t) -> open_ t
    | Terminal : Tree.t -> closed t
  [@@deriving sexp_of]

  let make_edge
    (kind : Tree.kind)
    ?(scroll = Scroll_config.only_on_primary ())
    ?(size = (`Auto : Length.t))
    ?(attr = Vdom.Attr.empty)
    view
    =
    Continuation
      (fun next ->
        let rows, cols =
          match kind with
          | Top | Bottom -> Tree.rows next + 1, Tree.cols next
          | Left | Right -> Tree.rows next, Tree.cols next + 1
        in
        Edge { kind; view; next; rows; cols; attr; size; scroll_config = scroll })
  ;;

  let none = Continuation Fn.id
  let top ?scroll ?size ?attr view = make_edge Top ?scroll ?size ?attr view
  let right ?scroll ?size ?attr view = make_edge Right ?scroll ?size ?attr view
  let bottom ?scroll ?size ?attr view = make_edge Bottom ?scroll ?size ?attr view
  let left ?scroll ?size ?attr view = make_edge Left ?scroll ?size ?attr view

  let body ?(scroll = Scroll_config.for_both ()) ?(attr = Vdom.Attr.empty) view =
    Terminal (Body { attr; view; scroll_config = scroll })
  ;;

  let split ~direction (children : closed t list) =
    let children = List.map children ~f:(fun (Terminal t) -> t) in
    Terminal (Tree.Split { direction; children })
  ;;

  let split_h = split ~direction:`Horizontal
  let split_v = split ~direction:`Vertical

  let ( |+| ) : type w. open_ t -> w t -> w t =
    fun l r ->
    match l, r with
    | Continuation f, Continuation g -> Continuation (fun h -> f (g h))
    | Continuation f, Terminal t -> Terminal (f t)
  ;;
end

let style field value = Vdom.Attr.style (Css_gen.create ~field ~value)

let grid_area ~left ~top ~cols ~rows =
  style "grid-area" (sprintf "%d / %d / span %d / span %d" top left rows cols)
;;

let for_body = "1fr"

let rec column_template = function
  | Tree.Body _ -> for_body
  | Edge { kind = Left; next; size; _ } ->
    Length.to_string_css size ^ " " ^ column_template next
  | Edge { kind = Right; next; size; _ } ->
    column_template next ^ " " ^ Length.to_string_css size
  | Edge { kind = Top | Bottom; next; _ } -> column_template next
  | Split { direction = `Horizontal; children } ->
    List.map children ~f:(fun _ -> for_body) |> String.concat ~sep:" "
  | Split { direction = `Vertical; children = _ } -> for_body
;;

let rec row_template = function
  | Tree.Body _ -> for_body
  | Edge { kind = Top; next; size; _ } ->
    Length.to_string_css size ^ " " ^ row_template next
  | Edge { kind = Bottom; size; next; _ } ->
    row_template next ^ " " ^ Length.to_string_css size
  | Edge { kind = Left | Right; next; _ } -> row_template next
  | Split { direction = `Vertical; children } ->
    List.map children ~f:(fun _ -> for_body) |> String.concat ~sep:" "
  | Split { direction = `Horizontal; children = _ } -> for_body
;;

let rec traverse ~left ~top = function
  | Tree.Body { view; attr; scroll_config } ->
    let grid_attr = grid_area ~left ~top ~cols:1 ~rows:1 in
    let scroll_attr = Scroll_config.to_attr `Top_and_bottom scroll_config in
    let attrs = [ grid_attr; scroll_attr; attr ] in
    [ Vdom.Node.div ~attrs [ view ] ]
  | Split { direction = `Horizontal; children } ->
    List.mapi children ~f:(fun i child ->
      let attr = Vdom.Attr.many [ grid_area ~left:(left + i) ~top ~cols:1 ~rows:1 ] in
      render ~container_attr:attr (Terminal child))
  | Split { direction = `Vertical; children } ->
    List.mapi children ~f:(fun i child ->
      let attr = Vdom.Attr.many [ grid_area ~left ~top:(top + i) ~cols:1 ~rows:1 ] in
      render ~container_attr:attr (Terminal child))
  | Edge { kind; next; view; attr; rows; cols; scroll_config; size = _ } ->
    let grid_attr, rest =
      match kind with
      | Left ->
        let attr = grid_area ~left ~top ~cols:1 ~rows in
        let rest = traverse ~left:(left + 1) ~top next in
        attr, rest
      | Top ->
        let attr = grid_area ~left ~top ~cols ~rows:1 in
        let rest = traverse ~left ~top:(top + 1) next in
        attr, rest
      | Bottom ->
        let rest = traverse ~left ~top next in
        let attr = grid_area ~left ~top:(top + rows - 1) ~cols ~rows:1 in
        attr, rest
      | Right ->
        let attr = grid_area ~left:(left + cols - 1) ~top ~cols:1 ~rows in
        let rest = traverse ~left ~top next in
        attr, rest
    in
    let scroll_attr =
      let which =
        match kind with
        | Left | Right -> `Left_and_right
        | Top | Bottom -> `Top_and_bottom
      in
      Scroll_config.to_attr which scroll_config
    in
    Vdom.Node.div ~attrs:[ grid_attr; scroll_attr; attr ] [ view ] :: rest

and render : ?container_attr:Vdom.Attr.t -> closed Builder.t -> Vdom.Node.t =
  fun ?(container_attr = Vdom.Attr.empty) (Terminal layout) ->
  let views = traverse ~left:1 ~top:1 layout in
  let attr =
    let column_template = column_template layout in
    let row_template = row_template layout in
    Vdom.Attr.many
      [ style "display" "grid"
      ; style "grid-template-columns" column_template
      ; style "grid-template-rows" row_template
      ; Style.root
      ]
  in
  Vdom.Node.div ~attrs:[ attr; container_attr ] views
;;

include Tree
include Builder

module Infix = struct
  let ( |+| ) = ( |+| )
end
