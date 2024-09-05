open! Core
open! Bonsai_web

module Theming : sig
  type t =
    [ `Legacy_don't_use_theme
    | `Themed
    ]
end

module Themed : sig
  type t

  val create : ?autosize:bool -> View.Theme.t -> unit -> Theming.t -> t
end

module Header_label : sig
  val wrap_clickable
    :  sortable:bool
    -> handle_click:Vdom.Attr.t
    -> Vdom.Node.t
    -> Vdom.Node.t

  val wrap_with_icon
    :  ?sort_indicator_attrs:Vdom.Attr.t list
    -> Vdom.Node.t
    -> Bonsai_web_ui_partial_render_table_protocol.Sort_state.t
    -> Vdom.Node.t
end

module Header : sig
  module Header_cell : sig
    type t

    val leaf_view
      :  Themed.t
      -> column_width:Css_gen.Length.t
      -> set_column_width:([> `Px_float of float ] -> unit Ui_effect.t)
      -> visible:bool
      -> resizable:bool
      -> label:Vdom.Node.t
      -> autosize:bool
      -> unit
      -> t

    val spacer_view : Themed.t -> colspan:int -> autosize:bool -> unit -> t

    val group_view
      :  Themed.t
      -> colspan:int
      -> autosize:bool
      -> label:Vdom.Node.t
      -> unit
      -> t
  end

  module Header_row : sig
    type t

    val view : Themed.t -> Header_cell.t list -> t
  end

  type t

  val view
    :  Themed.t
    -> set_header_client_rect:
         (Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.t -> unit Ui_effect.t)
    -> autosize:bool
    -> Header_row.t list
    -> t
end

module Cell : sig
  module Col_styles : sig
    type t

    val create
      :  (module Bonsai.Comparator
            with type t = 'column_id
             and type comparator_witness = 'cmp)
      -> themed_attrs:Themed.t
      -> autosize:bool
      -> row_height:int
      -> col_widths:('column_id, Column_size.t, 'cmp) Map.t
      -> leaves:'column_id Header_tree.leaf list
      -> ('column_id -> t) Staged.t
  end

  type t

  val view
    :  Themed.t
    -> is_focused:bool
    -> col_styles:Col_styles.t
    -> on_cell_click:unit Effect.t
    -> autosize:bool
    -> Vdom.Node.t
    -> t
end

module Row : sig
  module Styles : sig
    type t

    val create : row_height:int -> row_width:float -> autosize:bool -> t
  end

  type t

  val view
    :  Themed.t
    -> styles:Styles.t
    -> is_focused:bool
    -> extra_attrs:Vdom.Attr.t list
    -> autosize:bool
    -> Cell.t list
    -> t
end

module Body : sig
  type t

  module Body_row_key : sig
    type t =
      | Top_padding
      | Row of Opaque_map.Key.t
      | Bottom_border
      | Bottom_padding
    [@@deriving compare, sexp, equal]

    include Comparable.S with type t := t
  end

  val view
    :  Themed.t
    -> padding_top:int
    -> padding_bottom:int
    -> rows:(Body_row_key.t, Row.t, Body_row_key.comparator_witness) Map.t
    -> autosize:bool
    -> t
end

module Table : sig
  val view
    :  Themed.t
    -> private_body_classname:string
    -> vis_change_attr:Vdom.Attr.t
    -> total_height:int
    -> autosize:bool
    -> Header.t
    -> Body.t
    -> Vdom.Node.t
end
