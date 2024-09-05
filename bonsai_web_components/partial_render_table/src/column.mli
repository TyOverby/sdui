open! Core
open! Bonsai_web
module Sort_state := Bonsai_web_ui_partial_render_table_protocol.Sort_state
module Sort_kind := Bonsai_web_ui_partial_render_table_protocol.Sort_kind

module Indexed_column_id : sig
  type t [@@deriving equal, sexp]

  val to_int : t -> int
  val of_int : int -> t
end

module Dynamic_cells : sig
  type ('key, 'data) t

  val column
    :  ?initial_width:Css_gen.Length.t
    -> ?visible:bool Bonsai.t
    -> ?resizable:bool Bonsai.t
    -> header:Vdom.Node.t Bonsai.t
    -> cell:
         (key:'key Bonsai.t
          -> data:'data Bonsai.t
          -> Bonsai.graph
          -> Vdom.Node.t Bonsai.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t -> ('key, 'data) t
  val lift : ('key, 'data) t list -> ('key, 'data, Indexed_column_id.t) Column_intf.t

  module Sortable = Sortable
end

module Dynamic_columns : sig
  type ('key, 'data) t

  val column
    :  ?initial_width:Css_gen.Length.t
    -> ?visible:bool
    -> ?resizable:bool
    -> header:Vdom.Node.t
    -> cell:(key:'key -> data:'data -> Vdom.Node.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t -> ('key, 'data) t -> ('key, 'data) t

  val lift
    :  ('key, 'data) t list Bonsai.t
    -> ('key, 'data, Indexed_column_id.t) Column_intf.t

  module Sortable = Sortable
end

module Dynamic_experimental : sig
  val build
    :  ('column_id, _) Bonsai.comparator
    -> columns:'column_id list Bonsai.t
    -> render_header:('column_id Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> render_cell:
         ('column_id Bonsai.t
          -> 'key Bonsai.t
          -> 'data Bonsai.t
          -> Bonsai.graph
          -> Vdom.Node.t Bonsai.t)
    -> ('key, 'data, 'column_id) Column_intf.t

  module Sortable = Sortable
end

module Dynamic_cells_with_sorter : sig
  type ('key, 'data) t

  val column
    :  ?sort:('key * 'data -> 'key * 'data -> int) Bonsai.t
    -> ?sort_reversed:('key * 'data -> 'key * 'data -> int) Bonsai.t
    -> ?initial_width:Css_gen.Length.t
    -> ?visible:bool Bonsai.t
    -> ?resizable:bool Bonsai.t
    -> header:(Sort_state.t -> Vdom.Node.t) Bonsai.t
    -> cell:
         (key:'key Bonsai.t
          -> data:'data Bonsai.t
          -> Bonsai.graph
          -> Vdom.Node.t Bonsai.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t -> ('key, 'data) t

  val lift
    :  ('key, 'data) t list
    -> ('key, 'data, Indexed_column_id.t) Column_intf.with_sorter

  module Sortable = Sortable
end

module Dynamic_columns_with_sorter : sig
  type ('key, 'data) t

  val column
    :  ?sort:('key * 'data -> 'key * 'data -> int)
    -> ?sort_reversed:('key * 'data -> 'key * 'data -> int)
    -> ?initial_width:Css_gen.Length.t
    -> ?visible:bool
    -> ?resizable:bool
    -> header:(Sort_state.t -> Vdom.Node.t)
    -> cell:(key:'key -> data:'data -> Vdom.Node.t)
    -> unit
    -> ('key, 'data) t

  val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
  val expand : label:Vdom.Node.t -> ('key, 'data) t -> ('key, 'data) t

  val lift
    :  ('key, 'data) t list Bonsai.t
    -> ('key, 'data, Indexed_column_id.t) Column_intf.with_sorter

  module Sortable = Sortable
end

module Dynamic_experimental_with_sorter : sig
  val build
    :  ?sorts:
         ('column_id Bonsai.t
          -> Bonsai.graph
          -> ('key, 'data) Sort_kind.t option Bonsai.t)
    -> ('column_id, _) Bonsai.comparator
    -> columns:'column_id list Bonsai.t
    -> render_header:
         ('column_id Bonsai.t -> Bonsai.graph -> (Sort_state.t -> Vdom.Node.t) Bonsai.t)
    -> render_cell:
         ('column_id Bonsai.t
          -> 'key Bonsai.t
          -> 'data Bonsai.t
          -> Bonsai.graph
          -> Vdom.Node.t Bonsai.t)
    -> ('key, 'data, 'column_id) Column_intf.with_sorter

  module Sortable = Sortable
  module Sort_kind = Sort_kind
end
