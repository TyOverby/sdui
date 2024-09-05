open! Core
module Bonsai_proc := Bonsai_web.Proc
open! Bonsai_web
module Form := Form_automatic

module type Model = sig
  type t [@@deriving sexp_of]
end

module type Stringable_model = sig
  type t

  include Model with type t := t
  include Stringable with type t := t
end

(** For checkboxes and radio buttons, you can choose between having their visual display
    be the default native rendering, or if you want them to look like actual buttons, then
    the input element is hidden, which gives you much better control over the styling. *)
module Selectable_style = Elements_manual.Selectable_style

module Non_interactive : sig
  (** This form always contains the specified value. Setting the form has no
      effect. In addition, one must specify how the form should look to the user. *)
  val constant
    :  Vdom.Node.t Bonsai.t
    -> 'a Or_error.t Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

(** Methods in [Textbox], [Password], [Textarea], [Number], and [Range] use the theme's
    implementation of the respective methods. To customize the appearance of these
    elements within your app, you can override those methods in the theme. *)

module Textbox : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> string Form.t Bonsai.t

  val int
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> int Form.t Bonsai.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> float Form.t Bonsai.t

  val sexpable
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> (module Sexpable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> (module Stringable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

module Password : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> string Form.t Bonsai.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> (module Stringable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

module Textarea : sig
  val string
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> string Form.t Bonsai.t

  val int
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> int Form.t Bonsai.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> float Form.t Bonsai.t

  val sexpable
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> (module Sexpable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val stringable
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> (module Stringable with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

module Checkbox : sig
  val bool
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> default:bool
    -> unit
    -> Bonsai.graph
    -> bool Form.t Bonsai.t

  val set
    :  ?style:Selectable_style.t Bonsai.t
    -> ?extra_container_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_checkbox_attrs:(checked:bool -> Vdom.Attr.t list) Bonsai.t
    -> ?to_string:('a -> string)
    -> ?layout:[ `Vertical | `Horizontal ]
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> ('a, 'cmp) Set.t Form.t Bonsai.t

  module Private : sig
    val make_input
      :  id:Vdom.Attr.t
      -> extra_attrs:Vdom.Attr.t list
      -> state:bool
      -> set_state:(bool -> unit Ui_effect.t)
      -> ?key:string
      -> unit
      -> Vdom.Node.t
  end
end

module Toggle : sig
  (** Very similar to [Checkbox.bool], but with a different stylization.  Looks similar to
      the rounded variant here: https://www.w3schools.com/howto/howto_css_switch.asp *)
  val bool
    :  ?extra_attr:Vdom.Attr.t Bonsai.t
    -> default:bool
    -> unit
    -> Bonsai.graph
    -> bool Form.t Bonsai.t
end

module Dropdown : sig
  val list
    :  ?init:[ `Empty | `First_item | `This of 'a Bonsai.t ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Bonsai.t
    -> ?to_string:('a -> string)
    -> (module Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val list_opt
    :  ?init:[ `Empty | `First_item | `This of 'a Bonsai.t ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Bonsai.t
    -> ?to_string:('a -> string)
    -> (module Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> 'a option Form.t Bonsai.t

  val enumerable
    :  ?init:[ `Empty | `First_item | `This of 'a Bonsai.t ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Bonsai.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val enumerable_opt
    :  ?init:[ `Empty | `First_item | `This of 'a Bonsai.t ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_option_attrs:('a -> Vdom.Attr.t list) Bonsai.t
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> Bonsai.graph
    -> 'a option Form.t Bonsai.t

  module Private : sig
    module Opt : sig
      type 'a t =
        | Uninitialized
        | Explicitly_none
        | Set of 'a
      [@@deriving sexp, equal]

      val to_option : 'a t -> 'a option
    end

    val make_input
      :  ?to_string:('a -> string)
      -> (module Model with type t = 'a)
      -> equal:('a -> 'a -> bool)
      -> id:Vdom.Attr.t
      -> include_empty:bool
      -> default_value:'a option
      -> state:'a Opt.t
      -> set_state:('a Opt.t -> unit Ui_effect.t)
      -> extra_attrs:Vdom.Attr.t list
      -> extra_option_attrs:('a -> Vdom.Attr.t list)
      -> all:'a list
      -> Vdom.Node.t
  end
end

module Typeahead : sig
  val single
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Bonsai.t
    -> ?to_option_description:('a -> string) Bonsai.t
    -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
    -> (module Bonsai_proc.Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> all_options:'a list Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val single_opt
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Bonsai.t
    -> ?to_option_description:('a -> string) Bonsai.t
    -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
    -> (module Bonsai_proc.Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> all_options:'a list Bonsai.t
    -> Bonsai.graph
    -> 'a option Form.t Bonsai.t

  val set
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Bonsai.t
    -> ?to_option_description:('a -> string) Bonsai.t
    -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
    -> ?split:(string -> string list)
    -> ('a, 'cmp) Bonsai.comparator
    -> all_options:'a list Bonsai.t
    -> Bonsai.graph
    -> ('a, 'cmp) Set.t Form.t Bonsai.t

  val list
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?placeholder:string
    -> ?to_string:('a -> string) Bonsai.t
    -> ?to_option_description:('a -> string) Bonsai.t
    -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
    -> ?split:(string -> string list)
    -> ('a, _) Bonsai.comparator
    -> all_options:'a list Bonsai.t
    -> Bonsai.graph
    -> 'a list Form.t Bonsai.t
end

module Date_time : sig
  module Span_unit : sig
    type t =
      | Milliseconds
      | Seconds
      | Minutes
      | Hours
  end

  val date
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?default:Date.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Date.t Form.t Bonsai.t

  val date_opt
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?default:Date.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Date.t option Form.t Bonsai.t

  val time
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Time_ns.Ofday.t Form.t Bonsai.t

  val time_opt
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Time_ns.Ofday.t option Form.t Bonsai.t

  val time_span
    :  ?extra_unit_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_amount_attrs:Vdom.Attr.t list Bonsai.t
    -> ?default_unit:Span_unit.t
    -> ?default:Time_ns.Span.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Time_ns.Span.t Form.t Bonsai.t

  val time_span_opt
    :  ?extra_unit_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_amount_attrs:Vdom.Attr.t list Bonsai.t
    -> ?default_unit:Span_unit.t
    -> ?default:Time_ns.Span.t option
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Time_ns.Span.t option Form.t Bonsai.t

  val datetime_local
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Time_ns.t Form.t Bonsai.t

  val datetime_local_opt
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> Time_ns.t option Form.t Bonsai.t

  module Range : sig
    val date
      :  ?extra_attr:Vdom.Attr.t Bonsai.t
      -> ?allow_equal:bool
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> Bonsai.graph
      -> (Date.t * Date.t) Form.t Bonsai.t

    val date_opt
      :  ?extra_attr:Vdom.Attr.t Bonsai.t
      -> ?allow_equal:bool
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> Bonsai.graph
      -> (Date.t option * Date.t option) Form.t Bonsai.t

    val time
      :  ?extra_attr:Vdom.Attr.t Bonsai.t
      -> ?allow_equal:bool
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> Bonsai.graph
      -> (Time_ns.Ofday.t * Time_ns.Ofday.t) Form.t Bonsai.t

    val time_opt
      :  ?extra_attr:Vdom.Attr.t Bonsai.t
      -> ?allow_equal:bool
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> Bonsai.graph
      -> (Time_ns.Ofday.t option * Time_ns.Ofday.t option) Form.t Bonsai.t

    val datetime_local
      :  ?extra_attr:Vdom.Attr.t Bonsai.t
      -> ?allow_equal:bool
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> Bonsai.graph
      -> (Time_ns.t * Time_ns.t) Form.t Bonsai.t

    val datetime_local_opt
      :  ?extra_attr:Vdom.Attr.t Bonsai.t
      -> ?allow_equal:bool
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> unit
      -> Bonsai.graph
      -> (Time_ns.t option * Time_ns.t option) Form.t Bonsai.t
  end
end

module Multiselect : sig
  val set
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?to_string:('a -> string)
    -> ?default_selection_status:Bonsai_web_ui_multi_select.Selection_status.t Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> ('a, 'cmp) Bonsai.comparator
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> ('a, 'cmp) Set.t Form.t Bonsai.t

  val list
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?to_string:('a -> string)
    -> ?default_selection_status:Bonsai_web_ui_multi_select.Selection_status.t Bonsai.t
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> ('a, _) Bonsai.comparator
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> 'a list Form.t Bonsai.t
end

module Number : sig
  val int
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?min:int
    -> ?max:int
    -> ?default:int
    -> step:int
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> int Form.t Bonsai.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?min:float
    -> ?max:float
    -> ?default:float
    -> step:float
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> float Form.t Bonsai.t
end

module Range : sig
  val int
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?min:int
    -> ?max:int
    -> ?left_label:Vdom.Node.t
    -> ?right_label:Vdom.Node.t
    -> ?default:int
    -> step:int
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> int Form.t Bonsai.t

  val float
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?min:float
    -> ?max:float
    -> ?left_label:Vdom.Node.t
    -> ?right_label:Vdom.Node.t
    -> ?default:float
    -> step:float
    -> ?allow_updates_when_focused:[ `Always | `Never ]
    -> unit
    -> Bonsai.graph
    -> float Form.t Bonsai.t
end

module Radio_buttons : sig
  val list
    :  ?style:Selectable_style.t Bonsai.t
    -> ?extra_container_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_button_attrs:(checked:bool -> Vdom.Attr.t list) Bonsai.t
    -> ?init:'a
    -> ?to_string:('a -> string)
    -> (module Model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> layout:[ `Vertical | `Horizontal ]
    -> 'a list Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val enumerable
    :  ?style:Selectable_style.t Bonsai.t
    -> ?extra_container_attrs:Vdom.Attr.t list Bonsai.t
    -> ?extra_button_attrs:(checked:bool -> Vdom.Attr.t list) Bonsai.t
    -> ?init:'a
    -> ?to_string:('a -> string)
    -> (module Bonsai.Enum with type t = 'a)
    -> layout:[ `Vertical | `Horizontal ]
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t
end

module Color_picker : sig
  val hex
    :  ?extra_attr:Vdom.Attr.t Bonsai.t
    -> unit
    -> Bonsai.graph
    -> [ `Hex of string ] Form.t Bonsai.t
end

module Multiple : sig
  (* [stringable_list] creates a form with a single textbox which calls [of_string] on the
     contents of the textbox and adds it to the list, whenever enter is pressed. This is
     preferred to [list] when the string representation of a type is easy to write. *)
  val stringable_list
    :  ?extra_input_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_pill_container_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_pill_attr:Vdom.Attr.t Bonsai.t
    -> ?placeholder:string Bonsai.t
    -> (module Stringable_model with type t = 'a)
    -> equal:('a -> 'a -> bool)
    -> Bonsai.graph
    -> 'a list Form.t Bonsai.t

  val list
    :  ?element_group_label:(delete_button:Vdom.Node.t -> int -> Vdom.Node.t)
    -> ?add_element_text:string Bonsai.t
    -> ?button_placement:[ `Indented | `Inline ]
    -> (Bonsai.graph -> 'a Form.t Bonsai.t)
    -> Bonsai.graph
    -> 'a list Form.t Bonsai.t

  val nonempty_list
    :  ?element_group_label:(delete_button:Vdom.Node.t -> int -> Vdom.Node.t)
    -> ?add_element_text:string Bonsai.t
    -> ?button_placement:[ `Indented | `Inline ]
    -> (Bonsai.graph -> 'a Form.t Bonsai.t)
    -> Bonsai.graph
    -> 'a Nonempty_list.t Form.t Bonsai.t

  val set
    :  ?element_group_label:(delete_button:Vdom.Node.t -> int -> Vdom.Node.t)
    -> ?add_element_text:string Bonsai.t
    -> ?button_placement:[ `Indented | `Inline ]
    -> ('a, 'cmp) Bonsai.comparator
    -> (Bonsai.graph -> 'a Form.t Bonsai.t)
    -> Bonsai.graph
    -> ('a, 'cmp) Set.t Form.t Bonsai.t

  val map
    :  ?element_group_label:(delete_button:Vdom.Node.t -> int -> Vdom.Node.t)
    -> ?add_element_text:string Bonsai.t
    -> ?button_placement:[ `Indented | `Inline ]
    -> ('k, 'cmp) Bonsai.comparator
    -> key:(Bonsai.graph -> 'k Form.t Bonsai.t)
    -> data:(Bonsai.graph -> 'v Form.t Bonsai.t)
    -> Bonsai.graph
    -> ('k, 'v, 'cmp) Map.t Form.t Bonsai.t
end

module File_select : sig
  (** A form element that allows the user to select a file from their local disk.

      NOTE: these widgets are not safe for use in Tangle as internally they require a
      model which cannot be [of_sexp]'d.

      NOTE: These elements have their [set] behaviour restricted by the browser. For
      security reasons, these inputs can only be cleared. Attempting to set a value other
      than empty (i.e. [None] for [single_opt], or [Filename.Map.empty] for [list]) will
      result in a warning message and be ignored. *)

  val single_opt
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> Bonsai.graph
    -> Bonsai_web_ui_file.t option Form.t Bonsai.t

  (** A form where picking a file is mandatory. The form will be in an error state until a
      file is picked.

      Per the second note above, this form cannot be set. *)
  val single
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> Bonsai.graph
    -> Bonsai_web_ui_file.t Form.t Bonsai.t

  val multiple
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?accept:[ `Extension of string | `Mimetype of string ] list
    -> unit
    -> Bonsai.graph
    -> Bonsai_web_ui_file.t Filename.Map.t Form.t Bonsai.t
end

module Freeform_multiselect : sig
  val list
    :  ?extra_attr:Vdom.Attr.t Bonsai.t
    -> ?placeholder:string
    -> ?split:(string -> string list)
    -> unit
    -> Bonsai.graph
    -> string list Form.t Bonsai.t

  val set
    :  ?extra_attr:Vdom.Attr.t Bonsai.t
    -> ?placeholder:string
    -> ?split:(string -> string list)
    -> unit
    -> Bonsai.graph
    -> String.Set.t Form.t Bonsai.t
end

module Rank : sig
  val list
    :  ('a, 'b) Bonsai_web_ui_reorderable_list.comparator
    -> ?enable_debug_overlay:bool
    -> ?extra_item_attrs:Vdom.Attr.t Bonsai.t
    -> ?left:Css_gen.Length.t
    -> ?right:Css_gen.Length.t
    -> ?empty_list_placeholder:
         (item_is_hovered:bool Bonsai.t -> Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> ?default_item_height:int
    -> (source:Vdom.Attr.t Bonsai.t
        -> 'a Bonsai.t
        -> Bonsai.graph
        -> Vdom.Node.t Bonsai.t)
    -> Bonsai.graph
    -> 'a list Form.t Bonsai.t
end

module Query_box : sig
  val create_opt
    :  (module Bonsai.Comparator with type comparator_witness = 'cmp and type t = 'k)
    -> ?initial_query:string
    -> ?max_visible_items:int Bonsai.t
    -> ?suggestion_list_kind:Bonsai_web_ui_query_box.Suggestion_list_kind.t Bonsai.t
    -> ?selected_item_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_list_container_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_input_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_attr:Vdom.Attr.t Bonsai.t
    -> selection_to_string:('k -> string) Bonsai.t
    -> f:(string Bonsai.t -> Bonsai.graph -> ('k, Vdom.Node.t, 'cmp) Map.t Bonsai.t)
    -> unit
    -> Bonsai.graph
    -> 'k option Form.t Bonsai.t

  val create
    :  (module Bonsai.Comparator with type comparator_witness = 'cmp and type t = 'k)
    -> ?initial_query:string
    -> ?max_visible_items:int Bonsai.t
    -> ?suggestion_list_kind:Bonsai_web_ui_query_box.Suggestion_list_kind.t Bonsai.t
    -> ?selected_item_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_list_container_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_input_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_attr:Vdom.Attr.t Bonsai.t
    -> selection_to_string:('k -> string) Bonsai.t
    -> f:(string Bonsai.t -> Bonsai.graph -> ('k, Vdom.Node.t, 'cmp) Map.t Bonsai.t)
    -> unit
    -> Bonsai.graph
    -> 'k Form.t Bonsai.t

  val single
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?to_string:('a -> string) Bonsai.t
    -> ?to_option_description:('a -> string) Bonsai.t
    -> ?selected_item_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_list_container_attr:Vdom.Attr.t Bonsai.t
    -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
    -> (module Bonsai.Comparator with type t = 'a and type comparator_witness = 'cmp)
       (* If there are duplicate items in [all_options] (according to the comparator),
       the last of the duplicates will be the only one that show up in the list
       of suggestions. *)
    -> all_options:'a list Bonsai.t
    -> Bonsai.graph
    -> 'a Form.t Bonsai.t

  val single_opt
    :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?to_string:('a -> string) Bonsai.t
    -> ?to_option_description:('a -> string) Bonsai.t
    -> ?selected_item_attr:Vdom.Attr.t Bonsai.t
    -> ?extra_list_container_attr:Vdom.Attr.t Bonsai.t
    -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
    -> (module Bonsai.Comparator with type t = 'a and type comparator_witness = 'cmp)
       (* If there are duplicate items in [all_options] (according to the comparator),
       the last of the duplicates will be the only one that show up in the list
       of suggestions. *)
    -> all_options:'a list Bonsai.t
    -> Bonsai.graph
    -> 'a option Form.t Bonsai.t
end

module Optional : sig
  (* [dropdown] takes an existing form, and adds a dropdown with [some_label] and
     [none_label] options. If the user selects the [some_label] option, the provided form
     is used for the inner value of the option. *)
  val dropdown
    :  ?some_label:string (** default ["Some"] *)
    -> ?none_label:string (** default ["None"] *)
    -> (Bonsai.graph -> 'a Form.t Bonsai.t)
       (** shown when the [some_label] option is selected *)
    -> Bonsai.graph
    -> 'a option Form.t Bonsai.t
end
