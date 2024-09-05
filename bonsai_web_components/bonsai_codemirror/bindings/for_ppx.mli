open! Js_of_ocaml
open! Gen_js_api
open Custom_ojs_converter

(** This set of bindings is maintained manually and is filled out as necessary.
    Rather than inspecting this API to see if something is possible with
    codemirror, you should instead look at the reference documentation at

    https://codemirror.net/6/docs/ref/

    The bindings here are undoubtedly missing functions and classes, as well as
    parameters from some functions and classes. However, the invariant that
    this API ought to uphold is that the subset which it *does* bind is bound
    correctly.
*)

module Promise : sig
  type ('a, 'e) t

  val create : (('a -> unit) -> ('e -> unit) -> unit) -> ('a, 'e) t [@@js.new "Promise"]
end

module RegExp : sig
  [@@@js.scope "RegExp"]

  type t

  val create : string -> string option -> t [@@js.create]
end

module Text : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "State"]

  module Line : sig
    type t

    val from : t -> int [@@js.get]
    val to_ : t -> int [@@js.get "to"]
    val number : t -> int [@@js.get]
    val text : t -> string [@@js.get]
    val length : t -> int [@@js.get]
  end

  module Text_iterator : sig
    type t

    val next : t -> ?skip:int -> unit -> t [@@js.call]
    val value : t -> string [@@js.get]
    val done_ : t -> bool [@@js.get "done"]
    val line_break : t -> bool [@@js.get]
  end

  module Text : sig
    type t

    val length : t -> int [@@js.get]
    val lines : t -> int [@@js.get]
    val line_at : t -> int -> Line.t [@@js.call]
    val line : t -> int -> Line.t [@@js.call]
    val replace : t -> from:int -> to_:int -> with_:t -> t [@@js.call]
    val append : t -> t -> t [@@js.call]
    val slice : t -> from:int -> ?to_:int -> unit -> t [@@js.call]

    val slice_string : t -> from:int -> ?to_:int -> ?line_sep:string -> unit -> string
    [@@js.call]

    val equal : t -> t -> bool [@@js.call "eq"]
    val iter : t -> ?dir:int -> unit -> Text_iterator.t [@@js.call]
    val iter_range : t -> from:int -> ?to_:int -> unit -> Text_iterator.t [@@js.call]
    val to_json : t -> string list [@@js.call "toJSON"]
    val of_ : string list -> t [@@js.global "of"]
    val of_array : string array -> t [@@js.global "of"]
    val empty : t [@@js.global]
  end
  [@@js.scope "Text"]

  val count_column : string -> n:int -> tab_size:int -> int [@@js.call]

  module Offset_and_left_over : sig
    type t =
      { offset : int
      ; left_over : int
      }
  end

  val find_column : string -> n:int -> col:int -> tab_size:int -> Offset_and_left_over.t
  [@@js.call]

  val code_point_at : string -> int -> int [@@js.call]
  val from_code_point : int -> string [@@js.call]
  val code_point_size : int -> int [@@js.call]
  val find_cluster_break : string -> int -> ?forward:bool -> unit -> int [@@js.call]
end

module State : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "State"]

  module Extension : sig
    type t

    val of_list : t list -> t [@@js.cast]
  end

  module Editor_state_config : sig
    type t

    val create : ?doc:string -> ?extensions:Extension.t list -> unit -> t [@@js.builder]
  end

  type state_effect

  module State_effect_type : sig
    type 'a t

    val of_ : 'a t -> 'a -> state_effect [@@js.call "of"]
  end

  module State_effect : sig
    type t = state_effect

    val reconfigure : Extension.t State_effect_type.t [@@js.global]
  end
  [@@js.scope "StateEffect"]

  module Annotation : sig
    type t
  end

  module Effects_and_annotation : sig
    type t

    val create
      :  ?effects:State_effect.t list
      -> ?annotations:Annotation.t list
      -> unit
      -> t
    [@@js.builder]
  end

  type change_set

  module Change_spec : sig
    type t

    val single : ?from:int -> ?to_:(int[@js "to"]) -> ?insert:Text.Text.t -> unit -> t
    [@@js.builder]

    val change_set : change_set -> t [@@js.cast]
    val many : t list -> t [@@js.cast]
  end

  module Map_mode : sig
    type t =
      | Simple
      | TrackDel
      | TrackBefore
      | TrackAfter
    [@@js.enum]
  end

  module Cursor_association : sig
    type t =
      | Left [@js -1]
      | None [@js 0]
      | Right [@js 1]
    [@@js.enum]
  end

  module Touches_range : sig
    type t =
      | True [@js true]
      | False [@js false]
      | Cover [@js "cover"]
    [@@js.enum]
  end

  module Change_desc : sig
    type t

    val length : t -> int [@@js.get]
    val new_length : t -> int [@@js.get]
    val empty : t -> bool [@@js.get]
    val iter_gaps : t -> f:(a:int -> b:int -> length:int -> unit) -> unit [@@js.call]

    val iter_changed_ranges
      :  t
      -> f:(from_a:int -> to_a:int -> from_b:int -> to_b:int -> unit)
      -> unit
    [@@js.call]

    val inverted_desc : t -> t [@@js.get]
    val compose_desc : t -> t -> t [@@js.call]
    val map_desc : t -> t -> ?before:bool -> unit -> t [@@js.call]

    val map_pos : t -> int -> assoc:Cursor_association.t -> mode:Map_mode.t -> t
    [@@js.call]

    val touches_range : t -> from:int -> ?to_:int -> unit -> Touches_range.t [@@js.call]
  end

  module Selection_range : sig
    type t

    val from : t -> int [@@js.get]
    val to_ : t -> int [@@js.get "to"]
    val anchor : t -> int [@@js.get]
    val head : t -> int [@@js.get]
    val empty : t -> bool [@@js.get]
    val assoc : t -> Cursor_association.t [@@js.get]
    val bidi_level : t -> int option [@@js.get]
    val goal_column : t -> int option [@@js.get]
    val map : t -> Change_desc.t -> ?assoc:Cursor_association.t -> unit -> t [@@js.call]
    val extend : t -> from:int -> ?to_:int -> unit -> t [@@js.call]
    val equal : t -> t -> bool [@@js.call "eq"]
    val to_json : t -> Ojs.t [@@js.call "toJSON"]
    val from_json : Ojs.t -> t [@@js.global "fromJSON"]
  end
  [@@js.scope "SelectionRange"]

  module Editor_selection : sig
    type t

    val ranges : t -> Selection_range.t list [@@js.get]
    val main_index : t -> int [@@js.get]
    val main : t -> Selection_range.t [@@js.get]
    val map : t -> Change_desc.t -> ?assoc:Cursor_association.t -> unit -> t [@@js.call]
    val equal : t -> t -> bool [@@js.call "eq"]
    val as_single : t -> t [@@js.call]
    val add_range : t -> Selection_range.t -> ?main:bool -> unit -> t [@@js.call]
    val replace_range : Selection_range.t -> ?which:int -> unit -> t [@@js.call]
    val to_json : t -> Ojs.t [@@js.call "toJSON"]
    val from_json : Ojs.t -> t [@@js.global "fromJSON"]
    val single : anchor:int -> ?head:int -> unit -> t [@@js.global]

    val create : ranges:Selection_range.t list -> ?main_index:int -> unit -> t
    [@@js.global]

    val cursor
      :  pos:int
      -> ?assoc:Cursor_association.t
      -> ?bidi_level:int
      -> ?goal_column:int
      -> unit
      -> Selection_range.t
    [@@js.global]

    val range : anchor:int -> head:int -> ?goal_column:int -> unit -> Selection_range.t
    [@@js.global]
  end
  [@@js.scope "EditorSelection"]

  module Change_set : sig
    type t = change_set

    val iter_changes
      :  t
      -> f:
           (from_a:int
            -> to_a:int
            -> from_b:int
            -> to_b:int
            -> inserted:Text.Text.t
            -> unit)
      -> unit
    [@@js.call]

    val desc : t -> Change_desc.t [@@js.get]
  end

  type editor_state

  module Annotation_type : sig
    type 'a t
  end

  module Transaction : sig
    type t

    val start_state : t -> editor_state [@@js.get]
    val changes : t -> Change_set.t [@@js.get]
    val selection : t -> Editor_selection.t option [@@js.get]
    val effects : t -> State_effect.t list [@@js.get]
    val scroll_into_view : t -> bool [@@js.get]
    val new_doc : t -> Text.Text.t [@@js.get]
    val new_selection : t -> Editor_selection.t [@@js.get]
    val state : t -> editor_state [@@js.get]
    val annotation : t -> 'a Annotation_type.t -> 'a option [@@js.call]
    val doc_changed : t -> bool [@@js.get]
    val reconfigured : t -> bool [@@js.get]
    val time : int Annotation_type.t [@@js.global]
    val user_event : string Annotation_type.t [@@js.global]
    val add_to_history : bool Annotation_type.t [@@js.global]
  end
  [@@js.scope "Transaction"]

  module State_field_config : sig
    type 'v t

    val create
      :  create:(editor_state -> 'v)
      -> update:('v -> Transaction.t -> 'v)
      -> compare:('v -> 'v -> bool) option
      -> 'v t
    [@@js.builder]
  end

  module State_field : sig
    type 'v t

    val extension : 'v t -> Extension.t [@@js.get]
    val define : config:'v State_field_config.t -> 'v t [@@js.global]
  end
  [@@js.scope "StateField"]

  module Facet : sig
    type ('i, 'o) t
    type 'i multi_out = ('i, 'i list) t

    val of_ : ('i, 'o) t -> 'i With_conversion.t -> Extension.t [@@js.call "of"]

    module Dep : sig
      type ('i, 'o) facet = ('i, 'o) t

      type t =
        | Doc : t [@js "doc"]
        | Selection : t [@js "selection"]
          (* | Facet : ('i, 'o) facet -> t [@js.default] *)
      [@@js.enum]
    end

    val compute : ('i, 'o) t -> deps:Dep.t list -> get:(editor_state -> 'i) -> Extension.t
    [@@js.call "compute"]

    val from : ('i, 'o) t -> field:'t State_field.t -> get:(value:'t -> 'i) -> Extension.t
    [@@js.call]

    module Config : sig
      type ('i, 'o) facet = ('i, 'o) t
      type ('i, 'o) t

      (* [combine] isn't technically required in the codemirror api but it's not type safe
         if left off. *)
      val create
        :  combine:('i list -> 'o)
        -> ?compare:('o -> 'o -> bool)
        -> ?compareInput:('i -> 'i -> bool)
        -> ?static:bool
        -> ?enables:(('i, 'o) facet -> Extension.t)
        -> unit
        -> ('i, 'o) t
      [@@js.builder]
    end

    val define : ('i, 'o) Config.t -> ('i, 'o) t [@@js.global]
  end
  [@@js.scope "Facet"]

  module Char_category : sig
    type t =
      | Word
      | Space
      | Other
    [@@js.enum]
  end

  module Change_filter_result : sig
    type t

    val bool : bool -> t [@@js.cast]
    val ranges : int list -> t [@@js.cast]
  end

  module Transaction_spec : sig
    type t

    val create
      :  ?changes:Change_spec.t
      -> ?selection:Editor_selection.t
      -> ?effects:State_effect.t list
      -> ?annotations:Annotation.t list
      -> ?scroll_into_view:bool
      -> ?filter:bool
      -> ?sequential:bool
      -> unit
      -> t
    [@@js.builder]
  end

  module Compartment : sig
    type t

    val create : unit -> t [@@js.new "Compartment"]
    val of_ : t -> Extension.t -> Extension.t [@@js.call "of"]
    val reconfigure : t -> Extension.t -> state_effect [@@js.call]
    val get : t -> editor_state -> Extension.t option [@@js.call]
  end

  module Editor_state : sig
    type t = editor_state

    val doc : t -> Text.Text.t [@@js.get]
    val selection : t -> Editor_selection.t [@@js.get]
    val field : t -> 'a State_field.t -> ?require:bool -> unit -> 'a [@@js.call]
    val update : t -> (Transaction_spec.t list[@js.variadic]) -> Transaction.t [@@js.call]

    (* Codemirror also accepts a string instead of a Text.t, but that seems
       unnecessary to duplicate in the ocaml API *)
    val replace_selection : t -> Text.Text.t -> Transaction_spec.t [@@js.call]

    val change_by_range
      :  t
      -> f:(Selection_range.t -> Transaction_spec.t)
      -> Transaction_spec.t
    [@@js.call]

    val changes : t -> Change_spec.t -> Change_set.t [@@js.call]
    val to_text : t -> string -> Text.Text.t [@@js.call]
    val slice_doc : t -> ?from:int -> ?to_:int -> unit -> string [@@js.call]
    val facet : t -> ('i, 'o) Facet.t -> 'o [@@js.call]
    val to_json : t -> Ojs.t [@@js.call "toJSON"]
    val get_tab_size : t -> int [@@js.get "tabSize"]
    val line_break : t -> string [@@js.get]
    val phrase : t -> string -> string [@@js.call]
    val language_data_at : t -> name:string -> pos:int -> Ojs.t list [@@js.call]
    val char_categorizer : t -> int -> string -> Char_category.t [@@js.call]

    val from_json
      :  json:Ojs.t
      -> ?config:Editor_state_config.t
      -> ?fields:Ojs.t
      -> unit
      -> t
    [@@js.global "fromJSON"]

    val create : Editor_state_config.t -> t [@@js.global]
    val allow_multiple_selections : (bool, bool) Facet.t [@@js.global]
    val tab_size : (int, int) Facet.t [@@js.global]
    val read_only : (bool, bool) Facet.t [@@js.global]
    val line_separator : (string, string option) Facet.t [@@js.global]
    val phrases : Ojs.t Facet.multi_out [@@js.global]
    val language_data : (t -> int -> Ojs.t list) Facet.multi_out [@@js.global]

    val change_filter : (Transaction.t -> Change_filter_result.t) Facet.multi_out
    [@@js.global]

    val transaction_filter : (Transaction.t -> Transaction_spec.t list) Facet.multi_out
    [@@js.global]

    val transaction_extender : (Transaction.t -> Effects_and_annotation.t) Facet.multi_out
    [@@js.global]
  end
  [@@js.scope "EditorState"]

  module Range : sig
    type 'v t

    val from : 'v t -> int [@@js.get]
    val to_ : 'v t -> int [@@js.get]
  end

  module Range_set_update_spec : sig
    type 'v t

    val create
      :  add:'v Range.t list
      -> sort:bool
      -> filter:(from:int -> to_:int -> value:'v -> bool) option
      -> filter_from:int option
      -> filter_to:int option
      -> 'v t
    [@@js.builder]
  end

  module Range_set : sig
    type 'v t

    val empty : 'v t [@@js.global]
    val size : 'v t -> int [@@js.get]
    val update : 'v t -> update_spec:'v Range_set_update_spec.t -> 'v t [@@js.call]

    val between
      :  'v t
      -> from:int
      -> to_:int
      -> f:(from:int -> to_:int -> value:'v -> bool option)
      -> unit
    [@@js.call]
  end
  [@@js.scope "RangeSet"]

  module Prec : sig
    [@@@js.scope "Prec"]

    val fallback : Extension.t -> Extension.t [@@js.global]
    val default : Extension.t -> Extension.t [@@js.global]
    val extend : Extension.t -> Extension.t [@@js.global]
    val override : Extension.t -> Extension.t [@@js.global]
    val high : Extension.t -> Extension.t [@@js.global]
  end
end

module View : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "View"]

  type editor_view

  module Config : sig
    type t

    val create
      :  ?state:State.Editor_state.t
      -> ?dispatch:(State.Transaction.t -> editor_view -> unit) Callback.t
      -> ?scroll_to:State.State_effect.t
      -> unit
      -> t
    [@@js.builder]
  end

  module View_update : sig
    type t

    val changes : t -> State.Change_set.t [@@js.get]
    val state : t -> State.Editor_state.t [@@js.get]
    val view : t -> editor_view [@@js.get]
    val viewport_changed : t -> bool [@@js.get]
    val doc_changed : t -> bool [@@js.get]
    val t_of_js : Ojs.t -> t
    val t_to_js : t -> Ojs.t
  end

  val highlight_special_chars : unit -> State.Extension.t [@@js.global]
  val highlight_active_line : unit -> State.Extension.t [@@js.global]
  val placeholder : content:string -> State.Extension.t [@@js.global]

  module Draw_selection_config : sig
    type t

    val create : ?cursor_blink_rate:float -> ?draw_range_cursor:bool -> unit -> t
    [@@js.builder]
  end

  val draw_selection : ?config:Draw_selection_config.t -> unit -> State.Extension.t
  [@@js.global]

  module Widget_type : sig
    type t

    val create : unit -> t [@@js.new "WidgetType"]
    val set_to_dom : t -> (unit -> Ojs.t) -> unit [@@js.set "toDOM"]
  end

  module Decoration : sig
    type t

    val range : t -> from:int -> to_:int -> t State.Range.t [@@js.call]
    val set : t State.Range.t list -> sort:bool -> t State.Range_set.t [@@js.global]
    val none : t State.Range_set.t [@@js.global]

    module Mark_spec : sig
      type t

      val create
        :  ?inclusive:bool
        -> ?inclusive_start:bool
        -> ?inclusive_end:bool
        -> ?attributes:Ojs.t
        -> ?class_:string
        -> ?tag_name:string
        -> unit
        -> t
      [@@js.builder]
    end

    val mark : Mark_spec.t -> t [@@js.global]

    module Widget_spec : sig
      type t

      val create
        :  widget:Widget_type.t
        -> ?side:int
        -> ?inline_order:bool
        -> ?block:bool
        -> unit
        -> t
      [@@js.builder]
    end

    val widget : Widget_spec.t -> t [@@js.global]

    module Replace_spec : sig
      type t

      val create
        :  ?widget:Widget_type.t
        -> ?inclusive:bool
        -> ?inclusive_start:bool
        -> ?inclusive_end:bool
        -> ?block:bool
        -> unit
        -> t
      [@@js.builder]
    end

    val replace : Replace_spec.t -> t [@@js.global]

    module Line_spec : sig
      type t

      val create : ?attributes:Ojs.t -> ?class_:string -> unit -> t [@@js.builder]
    end

    val line : Line_spec.t -> t [@@js.global]
  end
  [@@js.scope "Decoration"]

  module Decoration_set : sig
    type t = Decoration.t State.Range_set.t
  end

  module Editor_view : sig
    type t = editor_view

    val create : Config.t -> t [@@js.create]
    val dom : t -> Dom_html_element.t [@@js.get]
    val content_dom : t -> Dom_html_element.t [@@js.get "contentDOM"]
    val update_listener : (View_update.t -> unit) State.Facet.multi_out [@@js.global]
    val update : t -> State.Transaction.t list -> unit [@@js.call]
    val dispatch : t -> State.Transaction.t -> unit [@@js.call]
    val set_state : t -> State.Editor_state.t -> unit [@@js.call]
    val state : t -> State.Editor_state.t [@@js.get]
    val focus : t -> unit [@@js.call]
    val destroy : t -> unit [@@js.call]
    val line_wrapping : State.Extension.t [@@js.global]
    val editable : (bool, bool) State.Facet.t [@@js.global]
    val dark_theme : (bool, bool) State.Facet.t [@@js.global]
    val decorations : (Decoration_set.t, Decoration_set.t) State.Facet.t [@@js.global]

    val decorations'
      : ( (editor_view -> Decoration_set.t) Callback.t
          , (editor_view -> Decoration_set.t) Callback.t )
          State.Facet.t
    [@@js.global "decorations"]

    module Viewport : sig
      type t

      val from : t -> int [@@js.get]
      val to_ : t -> int [@@js.get]
    end

    val viewport : t -> Viewport.t [@@js.get]

    module Scroll_into_view_options : sig
      type t

      val create : ?y:string -> unit -> t [@@js.builder]
    end

    val scroll_into_view
      :  pos:int
      -> ?options:Scroll_into_view_options.t
      -> unit
      -> State.State_effect.t
    [@@js.global]

    module Theme_options : sig
      type t

      val create : ?dark:bool -> unit -> t [@@js.builder]
    end

    val theme : spec:Ojs.t -> ?options:Theme_options.t -> unit -> State.Extension.t
    [@@js.global]
  end
  [@@js.scope "EditorView"]

  module Plugin_value : sig
    (* [custom_state] isn't a field that appears in the official CodeMirror API; it's
       just here to provide an interface for any extra data a user would want to store
       in a plugin value. This shouldn't cause any issues, since PluginValue
       is just an interface and not a class/it's meant to potentially have extra fields
       (e.g. as in their example - https://codemirror.net/examples/zebra/#:~:text=decorations%3A%20DecorationSet). *)
    type 'v t =
      { update : (View_update.t -> unit) option
      ; custom_state : 'v
      }
  end

  module Plugin_spec : sig
    type 'v t

    val create : decorations:('v Plugin_value.t -> Decoration_set.t) option -> 'v t
    [@@js.builder]
  end

  module View_plugin : sig
    type 'v t

    val extension : 'v t -> State.Extension.t [@@js.get]

    val define
      :  create:(Editor_view.t -> 'v Plugin_value.t)
      -> ?spec:'v Plugin_spec.t
      -> unit
      -> 'v t
    [@@js.global]
  end
  [@@js.scope "ViewPlugin"]

  module Match_decorator : sig
    module Config : sig
      type t

      val create
        :  regexp:RegExp.t
        -> ?decoration:Decoration.t
        -> ?decorate:
             (add:(from:int -> to_:int -> Decoration.t -> unit)
              -> from:int
              -> to_:int
              -> string option list
              -> Editor_view.t
              -> unit)
        -> ?max_length:int
        -> unit
        -> t
      [@@js.builder]
    end

    type t

    val create : Config.t -> t [@@js.new "MatchDecorator"]
    val create_deco : t -> Editor_view.t -> Decoration_set.t [@@js.call]
  end

  module Command : sig
    type t = Editor_view.t -> bool
  end

  module Key_binding : sig
    type t

    val create
      :  ?key:string
      -> ?mac:string
      -> ?win:string
      -> ?linux:string
      -> run:Command.t
      -> ?shift:Command.t
      -> ?scope:string
      -> ?prevent_default:bool
      -> unit
      -> t
    [@@js.builder]

    val t_of_js : Ojs.t -> t
    val t_to_js : t -> Ojs.t
  end

  val keymap : Key_binding.t list State.Facet.multi_out [@@js.global]
end

module Autocomplete : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Autocomplete"]

  module Completion : sig
    type t

    val create
      :  label:string
      -> ?detail:string
      -> ?info:([ `Str of string | `Dom of t -> Dom_html_element.t ][@js.union])
      -> ?type_:string
      -> ?boost:int
      -> ?apply:string
      -> unit
      -> t
    [@@js.builder]

    val label : t -> string [@@js.get]
    val type_ : t -> string option [@@js.get "type"]
  end

  module CompletionContext : sig
    type t

    val state : t -> State.Editor_state.t [@@js.get]
    val pos : t -> int [@@js.get]
    val explicit : t -> bool [@@js.get]
  end

  module CompletionResult : sig
    type t

    val create
      :  from:int
      -> ?to_:int
      -> options:Completion.t list
      -> ?valid_for:
           ([ `Regex of RegExp.t
            | `Fn of string -> from:int -> to_:int -> State.Editor_state.t -> bool
            ]
           [@js.union])
      -> ?filter:bool
      -> unit
      -> t
    [@@js.builder]
  end

  module CompletionSource : sig
    type t

    val of_sync_fun : (CompletionContext.t -> CompletionResult.t) -> t [@@js.cast]

    (* Note: technically the CompletionResult.t within the Promise should also be an option
       since the promise can return null. It caused something to break when I tried it,
       though
    *)
    val of_promise_fun
      :  (CompletionContext.t -> (CompletionResult.t, 'e) Promise.t option)
      -> t
    [@@js.cast]
  end

  module Config : sig
    type t

    module Add_to_options_parameters : sig
      type t =
        { render : Completion.t -> State.Editor_state.t -> Dom_html_element.t option
        ; position : int
        }
    end

    val create
      :  ?activate_on_typing:bool
      -> ?override:CompletionSource.t list
      -> ?max_rendered_options:int
      -> ?default_keymap:bool
      -> ?icons:bool
      -> ?add_to_options:Add_to_options_parameters.t list
      -> unit
      -> t
    [@@js.builder]
  end

  module Move_completion_selection_by : sig
    type t =
      | Option [@js "option"]
      | Page [@js "page"]
    [@@js.enum]
  end

  val autocompletion : Config.t -> State.Extension.t [@@js.global]
  val completion_status : State.Editor_state.t -> string option [@@js.global]
  val start_completion : View.Command.t [@@js.global]
  val close_completion : View.Command.t [@@js.global]
  val accept_completion : View.Command.t [@@js.global]

  val move_completion_selection
    :  forward:bool
    -> ?by:Move_completion_selection_by.t
    -> unit
    -> View.Command.t
  [@@js.global]

  val completion_keymap : View.Key_binding.t list [@@js.global]
end

module Commands : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Commands"]

  val standard_keymap : View.Key_binding.t list [@@js.global]
  val default_keymap : View.Key_binding.t list [@@js.global]
  val emacs_style_keymap : View.Key_binding.t list [@@js.global]
  val cursor_char_left : View.Command.t [@@js.global]
  val select_char_left : View.Command.t [@@js.global]
  val cursor_group_left : View.Command.t [@@js.global]
  val select_group_left : View.Command.t [@@js.global]
  val cursor_char_right : View.Command.t [@@js.global]
  val select_char_right : View.Command.t [@@js.global]
  val cursor_group_right : View.Command.t [@@js.global]
  val select_group_right : View.Command.t [@@js.global]
  val cursor_line_up : View.Command.t [@@js.global]
  val select_line_up : View.Command.t [@@js.global]
  val cursor_line_down : View.Command.t [@@js.global]
  val select_line_down : View.Command.t [@@js.global]
  val cursor_page_up : View.Command.t [@@js.global]
  val select_page_up : View.Command.t [@@js.global]
  val cursor_page_down : View.Command.t [@@js.global]
  val select_page_down : View.Command.t [@@js.global]
  val cursor_line_boundary_backward : View.Command.t [@@js.global]
  val select_line_boundary_backward : View.Command.t [@@js.global]
  val cursor_doc_start : View.Command.t [@@js.global]
  val select_doc_start : View.Command.t [@@js.global]
  val cursor_line_boundary_forward : View.Command.t [@@js.global]
  val select_line_boundary_forward : View.Command.t [@@js.global]
  val cursor_doc_end : View.Command.t [@@js.global]
  val select_doc_end : View.Command.t [@@js.global]
  val insert_newline_and_indent : View.Command.t [@@js.global]
  val select_all : View.Command.t [@@js.global]
  val delete_char_backward : View.Command.t [@@js.global]
  val delete_char_forward : View.Command.t [@@js.global]
  val delete_group_backward : View.Command.t [@@js.global]
  val delete_group_forward : View.Command.t [@@js.global]
  val cursor_syntax_left : View.Command.t [@@js.global]
  val select_syntax_left : View.Command.t [@@js.global]
  val cursor_syntax_right : View.Command.t [@@js.global]
  val select_syntax_right : View.Command.t [@@js.global]
  val move_line_up : View.Command.t [@@js.global]
  val copy_line_up : View.Command.t [@@js.global]
  val move_line_down : View.Command.t [@@js.global]
  val copy_line_down : View.Command.t [@@js.global]
  val simplify_selection : View.Command.t [@@js.global]
  val select_line : View.Command.t [@@js.global]
  val select_parent_syntax : View.Command.t [@@js.global]
  val indent_less : View.Command.t [@@js.global]
  val indent_more : View.Command.t [@@js.global]
  val indent_selection : View.Command.t [@@js.global]
  val delete_line : View.Command.t [@@js.global]
  val cursor_matching_bracket : View.Command.t [@@js.global]
  val toggle_comment : View.Command.t [@@js.global]
end

module Gutter : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "View"]

  module Block_info : sig
    type t

    val from : t -> int [@@js.get]
    val length : t -> int [@@js.get]
    val top : t -> int [@@js.get]
    val height : t -> int [@@js.get]
    val to_ : t -> int [@@js.get "to"]
    val bottom : t -> int [@@js.get]
  end

  module Line_numbers_config : sig
    type t

    module Dom_event_handlers : sig
      type t

      val create : ?mousedown:(View.Editor_view.t -> Block_info.t -> unit) -> unit -> t
      [@@js.builder]
    end

    val create
      :  ?format_number:(int -> State.Editor_state.t -> string)
      -> ?dom_event_handlers:Dom_event_handlers.t
      -> unit
      -> t
    [@@js.builder]
  end

  val line_numbers : Line_numbers_config.t -> State.Extension.t [@@js.global]
  val highlight_active_line_gutter : unit -> State.Extension.t [@@js.global]
end

module History : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Commands"]

  module Config : sig
    type t

    val create : ?min_depth:int -> ?new_group_delay:int -> unit -> t [@@js.builder]
  end

  val history : Config.t -> State.Extension.t [@@js.global]
  val undo : View.Command.t [@@js.global]
  val redo : View.Command.t [@@js.global]
  val undo_selection : View.Command.t [@@js.global]
  val redo_selection : View.Command.t [@@js.global]
end

module Search : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Search"]

  module Config : sig
    type t

    val create : ?top:bool -> ?match_case:bool -> unit -> t [@@js.builder]
  end

  val search : Config.t -> State.Extension.t [@@js.global]
  val find_next : View.Command.t [@@js.global]
  val find_previous : View.Command.t [@@js.global]
  val select_matches : View.Command.t [@@js.global]
  val replace_next : View.Command.t [@@js.global]
  val replace_all : View.Command.t [@@js.global]
  val open_search_panel : View.Command.t [@@js.global]
  val close_search_panel : View.Command.t [@@js.global]
  val goto_line : View.Command.t [@@js.global]
end

module Basic_setup : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Basic_setup"]

  val basic_setup : State.Extension.t [@@js.global]
end

module Language : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Language"]

  type t

  val extension : t -> State.Extension.t [@@js.get]
end

module Stream_parser : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Language"]

  module String_stream : sig
    type t

    val t_of_js : Ojs.t -> t
    val t_to_js : t -> Ojs.t

    val new_string_stream : string:string -> tab_size:int -> indent_unit:int -> t
    [@@js.new]

    val pos : t -> int [@@js.get]
    val set_pos : t -> int -> unit [@@js.set "pos"]
    val start : t -> int [@@js.get]
    val string : t -> string [@@js.get]
    val eol : t -> bool [@@js.call]
    val sol : t -> bool [@@js.call]
    val peek : t -> string option [@@js.call]
    val next : t -> string option [@@js.call]
    val current : t -> string [@@js.call]
    val skip_to_end : t -> unit [@@js.call "skipToEnd"]
    val eat_space : t -> bool [@@js.call "eatSpace"]
    val eat : t -> match_:string -> string option [@@js.call]
    val eat_regex : t -> match_:RegExp.t -> string option [@@js.call "eat"]
    val eat_while : t -> match_:string -> bool [@@js.call "eatWhile"]
    val eat_while_regex : t -> match_:RegExp.t -> bool [@@js.call "eatWhile"]

    val match_
      :  t
      -> pattern:string
      -> ?consume:bool
      -> ?case_insensitive:bool
      -> unit
      -> bool
    [@@js.call "match"]

    val match_regex
      :  t
      -> pattern:RegExp.t
      -> ?consume:bool
      -> ?case_insensitive:bool
      -> unit
      -> string list option
    [@@js.call "match"]
  end

  module Stream_parser : sig
    type t

    val create
      :  ?language_data:Ojs.t
      -> start_state:(unit -> 'state)
      -> token:(String_stream.t -> 'state -> string option)
      -> unit
      -> t
    [@@js.builder]
  end

  module Stream_language : sig
    type t

    val define : Stream_parser.t -> t [@@js.global]
    val to_language : t -> Language.t [@@js.cast]
  end
  [@@js.scope "StreamLanguage"]
end

module Mllike : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Legacy_modes_mllike"]

  open Stream_parser

  type t

  val ocaml : Stream_parser.t [@@js.global "oCaml"]
  val fsharp : Stream_parser.t [@@js.global "fSharp"]
  val sml : Stream_parser.t [@@js.global]
end

module Diff : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Legacy_modes_diff"]

  open Stream_parser

  type t

  val diff : Stream_parser.t [@@js.global]
end

module Commonlisp : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Legacy_modes_commonlisp"]

  open Stream_parser

  type t

  val common_lisp : Stream_parser.t [@@js.global]
end

module Scheme : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Legacy_modes_scheme"]

  open Stream_parser

  type t

  val scheme : Stream_parser.t [@@js.global]
end

module Lezer_highlight : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lezer_highlight"]

  module Highlighter : sig
    type t
  end

  module Tag : sig
    type t
  end

  module Tags : sig
    type t

    val arithmetic_operator : t -> Tag.t [@@js.get]
    val bool : t -> Tag.t [@@js.get]
    val bracket : t -> Tag.t [@@js.get]
    val comment : t -> Tag.t [@@js.get]
    val compare_operator : t -> Tag.t [@@js.get]
    val content : t -> Tag.t [@@js.get]
    val control_keyword : t -> Tag.t [@@js.get]
    val definition_keyword : t -> Tag.t [@@js.get]
    val doc_comment : t -> Tag.t [@@js.get]
    val invalid : t -> Tag.t [@@js.get]
    val keyword : t -> Tag.t [@@js.get]
    val literal : t -> Tag.t [@@js.get]
    val logic_operator : t -> Tag.t [@@js.get]
    val macro_name : t -> Tag.t [@@js.get]
    val name : t -> Tag.t [@@js.get]
    val number : t -> Tag.t [@@js.get]
    val operator : t -> Tag.t [@@js.get]
    val paren : t -> Tag.t [@@js.get]
    val punctuation : t -> Tag.t [@@js.get]
    val string : t -> Tag.t [@@js.get]
    val type_name : t -> Tag.t [@@js.get]
  end

  val tags : Tags.t [@@js.global]
  val class_highlighter : Highlighter.t [@@js.global]
end

module Highlight : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Language"]

  module Syntax_highlighting_options : sig
    type t

    val create : ?fallback:bool -> unit -> t [@@js.builder]
  end

  module Highlight_style : sig
    val define : specs:Ojs.t list -> Lezer_highlight.Highlighter.t [@@js.global]
  end
  [@@js.scope "HighlightStyle"]

  val syntax_highlighting
    :  Lezer_highlight.Highlighter.t
    -> ?options:Syntax_highlighting_options.t
    -> unit
    -> State.Extension.t
  [@@js.global]

  val default_highlight_style : Lezer_highlight.Highlighter.t [@@js.global]
end

module Folding : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Language"]

  module Fold : sig
    type t

    val create : from:int -> to_:(int[@js "to"]) -> t [@@js.builder]
  end

  module Fold_service_callback : sig
    type t = State.Editor_state.t -> line_start:int -> line_end:int -> Fold.t option

    val t_to_js : t -> Ojs.t
  end

  val fold_service : (Fold_service_callback.t, Fold_service_callback.t) State.Facet.t
  [@@js.global]

  val fold_gutter : unit -> State.Extension.t [@@js.global]
end

module Matchbrackets : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Language"]

  val bracket_matching : unit -> State.Extension.t [@@js.global]
end

module Lang_markdown : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_markdown"]

  val markdown : unit -> Language.t [@@js.global]
end

module Lang_python : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_python"]

  val python : unit -> Language.t [@@js.global]
end

module Lang_sql : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_sql"]

  module Sql_dialect_spec : sig
    type t

    val create
      :  ?keywords:string
      -> ?builtin:string
      -> ?types:string
      -> ?backslash_escapes:string
      -> ?hash_comments:bool
      -> ?slash_comments:bool
      -> ?space_after_dashes:bool
      -> ?double_quoted_strings:bool
      -> ?char_set_casts:bool
      -> ?operator_chars:string
      -> ?special_var:string
      -> ?identifier_quotes:string
      -> unit
      -> t
    [@@js.builder]
  end

  module Sql_dialect : sig
    [@@@js.scope "SQLDialect"]

    type t

    val define : spec:Sql_dialect_spec.t -> t [@@js.global]
  end

  module Sql_config : sig
    type t

    val create
      :  ?dialect:Sql_dialect.t
      -> ?upper_case_keywords:bool
      -> ?tables:Autocomplete.Completion.t list
      -> ?default_table:int
      -> ?schema:Ojs.t
           (** [schema] is required to be truthy in order to activate completion with [tables]
      *)
      -> unit
      -> t
    [@@js.builder]
  end

  val postgresql : Sql_dialect.t [@@js.global "PostgreSQL"]
  val sql : ?config:Sql_config.t -> unit -> Language.t [@@js.global]
end

module Lang_html : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_html"]

  type t

  val html : unit -> Language.t [@@js.global]
end

module Lang_css : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_css"]

  type t

  val css : unit -> Language.t [@@js.global]
end

module Lang_javascript : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_javascript"]

  type t

  val javascript : unit -> Language.t [@@js.global]
end

module Lang_php : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_php"]

  type t

  val php : unit -> Language.t [@@js.global]
end

module Lang_rust : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_rust"]

  type t

  val rust : unit -> Language.t [@@js.global]
end

module Lang_xml : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Lang_xml"]

  type t

  val xml : unit -> Language.t [@@js.global]
end

module Merge : sig
  [@@@js.scope "codemirror"]
  [@@@js.scope "Merge"]

  module Unified_merge_config : sig
    type t

    val create_from_string
      :  ?highlight_changes:bool
      -> ?gutter:bool
      -> ?syntax_highlight_deletions:bool
      -> ?merge_controls:bool
      -> (string[@js "original"])
      -> t
    [@@js.builder]

    val create_from_text
      :  ?highlight_changes:bool
      -> ?gutter:bool
      -> ?syntax_highlight_deletions:bool
      -> ?merge_controls:bool
      -> (Text.Text.t[@js "original"])
      -> t
    [@@js.builder]
  end

  val unified_merge_view : Unified_merge_config.t -> State.Extension.t [@@js.global]
end
