open! Core
module Bonsai_proc := Bonsai_web.Proc
open! Bonsai_web

(** These controls come unstyled by default. jane-web-style provides css that will make
    the control and option pills pretty. *)

(** A ['a t] represents a typeahead whose value has type ['a].

    [current_input] gives access to the current contents of the form's [<input>] element
*)
type 'a t =
  { selected : 'a
  ; set_selected : 'a -> unit Ui_effect.t
  ; current_input : string
  ; view : Vdom.Node.t
  }

(** [create] returns a typeahead using native browser controls.

    [to_option_description] if provided will render the description provided below the
    option.

    Note that [set_selected] does not enforce that the given value is present in
    [all_options]. Setting a value not in [all_options] will successfully set the
    typeahead to that value.
*)
val create
  :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
  -> ?placeholder:string
  -> ?on_select_change:('a option -> unit Ui_effect.t) Bonsai.t
  -> ?to_string:('a -> string) Bonsai.t
  -> ?to_option_description:('a -> string) Bonsai.t
  -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
  -> (module Bonsai_proc.Model with type t = 'a)
  -> equal:('a -> 'a -> bool)
  -> all_options:'a list Bonsai.t
  -> Bonsai.graph
  -> 'a option t Bonsai.t

val create_multi
  :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
  -> ?placeholder:string
  -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Bonsai.t
  -> ?to_string:('a -> string) Bonsai.t
  -> ?to_option_description:('a -> string) Bonsai.t
  -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
  -> ?split:(string -> string list)
  -> ('a, 'cmp) Bonsai.comparator
  -> all_options:'a list Bonsai.t
  -> Bonsai.graph
  -> ('a, 'cmp) Set.t t Bonsai.t

module Private : sig
  module For_testing : sig
    val create_with_browser_behavior_in_test
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?placeholder:string
      -> ?on_select_change:('a option -> unit Ui_effect.t) Bonsai.t
      -> ?to_string:('a -> string) Bonsai.t
      -> ?to_option_description:('a -> string) Bonsai.t
      -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
      -> (module Bonsai_proc.Model with type t = 'a)
      -> equal:('a -> 'a -> bool)
      -> all_options:'a list Bonsai.t
      -> Bonsai.graph
      -> 'a option t Bonsai.t

    val create_multi_with_browser_behavior_in_test
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?placeholder:string
      -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Bonsai.t
      -> ?to_string:('a -> string) Bonsai.t
      -> ?to_option_description:('a -> string) Bonsai.t
      -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
      -> ?split:(string -> string list)
      -> ('a, 'cmp) Bonsai.comparator
      -> all_options:'a list Bonsai.t
      -> Bonsai.graph
      -> ('a, 'cmp) Set.t t Bonsai.t
  end
end
