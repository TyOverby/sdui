open! Core
open! Bonsai_web
module Form := Form_manual

module Optional : sig
  (* [dropdown] takes an existing form, and adds a dropdown with [some_label] and
     [none_label] options. If the user selects the [some_label] option, the provided form
     is used for the inner value of the option. *)
  val dropdown
    :  ?some_label:string (** default ["Some"] *)
    -> ?none_label:string (** default ["None"] *)
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> (Bonsai.graph -> ('a, 'view) Form.t Bonsai.t)
       (** shown when the [some_label] option is selected *)
    -> Bonsai.graph
    -> ('a option, Vdom.Node.t * 'view option) Form.t Bonsai.t
end
