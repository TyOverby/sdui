open! Core
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_manual_view

val textarea
  :  ?default:string
  -> ?validate:(string -> string)
  -> ?container_attrs:Vdom.Attr.t list
  -> ?textarea_attrs:Vdom.Attr.t list
  -> ?label:string
  -> Bonsai.graph
  -> (string, ?colorize:(string -> Vdom.Node.t list) -> unit -> Vdom.Node.t) Form.t
       Bonsai.t

val int_form
  :  ?input_attrs:Vdom.Attr.t list
  -> ?container_attrs:
       (state:Int63.t -> set_state:(Int63.t -> unit Effect.t) -> Vdom.Attr.t list)
  -> title:string
  -> default:Int63.t
  -> step:int
  -> length:Css_gen.Length.t
  -> min:Int63.t
  -> max:Int63.t
  -> validate_or_correct:(string -> (Int63.t, Int63.t) Result.t)
  -> Bonsai.graph
  -> (Int63.t, Vdom.Node.t) Form.t Bonsai.t

val bool_form
  :  ?input_attrs:Vdom.Attr.t list
  -> ?container_attrs:Vdom.Attr.t list
  -> title:string
  -> default:bool
  -> Bonsai.graph
  -> (bool, Vdom.Node.t) Form.t Bonsai.t

module Label_modifications : sig
  val muted_label : Vdom.Attr.t

  module Variables : sig
    val set_all : border:string -> fg:string -> Vdom.Attr.t
  end
end
