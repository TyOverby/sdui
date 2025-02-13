open! Core
open! Bonsai_web
open! Async_kernel
module Form := Bonsai_web_ui_form.With_manual_view

type t =
  ( Txt2img.Query.t
    , on_submit:unit Effect.t -> hosts_panel:Vdom.Node.t -> Vdom.Node.t )
    Form.t

val component
  :  hosts:Hosts.t Bonsai.t
  -> available_hosts:Hosts.Host.Set.t Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t * (Models.t, Vdom.Node.t) Form.t Bonsai.t

module Individual : sig
  val width_height_form
    :  ?default:int
    -> label:string
    -> local_ Bonsai.graph
    -> (Int63.t, Vdom.Node.t) Form.t Bonsai.t

  val min_1_form
    :  ?step:int
    -> ?input_attrs:Vdom.Attr.t list Bonsai.t
    -> default:Int63.t
    -> max:int
    -> label:string
    -> local_ Bonsai.graph
    -> (Int63.t, Vdom.Node.t) Form.t Bonsai.t

  val seed_form
    :  ?container_attrs:
         (state:Int63.t -> set_state:(Int63.t -> unit Effect.t) -> Vdom.Attr.t list)
           Bonsai.t
    -> local_ Bonsai.graph
    -> (Int63.t, Vdom.Node.t) Form.t Bonsai.t

  val prompt_form
    :  ?default:string
    -> ?textarea_attrs:Vdom.Attr.t list
    -> ?container_attrs:Vdom.Attr.t list
    -> label:string
    -> local_ Bonsai.graph
    -> (string, Vdom.Node.t) Form.t Bonsai.t
end
