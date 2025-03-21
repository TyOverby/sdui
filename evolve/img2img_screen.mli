open! Core
open! Bonsai_web
module Lease_pool = Sd_chain.Lease_pool

type lease_pool :=
  ( Sd.Hosts.Host.t
    , Sd.Hosts.Current_model.t
    , Sd.Hosts.Host.comparator_witness )
    Lease_pool.t

type basic_card :=
  (modifier:(parameters:Sd_chain.Parameters.t
             -> image:Sd.Image.t
             -> ctrlnet_image:Sd.Image.t option
             -> Sd_chain.Parameters.t)
  * view:(button:Vdom.Node.t -> Vdom.Node.t))
    Bonsai.t

type resize_card :=
  (get_images:Sd_chain.Paint.Images.t Effect.t
   -> set_result:
        (new_width:Base.Int63.t -> new_height:Base.Int63.t -> Sd.Image.t -> unit Effect.t)
   -> Vdom.Node.t)
    Bonsai.t

type ('a, 'b, 'c, 'd, 'e) controlnet_fix_card :=
  (params:(module_:(Sd.Controlnet_modules.t option, 'a) result
          * model:'b
          * weight:'c
          * start_point:'d
          * end_point:'e)
  * view:(button:Vdom.Node.t -> Vdom.Node.t))
    Bonsai.t

val component
  :  lease_pool:lease_pool
  -> inject:(Image_tree.Action.t -> unit Effect.t) Bonsai.t
  -> id:Image_tree.Unique_id.t Bonsai.t
  -> img:Sd.Image.t Bonsai.t
  -> parent_img:Sd.Image.t Bonsai.t
  -> is_image_editor:bool
  -> parameters:Sd_chain.Parameters.t Bonsai.t
  -> refine_card:basic_card
  -> reimagine_card:basic_card
  -> upscale_card:basic_card
  -> other_model_card:basic_card
  -> resize_card:resize_card
  -> controlnet_fix_card:(_, _, _, _, _) controlnet_fix_card
  -> zoom:(float * Vdom.Node.t) Bonsai.t
  -> local_ Bonsai.graph
  -> ((state_tree:Vdom.Node.t -> host_monitor:Vdom.Node.t -> Vdom.Node.t)
     * Vdom_keyboard.Keyboard_event_handler.t
     * (Sd.Image.t -> unit Effect.t) option)
       Bonsai.t
