open! Core
open! Bonsai_web
open! Async_kernel
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

module Style =
  [%css
  stylesheet
    {|
  .container {
    display: flex;
    padding: 0.25em;
    flex-wrap: wrap;
    align-items: flex-start;
    justify-content: space-evenly;
    gap: 1em;
    z-index: 1;
    user-select:none;
  }

  .image-wrapper {
    border: 1px solid rgb(49, 57, 67);
    height: max-content;
    display: inline-flex;
    padding: 5px;
    border-radius: 9px;
    background: rgb(20, 24, 28);
    overflow: clip;
    position: relative;
  }

  .image-wrapper > img {
    border-radius: 4px;
    box-shadow: currentcolor 0px 0px 5px;
    color: rgb(20, 24, 28);
    padding: 0px;
    margin: 0px;
    /*filter: blur(20px);*/
    transition: 1s linear filter;
    filter: blur(0px);
  }

  .image-wrapper.preview > img {
    filter: blur(10px);
  }


  .icon-button  {
    position: absolute;
    background: rgba(255, 255, 255, 0.5);
    border: 1px solid white;
    display: flex;
    border-radius: 2px;
    backdrop-filter: blur(5px);
    opacity:0%;
    transition: opacity 0.15s ease-in;
    cursor: pointer;
  }

  .image-wrapper:hover > .icon-button {
    opacity: 50%;
  }

  .image-wrapper > .icon-button:hover {
    opacity: 100%;
  }

  .remove-button {
    top: 8px;
    right: 8px;
  }

  .set-params-button {
    top: 8px;
    left: 8px;
  }

|}]

type t =
  { ongoing : bool
  ; wrap_request : 'a. 'a Effect.t -> 'a Effect.t
  ; add_images :
      params:Txt2img.Query.t -> images:Base64_image.t Or_error.t list -> unit Effect.t
  ; view : preview:Vdom.Node.t option -> Vdom.Node.t
  }

let icon_svg content =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:
      Virtual_dom_svg.Attr.
        [ width 16.0
        ; height 16.0
        ; viewbox ~min_x:0.0 ~min_y:0.0 ~width:24.0 ~height:24.0
        ; stroke (`Name "black")
        ; stroke_width 2.0
        ; Vdom.Attr.create "fill" "none"
        ; stroke_linecap `Round
        ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:content
    ()
;;

let image_wrapper ~remove ~set_params ~image =
  let remove_btn =
    Vdom.Node.div
      ~attrs:
        [ Style.icon_button; Style.remove_button; Vdom.Attr.on_click (fun _ -> remove) ]
      [ icon_svg
          {| <line x1="18" y1="6" x2="6" y2="18"/><line x1="6" y1="6" x2="18" y2="18"/> |}
      ]
  in
  let set_params_btn =
    Vdom.Node.div
      ~attrs:
        [ Style.set_params_button
        ; Style.icon_button
        ; Vdom.Attr.on_click (fun _ -> set_params)
        ]
      [ icon_svg
          {| <line x1="4" y1="21" x2="4" y2="14"/><line x1="4" y1="10" x2="4" y2="3"/><line x1="12" y1="21" x2="12" y2="12"/><line x1="12" y1="8" x2="12" y2="3"/><line x1="20" y1="21" x2="20" y2="16"/><line x1="20" y1="12" x2="20" y2="3"/><line x1="1" y1="14" x2="7" y2="14"/><line x1="9" y1="8" x2="15" y2="8"/><line x1="17" y1="16" x2="23" y2="16"/> |}
      ]
  in
  Vdom.Node.div
    ~attrs:[ Style.image_wrapper ]
    [ Base64_image.to_vdom image; remove_btn; set_params_btn ]
;;

module Key = struct
  module T = struct
    type t =
      | Id of int
      | Preview
    [@@deriving compare, sexp]

    let compare = Comparable.reverse compare
  end

  include T
  include Comparable.Make (T)
end

let component ~set_params =
  let%sub ongoing =
    Bonsai.state_machine0 ~default_model:0 () ~apply_action:(fun _ctx model -> function
      | `Incr -> model + 1
      | `Decr -> model - 1)
  in
  let%sub (_, images), modify_images =
    Bonsai.state_machine0
      ~default_model:(0, Map.empty (module Key))
      ~apply_action:(fun _ctx (next_idx, map) -> function
        | `Add (params, images) ->
          List.fold ~init:(next_idx, map) images ~f:(fun (next_idx, map) image ->
            next_idx + 1, Map.add_exn map ~key:(Id next_idx) ~data:(params, image))
        | `Remove idx -> next_idx, Map.remove map idx)
      ()
  in
  let%sub images =
    Bonsai.assoc
      (module Key)
      images
      ~f:(fun idx data ->
        let%arr idx = idx
        and data = data
        and set_params = set_params
        and modify_images = modify_images in
        match data with
        | params, Ok image ->
          let remove = modify_images (`Remove idx) in
          let set_params = set_params params in
          image_wrapper ~remove ~set_params ~image
        | _params, Error e -> Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e))
  in
  let%sub view =
    let%arr images = images in
    fun ~preview ->
      let images =
        match preview with
        | None -> images
        | Some preview ->
          let preview =
            Vdom.Node.div ~attrs:[ Style.image_wrapper; Style.preview ] [ preview ]
          in
          Map.set images ~key:Preview ~data:preview
      in
      Vdom_node_with_map_children.make
        ~tag:"div"
        ~attr:(Vdom.Attr.many [ Style.container ])
        images
  in
  let%arr ongoing, inject_ongoing = ongoing
  and modify_images = modify_images
  and view = view in
  { ongoing = ongoing > 0
  ; wrap_request =
      (fun e ->
        let open Effect.Let_syntax in
        let%bind () = inject_ongoing `Incr in
        let%bind r = e in
        let%bind () = inject_ongoing `Decr in
        return r)
  ; view
  ; add_images = (fun ~params ~images -> modify_images (`Add (params, images)))
  }
;;
