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

    max-height: 75vh;
    max-width: 66vw;
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

  .image-wrapper:hover .icon-button {
    opacity: 50%;
  }

  .image-wrapper .icon-button:hover {
    opacity: 100%;
  }

  .remove-button {
    top: 8px;
    right: 8px;
  }

  .set-params-container {
    top: 8px;
    left: 8px;
    position: absolute;
    gap:4px;
  }

  .set-params-container .icon-button {
    position:unset;
  }

|}]

type t =
  { queue_request : Txt2img.Query.t -> unit Effect.t
  ; view : Vdom.Node.t
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

let image_wrapper ~remove ~set_params ~upscale ~aspect_ratio image =
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
      ~attrs:[ Style.icon_button; Vdom.Attr.on_click (fun _ -> set_params) ]
      [ icon_svg
          {| <line x1="4" y1="21" x2="4" y2="14"/><line x1="4" y1="10" x2="4" y2="3"/><line x1="12" y1="21" x2="12" y2="12"/><line x1="12" y1="8" x2="12" y2="3"/><line x1="20" y1="21" x2="20" y2="16"/><line x1="20" y1="12" x2="20" y2="3"/><line x1="1" y1="14" x2="7" y2="14"/><line x1="9" y1="8" x2="15" y2="8"/><line x1="17" y1="16" x2="23" y2="16"/> |}
      ]
  in
  let upscale_btn =
    match upscale with
    | None -> Vdom.Node.none
    | Some upscale ->
      Vdom.Node.div
        ~attrs:[ Style.icon_button; Vdom.Attr.on_click (fun _ -> upscale) ]
        [ icon_svg
            {|<polyline points="17 11 12 6 7 11"></polyline><polyline points="17 18 12 13 7 18"></polyline>|}
        ]
  in
  Vdom.Node.div
    ~attrs:[ Style.image_wrapper; Vdom.Attr.style aspect_ratio ]
    [ image
    ; remove_btn
    ; View.hbox ~attrs:[ Style.set_params_container ] [ set_params_btn; upscale_btn ]
    ]
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

let component ~host_and_port ~set_params =
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
  let%sub add_images =
    let%arr modify_images = modify_images in
    fun ~params ~images -> modify_images (`Add (params, images))
  in
  let%sub { view = queue_view; preview_view; queue_request } =
    Request_queue.component ~host_and_port ~add_images
  in
  let%sub images =
    Bonsai.assoc
      (module Key)
      images
      ~f:(fun idx data ->
        match%sub data with
        | params, Ok (image, info) ->
          let%sub image_vdom_and_aspect_ratio =
            let%arr image = image in
            let vdom = Base64_image.to_vdom image ~drop_size:true in
            let aspect_ratio =
              match Base64_image.size image with
              | Some (w, h) ->
                Css_gen.create
                  ~field:"aspect-ratio"
                  ~value:(sprintf "%s / %s" (Int63.to_string w) (Int63.to_string h))
              | None -> Css_gen.empty
            in
            vdom, aspect_ratio
          in
          let%arr idx = idx
          and info = info
          and params = params
          and image_vdom, aspect_ratio = image_vdom_and_aspect_ratio
          and set_params = set_params
          and modify_images = modify_images
          and queue_request = queue_request in
          let remove = modify_images (`Remove idx) in
          let set_params = set_params (Txt2img.Query.apply_info params info) in
          let upscale =
            if params.enable_hr
            then None
            else
              lazy (queue_request { params with Txt2img.Query.seed = info.seed; enable_hr = true })
              |> Effect.lazy_
              |> Some
          in
          image_wrapper ~remove ~set_params ~upscale ~aspect_ratio image_vdom
        | _params, Error e ->
          let%arr e = e in
          Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e))
  in
  let%sub view =
    let%arr images = images
    and preview_view = preview_view
    and queue_view = queue_view in
    let images =
      match preview_view with
      | None -> images
      | Some preview ->
        let preview =
          Vdom.Node.div ~attrs:[ Style.image_wrapper; Style.preview ] [ preview ]
        in
        Map.set images ~key:Preview ~data:preview
    in
    Vdom.Node.div
      [ Vdom_node_with_map_children.make
          ~tag:"div"
          ~attr:(Vdom.Attr.many [ Style.container ])
          images
      ; queue_view
      ]
  in
  let%arr queue_request = queue_request
  and view = view in
  { view; queue_request }
;;
