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
    flex-flow: row-reverse wrap-reverse;
    padding: 0.25em;
    align-items: flex-end;
    justify-content: flex-start;
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

  .image-wrapper.upscaling > img {
    filter: blur(10px);
    filter: unset;
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

  .choice-container {
    bottom: 6px;
    left: 6px;
    right: 6px;
    max-height: 25%;
    max-width: 100%;
    position: absolute;
    gap:4px;

  }

  .choice-container > * {
    flex-grow: 1;
    display: flex; 
    justify-content: center;
  }

  .choice-container .icon-button {
    position:unset;
  }

|}]

type t =
  { queue_request : Txt2img.Query.t -> unit Effect.t
  ; view : Vdom.Node.t
  }

let icon_svg ~maximize content =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:
      Virtual_dom_svg.Attr.
        [ Vdom.Attr.many (if maximize then [] else [ width 16.0; height 16.0 ])
        ; viewbox ~min_x:0.0 ~min_y:0.0 ~width:24.0 ~height:24.0
        ; stroke (`Name "black")
        ; stroke_width 2.0
        ; Vdom.Attr.create "fill" "none"
        ; stroke_linecap `Round
        ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:content
    ()
;;

let image_wrapper
  ~upscaling
  ~remove
  ~set_params
  ~duplicate
  ~similar
  ~upscale
  ~aspect_ratio
  image
  =
  let remove_btn =
    Vdom.Node.div
      ~attrs:
        [ Style.icon_button; Style.remove_button; Vdom.Attr.on_click (fun _ -> remove) ]
      [ icon_svg
          ~maximize:true
          {| <line x1="18" y1="6" x2="6" y2="18"/><line x1="6" y1="6" x2="18" y2="18"/> |}
      ]
  in
  let set_params_btn =
    Vdom.Node.div
      ~attrs:[ Style.icon_button; Vdom.Attr.on_click (fun _ -> set_params) ]
      [ icon_svg
          ~maximize:false
          {| <line x1="4" y1="21" x2="4" y2="14"/><line x1="4" y1="10" x2="4" y2="3"/><line x1="12" y1="21" x2="12" y2="12"/><line x1="12" y1="8" x2="12" y2="3"/><line x1="20" y1="21" x2="20" y2="16"/><line x1="20" y1="12" x2="20" y2="3"/><line x1="1" y1="14" x2="7" y2="14"/><line x1="9" y1="8" x2="15" y2="8"/><line x1="17" y1="16" x2="23" y2="16"/> |}
      ]
  in
  let duplicate_btn =
    Vdom.Node.div
      ~attrs:[ Style.icon_button; Vdom.Attr.on_click (fun _ -> duplicate) ]
      [ icon_svg
          ~maximize:true
          {| <rect x="9" y="9" width="13" height="13" rx="2" ry="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/> |}
      ]
  in
  let similar_btn =
    Vdom.Node.div
      ~attrs:[ Style.icon_button; Vdom.Attr.on_click (fun _ -> similar) ]
      [ icon_svg
          ~maximize:true
          {| <path d="M8 3v3a2 2 0 0 1-2 2H3m18 0h-3a2 2 0 0 1-2-2V3m0 18v-3a2 2 0 0 1 2-2h3M3 16h3a2 2 0 0 1 2 2v3"/> |}
      ]
  in
  let upscale_btn =
    match upscale with
    | None -> Vdom.Node.none
    | Some upscale ->
      Vdom.Node.div
        ~attrs:[ Style.icon_button; Vdom.Attr.on_click (fun _ -> upscale) ]
        [ icon_svg
            ~maximize:true
            {|<polyline points="17 11 12 6 7 11"></polyline><polyline points="17 18 12 13 7 18"></polyline>|}
        ]
  in
  let maybe_upscaling =
    match upscaling with
    | `Not_upscaling -> Vdom.Attr.empty
    | `Upscaling -> Style.upscaling
  in
  Vdom.Node.div
    ~attrs:[ Style.image_wrapper; maybe_upscaling; Vdom.Attr.style aspect_ratio ]
    [ image
    ; set_params_btn
    ; View.hbox
        ~attrs:[ Style.choice_container ]
        [ upscale_btn; duplicate_btn; similar_btn; remove_btn ]
    ]
;;

module Key = struct
  module T = struct
    type t =
      | Id of int
      | Preview
    [@@deriving compare, sexp]

    let compare = compare
  end

  include T
  include Comparable.Make (T)
end

let component ~(request_host : Hosts.request_host Value.t) ~set_params =
  let%sub (_, images), modify_images =
    Bonsai.state_machine0
      ~default_model:(0, Map.empty (module Key))
      ~apply_action:(fun _ctx (next_idx, map) -> function
        | `Enqueue (params, count) ->
          List.init count ~f:(Fn.const ())
          |> List.fold ~init:(next_idx, map) ~f:(fun (next_idx, map) () ->
            next_idx + 1, Map.add_exn map ~key:(Id next_idx) ~data:params)
        | `Remove idx -> next_idx, Map.remove map idx)
      ()
  in
  let%sub add_images =
    let%arr modify_images = modify_images in
    fun ~params ~count -> modify_images (`Enqueue (params, count))
  in
  let%sub images =
    Bonsai.assoc
      (module Key)
      images
      ~f:(fun idx params ->
        let%sub state, set_state = Bonsai.state `Queued in
        let%sub dispatch =
          let%arr request_host = request_host
          and set_state = set_state in
          fun params ->
            let%bind.Effect work = request_host in
            Effect.ignore_m
            @@ work.f (fun host_and_port ->
              match%bind.Effect Txt2img.dispatch ~host_and_port params with
              | Ok [] ->
                let error = Error.of_string "at least one image expected" in
                let%map.Effect () = set_state (`Error error) in
                Error error
              | Ok ((img, info) :: _) ->
                let%map.Effect () = set_state (`Done (img, info, `Not_upscaling)) in
                Ok ()
              | Error e ->
                let%map.Effect () = set_state (`Error e) in
                Error e)
        in
        let%sub () =
          let on_activate =
            let%map params = params
            and dispatch = dispatch in
            dispatch params
          in
          Bonsai.Edge.lifecycle ~on_activate ()
        in
        match%sub state with
        | `Queued ->
          let%arr params = params
          and modify_images = modify_images
          and set_params = set_params
          and idx = idx in
          let remove = modify_images (`Remove idx) in
          let w, h = params.width, params.height in
          let aspect_ratio =
            Css_gen.combine
              (Css_gen.create
                 ~field:"aspect-ratio"
                 ~value:(sprintf "%s / %s" (Int63.to_string w) (Int63.to_string h)))
              (Css_gen.width (`Px (Int63.to_int_exn w)))
          in
          image_wrapper
            ~upscaling:`Not_upscaling
            ~remove
            ~set_params:(set_params params)
            ~duplicate:Effect.Ignore
            ~similar:Effect.Ignore
            ~upscale:None
            ~aspect_ratio
            (Vdom.Node.text "generating...")
            (*Vdom.Node.text (Sexp.to_string_hum [%sexp (params : Txt2img.Query.t)])*)
        | `Done (image, info, upscaling) ->
          let%sub image_vdom_and_aspect_ratio =
            let%arr image = image
            and info = info in
            let vdom = Base64_image.to_vdom image ~drop_size:true in
            let aspect_ratio =
              match Base64_image.size image with
              | Some (w, h) ->
                Css_gen.combine
                  (Css_gen.create
                     ~field:"aspect-ratio"
                     ~value:(sprintf "%s / %s" (Int63.to_string w) (Int63.to_string h)))
                  (let w = if info.enable_hr then Int63.(w / of_int 2) else w in
                   Css_gen.width (`Px (Int63.to_int_exn w)))
              | None -> Css_gen.empty
            in
            vdom, aspect_ratio
          in
          let%sub upscale =
            let%arr info = info
            and upscaling = upscaling
            and dispatch = dispatch
            and set_state = set_state
            and image = image
            and params = params in
            match info.enable_hr, upscaling with
            | true, _ | _, `Upscaling -> None
            | _ ->
              lazy
                (let%bind.Effect () = set_state (`Done (image, info, `Upscaling)) in
                 dispatch { params with Txt2img.Query.seed = info.seed; enable_hr = true })
              |> Effect.lazy_
              |> Some
          in
          let%sub () =
            Bonsai_extra.exactly_once (upscale >>| Option.value ~default:Effect.Ignore)
          in
          let%arr idx = idx
          and info = info
          and upscaling = upscaling
          and params = params
          and image_vdom, aspect_ratio = image_vdom_and_aspect_ratio
          and set_params = set_params
          and modify_images = modify_images
          and upscale = upscale in
          let remove = modify_images (`Remove idx) in
          let set_params = set_params (Txt2img.Query.apply_info params info) in
          let duplicate = modify_images (`Enqueue (params, 1)) in
          let similar =
            let params =
              { (Txt2img.Query.apply_info params info) with
                subseed_strength = params.subseed_strength +. 0.05
              ; enable_hr = false
              }
            in
            modify_images (`Enqueue (params, 4))
          in
          image_wrapper
            ~upscaling
            ~remove
            ~set_params
            ~upscale
            ~duplicate
            ~similar
            ~aspect_ratio
            image_vdom
        | `Error e ->
          let%arr e = e in
          Vdom.Node.text "generating...")
          (* Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e))*)
  in
  let%sub view =
    let%arr images = images in
    Vdom.Node.div
      [ Vdom_node_with_map_children.make
          ~tag:"div"
          ~attr:(Vdom.Attr.many [ Style.container ])
          images
      ]
  in
  let%arr add_images = add_images
  and view = view in
  let queue_request params = add_images ~params ~count:1 in
  { view; queue_request }
;;
