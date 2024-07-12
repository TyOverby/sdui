open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module P = Sd.Parameters.Individual

module Parameters = struct
  type 'a form = ('a, Vdom.Node.t) Form.t Bonsai.t

  type t =
    { seed : Int63.t form
    ; pos_prompt : string form
    ; neg_prompt : string form
    ; width : Int63.t form
    ; height : Int63.t form
    ; steps : Int63.t form
    ; cfg : Int63.t form
    ; denoise : Int63.t form
    }

  let component ~default_size graph =
    let seed =
      P.seed_form
        ~container_attrs:(fun ~state ~set_state ->
          [ Vdom.Attr.on_double_click (fun _ -> set_state Int63.(state + of_int 4)) ])
        graph
    in
    let pos_prompt =
      P.prompt_form ~container_attrs:[ {%css| flex-grow: 2 |} ] ~label:"prompt" graph
    in
    let neg_prompt =
      P.prompt_form ~container_attrs:[ {%css| flex-grow: 1 |} ] ~label:"negative" graph
    in
    let width = P.width_height_form ~default:default_size ~label:"width" graph in
    let height = P.width_height_form ~default:default_size ~label:"height" graph in
    let steps = P.min_1_form ~default:(Int63.of_int 25) ~max:150 ~label:"steps" graph in
    let cfg = P.min_1_form ~default:(Int63.of_int 7) ~max:30 ~label:"cfg" graph in
    let denoise = P.min_1_form ~default:(Int63.of_int 70) ~max:100 ~label:"deno" graph in
    { seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise }
  ;;

  let for_img2img { seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise } =
    let%arr seed = seed
    and pos_prompt = pos_prompt
    and neg_prompt = neg_prompt
    and width = width
    and height = height
    and steps = steps
    and cfg = cfg
    and denoise = denoise in
    let%map.Or_error prompt = Form.value pos_prompt
    and negative_prompt = Form.value neg_prompt
    and width = Form.value width
    and height = Form.value height
    and steps = Form.value steps
    and cfg_scale = Form.value cfg
    and seed = Form.value seed
    and denoise = Form.value denoise in
    let denoising_strength = Int63.to_float denoise /. 100.0 in
    { Sd.Img2img.Query.init_images = []
    ; prompt
    ; negative_prompt
    ; width
    ; height
    ; steps
    ; cfg_scale
    ; seed
    ; denoising_strength
    ; sampler = Sd.Samplers.default
    ; subseed_strength = 0.0
    ; styles = Sd.Styles.none
    }
  ;;

  let for_txt2img { seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise } =
    let%arr seed = seed
    and pos_prompt = pos_prompt
    and neg_prompt = neg_prompt
    and width = width
    and height = height
    and steps = steps
    and cfg = cfg
    and denoise = denoise in
    let%map.Or_error prompt = Form.value pos_prompt
    and negative_prompt = Form.value neg_prompt
    and width = Form.value width
    and height = Form.value height
    and steps = Form.value steps
    and cfg_scale = Form.value cfg
    and seed = Form.value seed
    and denoise = Form.value denoise in
    let denoising_strength = Int63.to_float denoise /. 100.0 in
    { Sd.Txt2img.Query.prompt
    ; negative_prompt
    ; width
    ; height
    ; steps
    ; cfg_scale
    ; seed
    ; denoising_strength
    ; sampler = Sd.Samplers.default
    ; subseed_strength = 0.0
    ; enable_hr = false
    ; data_url = ""
    ; hr_upscaler = Sd.Upscaler.default
    ; styles = Sd.Styles.none
    }
  ;;
end

let parallel_both a b =
  Effect.Private.make ~request:() ~evaluator:(fun cb ->
    let a_res = ref None
    and b_res = ref None in
    let maybe_finalize () =
      match !a_res, !b_res with
      | Some a, Some b ->
        Effect.Expert.handle_non_dom_event_exn
          (Effect.Private.Callback.respond_to cb (a, b))
      | _ -> ()
    in
    Effect.Expert.handle_non_dom_event_exn
      (let%map.Effect a = a in
       a_res := Some a;
       maybe_finalize ());
    Effect.Expert.handle_non_dom_event_exn
      (let%map.Effect b = b in
       b_res := Some b;
       maybe_finalize ()))
;;

let rec parallel_all = function
  | [] -> Effect.return []
  | a :: rest ->
    let%map.Effect a, rest = parallel_both a (parallel_all rest) in
    a :: rest
;;

let parallel_n n ~f = List.init n ~f:(fun i -> f i) |> parallel_all

let image ~params ~prev ~pool graph =
  let result =
    match%sub prev with
    | None ->
      let query =
        Inc.of_or_error_bonsai
          ~equal:Sd.Txt2img.Query.equal
          ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 2.0))
          (Parameters.for_txt2img params)
          graph
      in
      Inc.map
        ~equal:Sd.Txt2img.Query.equal
        query
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool in
           fun query ->
             let%map.Effect results =
               parallel_n 4 ~f:(fun i ->
                 dispatcher (function
                   | None ->
                     Effect.return
                       (Error (Error.of_string "couldn't find host for dispatch"))
                   | Some (host, _) ->
                     let query =
                       { query with
                         Sd.Txt2img.Query.seed =
                           Int63.(query.Sd.Txt2img.Query.seed + of_int i)
                       }
                     in
                     Sd.Txt2img.dispatch
                       ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
                       query))
             in
             Or_error.all results)
      |> Inc.collapse_error
    | Some prev ->
      let query =
        Inc.of_or_error_bonsai
          ~equal:Sd.Img2img.Query.equal
          ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 2.0))
          (Parameters.for_img2img params)
          graph
      in
      Inc.map2
        ~equal_a:Sd.Img2img.Query.equal
        ~equal_b:[%equal: Sd.Base64_image.t list]
        query
        prev
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool in
           fun query prev ->
             let%map.Effect results =
               parallel_n 4 ~f:(fun i ->
                 let query =
                   { query with
                     Sd.Img2img.Query.init_images =
                       [ List.nth_exn prev (i % List.length prev) ]
                   ; seed = Int63.(query.Sd.Img2img.Query.seed + of_int i)
                   }
                 in
                 dispatcher (function
                   | None ->
                     Effect.return
                       (Error (Error.of_string "couldn't find host for dispatch"))
                   | Some (host, _) ->
                     Sd.Img2img.dispatch
                       ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
                       query))
             in
             Or_error.all results)
      |> Inc.collapse_error
  in
  result
  |> Inc.map_pure ~f:(fun images ->
    List.map images ~f:(function
      | [ (image, _) ] -> Ok image
      | [] -> Error (Error.of_string "no images in response")
      | _ :: _ :: _ -> Error (Error.of_string "more than one image in response"))
    |> Or_error.all)
  |> Inc.collapse_error
;;

let component ~default_size ~pool ~prev graph =
  let ({ Parameters.seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise } as
       params)
    =
    Parameters.component ~default_size graph
  in
  let images, reset =
    Bonsai.with_model_resetter graph ~f:(fun graph -> image ~pool ~prev ~params graph)
  in
  let picked, set_picked = Bonsai.state Int.Set.empty graph in
  let view =
    let%arr seed = seed
    and pos_prompt = pos_prompt
    and neg_prompt = neg_prompt
    and width = width
    and height = height
    and steps = steps
    and cfg = cfg
    and denoise = denoise
    and images = images
    and theme = View.Theme.current graph
    and reset = reset
    and picked = picked
    and set_picked = set_picked in
    let images =
      let base64_to_vdom i img =
        let checked = Set.mem picked i in
        let toggle =
          set_picked (if checked then Set.remove picked i else Set.add picked i)
        in
        Vdom.Node.div
          ~attrs:
            [ {%css| padding: 4px; border-radius: 4px; border:1px solid white; |}
            ; (if checked
               then {%css| background: #10ff1033; border-color:green; |}
               else
                 {%css| background: rgba(255,255,255,0.25); background: rgba(255,255,255,0.5);
             |})
            ]
          [ (let max_width =
               match Sd.Base64_image.size img with
               | Some (w, _) ->
                 Float.to_string
                   (Int63.to_float w /. Js_of_ocaml.Js.Unsafe.global##.devicePixelRatio)
                 ^ "px"
               | None -> "512px"
             in
             Sd.Base64_image.to_vdom
               ~drop_size:true
               ~attrs:
                 [ {%css| width: 33vw; max-width:%{max_width};|}
                 ; Vdom.Attr.on_click (fun _ -> toggle)
                 ]
               img)
          ]
      in
      match images with
      | Fresh img -> View.hbox ~gap:(`Em 1) (List.mapi img ~f:base64_to_vdom)
      | Stale img ->
        View.hbox
          ~gap:(`Em 1)
          ~attrs:[ {%css| opacity: 0.5;|} ]
          (List.mapi img ~f:base64_to_vdom)
      | Not_computed -> Vdom.Node.div [ Vdom.Node.text "not computed yet..." ]
      | Error e -> Vdom.Node.div [ Vdom.Node.sexp_for_debugging (Error.sexp_of_t e) ]
    in
    Vdom.Node.div
      [ View.vbox
          [ View.hbox
              [ View.button theme "reset" ~on_click:reset
              ; View.vbox [ Form.view width; Form.view height ]
              ; View.vbox [ Form.view steps; Form.view cfg ]
              ; View.vbox [ Form.view denoise; Form.view seed ]
              ; Form.view pos_prompt
              ; Form.view neg_prompt
              ]
          ; images
          ]
      ]
  in
  let picked =
    Inc.of_bonsai
      ~equal:Int.Set.equal
      picked
      ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 1.0))
      graph
  in
  let images =
    Inc.map2_pure images picked ~f:(fun images picked ->
      let result = List.filteri images ~f:(fun i _image -> Set.mem picked i) in
      if List.is_empty result then images else result)
  in
  images, view
;;
