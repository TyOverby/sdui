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

  let component graph =
    let seed = P.seed_form graph in
    let pos_prompt = P.prompt_form ~label:"prompt" graph in
    let neg_prompt = P.prompt_form ~label:"negative" graph in
    let width = P.width_height_form ~label:"width" graph in
    let height = P.width_height_form ~label:"height" graph in
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
             dispatcher (function
               | None ->
                 Effect.return (Error (Error.of_string "couldn't find host for dispatch"))
               | Some (host, _) ->
                 Sd.Txt2img.dispatch
                   ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
                   query))
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
        ~equal_b:Sd.Base64_image.equal
        query
        prev
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool in
           fun query prev ->
             let query = { query with Sd.Img2img.Query.init_images = [ prev ] } in
             dispatcher (function
               | None ->
                 Effect.return (Error (Error.of_string "couldn't find host for dispatch"))
               | Some (host, _) ->
                 Sd.Img2img.dispatch
                   ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
                   query))
      |> Inc.collapse_error
  in
  result
  |> Inc.map_pure ~f:(function
    | [ (image, _) ] -> Ok image
    | [] -> Error (Error.of_string "no images in response")
    | _ :: _ :: _ -> Error (Error.of_string "more than one image in response"))
  |> Inc.collapse_error
;;

let component ~pool ~prev graph =
  let ({ Parameters.seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise } as
       params)
    =
    Parameters.component graph
  in
  let image = image ~pool ~prev ~params graph in
  let%arr seed = seed
  and pos_prompt = pos_prompt
  and neg_prompt = neg_prompt
  and width = width
  and height = height
  and steps = steps
  and cfg = cfg
  and denoise = denoise
  and image = image
  and theme = View.Theme.current graph in
  let image =
    match image with
    | Fresh img -> Vdom.Node.div [ Sd.Base64_image.to_vdom img ]
    | Stale img ->
      Vdom.Node.div ~attrs:[ {%css| opacity: 0.5;|} ] [ Sd.Base64_image.to_vdom img ]
    | Not_computed -> Vdom.Node.div [ Vdom.Node.text "not computed yet..." ]
    | Error e -> Vdom.Node.div [ Vdom.Node.sexp_for_debugging (Error.sexp_of_t e) ]
  in
  View.card'
    theme
    [ View.vbox
        [ View.hbox [ Form.view pos_prompt; Form.view neg_prompt ]
        ; View.hbox
            [ View.vbox
                [ Form.view width
                ; Form.view height
                ; Form.view steps
                ; Form.view cfg
                ; Form.view denoise
                ; Form.view seed
                ]
            ; image
            ]
        ]
    ]
;;
