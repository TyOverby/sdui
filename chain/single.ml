open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module P = Sd.Parameters.Individual

module Parameters = struct
  type t =
    { seed : Int63.t
    ; pos_prompt : string
    ; neg_prompt : string
    ; width : Int63.t
    ; height : Int63.t
    ; steps : Int63.t
    ; cfg : Int63.t
    ; denoise : Int63.t
    ; ratios : string
    }
  [@@deriving typed_fields]

  let component ~default_size graph =
    let seed =
      P.seed_form
        ~container_attrs:(fun ~state ~set_state ->
          [ Vdom.Attr.on_double_click (fun _ -> set_state Int63.(state + of_int 4)) ])
        graph
    and pos_prompt =
      P.prompt_form ~container_attrs:[ {%css| flex-grow: 2 |} ] ~label:"prompt" graph
    and neg_prompt =
      P.prompt_form ~container_attrs:[ {%css| flex-grow: 1 |} ] ~label:"negative" graph
    and width = P.width_height_form ~default:default_size ~label:"width" graph
    and height = P.width_height_form ~default:default_size ~label:"height" graph
    and steps = P.min_1_form ~default:(Int63.of_int 25) ~max:150 ~label:"steps" graph
    and cfg = P.min_1_form ~default:(Int63.of_int 7) ~max:30 ~label:"cfg" graph
    and denoise = P.min_1_form ~default:(Int63.of_int 70) ~max:100 ~label:"deno" graph
    and ratios =
      Sd.Custom_form_elements.textarea ~label:"ratios" graph
      >>| Form.map_view ~f:(fun view -> view ?colorize:None ())
    in
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        type field_view = Vdom.Node.t

        type resulting_view =
          theme:View.Theme.t -> reset:unit Effect.t -> images:Vdom.Node.t -> Vdom.Node.t

        type form_of_field_fn =
          { f : 'a. 'a Typed_field.t -> ('a, Vdom.Node.t) Form.t Bonsai.t }

        let form_for_field (type a) (field : a Typed_field.t) _graph
          : (a, Vdom.Node.t) Form.t Bonsai.t
          =
          match field with
          | Seed -> seed
          | Pos_prompt -> pos_prompt
          | Neg_prompt -> neg_prompt
          | Width -> width
          | Height -> height
          | Steps -> steps
          | Cfg -> cfg
          | Denoise -> denoise
          | Ratios -> ratios
        ;;

        let finalize_view { f } _graph =
          let%arr width = f Width
          and height = f Height
          and steps = f Steps
          and cfg = f Cfg
          and denoise = f Denoise
          and seed = f Seed
          and ratios = f Ratios
          and pos_prompt = f Pos_prompt
          and neg_prompt = f Neg_prompt in
          fun ~theme ~reset ~images ->
            Vdom.Node.div
              [ View.vbox
                  [ View.hbox
                      [ View.button theme "reset" ~on_click:reset
                      ; View.vbox [ Form.view width; Form.view height ]
                      ; View.vbox [ Form.view steps; Form.view cfg ]
                      ; View.vbox [ Form.view denoise; Form.view seed ]
                      ; Form.view ratios
                      ; Form.view pos_prompt
                      ; Form.view neg_prompt
                      ]
                  ; images
                  ]
              ]
        ;;
      end)
      graph
  ;;

  let for_img2img t =
    let { seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise; ratios = _ } =
      t
    in
    let denoising_strength = Int63.to_float denoise /. 100.0 in
    { Sd.Img2img.Query.init_images = []
    ; mask = None
    ; prompt = pos_prompt
    ; negative_prompt = neg_prompt
    ; width
    ; height
    ; steps
    ; cfg_scale = cfg
    ; seed
    ; denoising_strength
    ; sampler = Sd.Samplers.default
    ; subseed_strength = 0.0
    ; styles = Sd.Styles.none
    }
  ;;

  let for_txt2img t =
    let { seed; pos_prompt; neg_prompt; width; height; steps; cfg; denoise; ratios } =
      t
    in
    let denoising_strength = Int63.to_float denoise /. 100.0 in
    let regional_prompter =
      if String.for_all ratios ~f:Char.is_whitespace
      then None
      else (
        match ratios |> String.lowercase |> String.strip |> String.lsplit2 ~on:' ' with
        | None -> None
        | Some (mode, ratios) ->
          let matrix_mode =
            match mode with
            | "row" | "rows" -> `Rows
            | "col" | "column" | "columns" -> `Columns
            | _ -> `Rows
          in
          let ratios = String.filter ratios ~f:(Fn.non Char.is_whitespace) in
          Some { Sd.Alwayson_scripts.Regional_prompter.Query.matrix_mode; ratios })
    in
    { Sd.Txt2img.Query.prompt = pos_prompt
    ; negative_prompt = neg_prompt
    ; width
    ; height
    ; steps
    ; cfg_scale = cfg
    ; seed
    ; denoising_strength
    ; sampler = Sd.Samplers.default
    ; subseed_strength = 0.0
    ; enable_hr = false
    ; ctrlnet = None
    ; hr_upscaler = Sd.Upscaler.default
    ; styles = Sd.Styles.none
    ; regional_prompter
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

let parallel_n
  (type a)
  ~(update : (a list list Or_error.t option -> a list list Or_error.t) -> unit Effect.t)
  n
  ~(f : int -> a list Or_error.t Effect.t)
  =
  let _ = update in
  List.init n ~f:(fun i ->
    let%bind.Effect r = f i in
    let%bind.Effect () =
      update (fun state ->
        match state, r with
        | (None | Some (Error _)), Ok r -> Ok [ r ]
        | None, Error e -> Error e
        | Some (Ok l), Ok r -> Ok (l @ [r])
        | Some (Ok l), Error _ -> Ok l
        | Some (Error e1), Error e2 -> (Error (Error.of_list [e1;e2])))
    in
    Effect.return r)
  |> parallel_all
;;

let image ~params ~prev ~mask ~pool graph =
  let result =
    match%sub prev with
    | None ->
      let query =
        Inc.of_or_error_bonsai
          ~equal:Sd.Txt2img.Query.equal
          ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 2.0))
          (let%arr params = params in
           let%map.Or_error params = Form.value params in
           Parameters.for_txt2img params)
          graph
      in
      Inc.map
        ~equal:Sd.Txt2img.Query.equal
        query
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool in
           fun ~update query ->
             let%map.Effect results =
               parallel_n ~update 4 ~f:(fun i ->
                 dispatcher (function
                   | Error _ as error -> Effect.return error
                   | Ok (host, _) ->
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
             Or_error.filter_ok_at_least_one results)
      |> Inc.collapse_error
    | Some prev ->
      let query =
        Inc.of_or_error_bonsai
          ~equal:Sd.Img2img.Query.equal
          ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 2.0))
          (let%arr params = params in
           let%map.Or_error params = Form.value params in
           Parameters.for_img2img params)
          graph
      in
      Inc.map3
        ~equal_a:Sd.Img2img.Query.equal
        ~equal_b:[%equal: Sd.Base64_image.t list]
        ~equal_c:[%equal: Sd.Base64_image.t option]
        query
        prev
        (Inc.optional mask)
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool in
           fun ~update query prev mask ->
             let%map.Effect results =
               parallel_n ~update 4 ~f:(fun i ->
                 let query =
                   { query with
                     Sd.Img2img.Query.init_images =
                       [ List.nth_exn prev (i % List.length prev) ]
                   ; seed = Int63.(query.Sd.Img2img.Query.seed + of_int i)
                   }
                 in
                 let query = { query with mask } in
                 dispatcher (function
                   | Error _ as e -> Effect.return e
                   | Ok (host, _) ->
                     Sd.Img2img.dispatch
                       ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
                       query))
             in
             Or_error.filter_ok_at_least_one results)
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

let component ~default_size ~pool ~prev ~mask graph =
  let params = Parameters.component ~default_size graph in
  let images, reset =
    Bonsai.with_model_resetter graph ~f:(fun graph ->
      image ~pool ~prev ~mask ~params graph)
  in
  let picked, set_picked = Bonsai.state Int.Set.empty graph in
  let view =
    let%arr { view = form; _ } = params
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
    form ~theme ~reset ~images
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
  images, view, params >>| Form.map_view ~f:(fun _ -> ())
;;
