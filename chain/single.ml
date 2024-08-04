open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module P = Sd.Parameters.Individual

let parallelism = 4

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
          direction:[ `Vertical | `Horizontal ]
          -> theme:View.Theme.t
          -> reset:unit Effect.t
          -> Vdom.Node.t

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
          fun ~direction ~theme ~reset ->
            let vbox, hbox =
              match direction with
              | `Horizontal -> (fun a -> View.vbox a), fun a -> View.hbox a
              | `Vertical -> (fun a -> View.hbox a), fun a -> View.vbox a
            in
            Vdom.Node.div
              [ vbox
                  [ hbox
                      [ View.button theme "reset" ~on_click:reset
                      ; vbox [ Form.view width; Form.view height ]
                      ; vbox [ Form.view steps; Form.view cfg ]
                      ; vbox [ Form.view denoise; Form.view seed ]
                      ; Form.view ratios
                      ; Form.view pos_prompt
                      ; Form.view neg_prompt
                      ]
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
  ~(update : (a Or_error.t Int.Map.t option -> a Or_error.t Int.Map.t) -> unit Effect.t)
  n
  ~(f : int -> a Or_error.t Effect.t)
  =
  List.init n ~f:(fun i ->
    let%bind.Effect r = f i in
    let%bind.Effect () =
      update (fun state ->
        match state with
        | None -> Int.Map.singleton i r
        | Some map -> Map.set map ~key:i ~data:r)
    in
    Effect.return r)
  |> parallel_all
  |> Effect.map ~f:(fun all -> List.mapi all ~f:Tuple2.create |> Int.Map.of_alist_exn)
;;

let while_running eff ~do_this =
  Ui_effect.Expert.of_fun ~f:(fun ~callback ->
    let finished = ref false in
    Ui_effect.Expert.eval eff ~f:(fun result ->
      finished := true;
      callback result);
    let rec loop () =
      if !finished then () else Ui_effect.Expert.eval do_this ~f:(fun () -> loop ())
    in
    loop ())
;;

let perform_dispatch ~dispatcher ~api_fun ~query ~update ~sleep ~width ~height i =
  dispatcher (function
    | Error _ as error -> Effect.return error
    | Ok (host, _) ->
      while_running
        ~do_this:
          (match%bind.Effect
             Sd.Progress.dispatch ((host : Sd.Hosts.Host.t) :> string)
           with
           | Ok { current_image = Some current_image; _ } ->
             update (fun map ->
               Map.set
                 (Option.value map ~default:Int.Map.empty)
                 ~key:i
                 ~data:(Ok (Sd.Image.with_size current_image ~width ~height)))
           | _ -> sleep (Time_ns.Span.of_sec 0.5))
        (match%map.Effect
           api_fun ~host_and_port:((host : Sd.Hosts.Host.t) :> string) query
         with
         | Ok [ (image, _) ] -> Ok image
         | Error e -> Error e
         | Ok _ -> Error (Error.of_string "unexpected number of images")))
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
          (let%arr dispatcher = Lease_pool.dispatcher pool
           and sleep = Bonsai.Clock.sleep graph in
           fun ~update (query : Sd.Txt2img.Query.t) ->
             parallel_n ~update parallelism ~f:(fun i ->
               let query = { query with seed = Int63.(query.seed + of_int i) } in
               perform_dispatch
                 ~width:query.width
                 ~height:query.height
                 ~api_fun:Sd.Txt2img.dispatch
                 ~dispatcher
                 ~query
                 ~update
                 ~sleep
                 i))
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
        ~equal_b:[%equal: Sd.Image.t]
        ~equal_c:[%equal: Sd.Image.t option]
        query
        prev
        (Inc.optional mask)
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool
           and sleep = Bonsai.Clock.sleep graph in
           fun ~update query prev mask ->
             let%map.Effect results =
               parallel_n ~update parallelism ~f:(fun i ->
                 let query =
                   { query with
                     Sd.Img2img.Query.init_images = [ prev ]
                   ; seed = Int63.(query.Sd.Img2img.Query.seed + of_int i)
                   ; mask
                   }
                 in
                 perform_dispatch
                   ~width:query.width
                   ~height:query.height
                   ~api_fun:Sd.Img2img.dispatch
                   ~dispatcher
                   ~query
                   ~update
                   ~sleep
                   i)
             in
             results)
  in
  result
  |> Inc.map_pure ~f:(fun images -> images |> Map.data |> Or_error.filter_ok_at_least_one)
  |> Inc.collapse_error
;;

type t =
  { image : Sd.Image.t Inc.t
  ; gallery_view : Vdom.Node.t Bonsai.t
  ; form_view : Vdom.Node.t Bonsai.t
  ; form : (Parameters.t, unit) Form.t Bonsai.t
  }

let component ~direction ~default_size ~pool ~prev ~mask graph =
  let params = Parameters.component ~default_size graph in
  let images, reset =
    Bonsai.with_model_resetter graph ~f:(fun graph ->
      image ~pool ~prev ~mask ~params graph)
  in
  let picked, set_picked = Bonsai.state_opt graph in
  let form_view =
    let%arr { view = form; _ } = params
    and theme = View.Theme.current graph
    and reset = reset in
    form ~theme ~reset ~direction
  in
  let gallery_view =
    let%arr images = images
    and picked = picked
    and set_picked = set_picked in
    let images =
      let base64_to_vdom i img =
        let checked = [%equal: int option] picked (Some i) in
        let toggle = set_picked (if checked then None else Some i) in
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
               match Sd.Image.size img with
               | Some (w, _) ->
                 Float.to_string
                   (Int63.to_float w /. Js_of_ocaml.Js.Unsafe.global##.devicePixelRatio)
                 ^ "px"
               | None -> "512px"
             in
             Sd.Image.to_vdom
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
    images
  in
  let picked =
    Inc.of_bonsai
      ~equal:[%equal: int option]
      picked
      ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 1.0))
      graph
  in
  let image =
    let error =
      Inc.Or_error_or_stale.Error (Error.of_string "you must pick an image to proceed")
    in
    Inc.map2_pure images picked ~f:(fun images ->
        function
        | None ->
          Inc.Or_error_or_stale.Error
            (Error.of_string "you must pick an image to proceed")
        | Some picked ->
          (match List.nth images (picked mod List.length images) with
           | None -> error
           | Some image -> Inc.Or_error_or_stale.Fresh image))
  in
  { image = image >>| Inc.Or_error_or_stale.join
  ; form = params >>| Form.map_view ~f:(fun _ -> ())
  ; form_view
  ; gallery_view
  }
;;
