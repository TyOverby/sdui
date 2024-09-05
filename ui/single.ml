open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module P = Sd.Parameters.Individual

let perform_dispatch ~dispatcher ~api_fun ~query ~update ~sleep ~width ~height i =
  dispatcher (function
    | Error _ as error -> Effect.return error
    | Ok (host, _) ->
      Shared.Effect_utils.while_running
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
          ~equal:[%equal: int * Sd.Txt2img.Query.t]
          ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 2.0))
          (let%arr params = params in
           let%map.Or_error params = Form.value params in
           Parameters.num_images params, Parameters.for_txt2img params)
          graph
      in
      Inc.map
        ~equal:[%equal: int * Sd.Txt2img.Query.t]
        query
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool
           and sleep = Bonsai.Clock.sleep graph in
           fun ~update (num_images, (query : Sd.Txt2img.Query.t)) ->
             Shared.Effect_utils.parallel_n ~update num_images ~f:(fun i ->
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
          ~equal:[%equal: int * Sd.Img2img.Query.t]
          ~time_to_stable:(Bonsai.return (Time_ns.Span.of_sec 2.0))
          (let%arr params = params in
           let%map.Or_error params = Form.value params in
           Parameters.num_images params, Parameters.for_img2img params)
          graph
      in
      Inc.map3
        ~equal_a:[%equal: int * Sd.Img2img.Query.t]
        ~equal_b:[%equal: Sd.Image.t]
        ~equal_c:[%equal: Sd.Image.t option]
        query
        prev
        (Inc.optional mask)
        graph
        ~f:
          (let%arr dispatcher = Lease_pool.dispatcher pool
           and sleep = Bonsai.Clock.sleep graph in
           fun ~update (num_images, query) prev mask ->
             let%map.Effect results =
               Shared.Effect_utils.parallel_n ~update num_images ~f:(fun i ->
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

let component ~direction ~pool ~prev ~mask graph =
  let params = Parameters.component graph in
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
