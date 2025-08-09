open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool
module P = Sd.Parameters.Individual
module Toplayer = Bonsai_web_ui_toplayer
module Visibility = Bonsai_web_ui_visibility

module Style =
  [%css
  stylesheet
    {|

    .image-container  {
      display:flex; 
      flex-direction: column;
    }

    .image {
      width: 100vw;
      height:auto;
    }

    :root:contains(.me),
    :root:contains(.me) * {
      touch-action: pan-y;
    }

    .me {}
|}]

let modify_image ~modify_parameters ~image ~parameters ~dispatcher =
  let%bind.Effect parameters = modify_parameters ~parameters ~image in
  let parameters = { parameters with Sd_chain.Parameters.seed = Int63.of_int (-1) } in
  let pred =
    Option.map parameters.specific_model ~f:(fun specific_model _host current_model ->
      Sd.Hosts.Current_model.equal current_model specific_model)
  in
  dispatcher ?info:None ?pred (fun get_host ->
    match get_host with
    | Error e -> Effect.return (Error e)
    | Ok (host, _current_model) ->
      (match%map.Effect
         Sd.Img2img.dispatch
           ~host_and_port:((host : Sd.Hosts.Host.t) :> string)
           { (Sd_chain.Parameters.for_img2img parameters) with image }
       with
       | Ok (img, _) -> Ok (img, parameters)
       | Error e -> Error e))
;;

let tapper ~image ~parameters ~inject ~lease_pool ~on_remove graph =
  let image_modifier ~f =
    let%arr image
    and parameters
    and inject
    and dispatcher = Lease_pool.dispatcher lease_pool in
    match%bind.Effect
      modify_image ~modify_parameters:f ~image ~parameters ~dispatcher
    with
    | Ok (image, parameters) -> inject (`Add (Ok image, parameters))
    | Error e -> inject (`Add (Error e, parameters))
  in
  Tapper.component
    ~on_remove
    ~refine:
      (image_modifier ~f:(fun ~parameters ~image:_ ->
         Effect.return
           { parameters with
             cfg = Int63.of_int 10
           ; denoise = Int63.of_int 55
           ; steps = Int63.of_int 50
           }))
    ~reimagine:
      (image_modifier ~f:(fun ~parameters ~image:_ ->
         Effect.return
           { parameters with
             cfg = Int63.of_int 10
           ; denoise = Int63.of_int 70
           ; steps = Int63.of_int 50
           }))
    ~upscale:
      (image_modifier ~f:(fun ~parameters ~image:_ ->
         Effect.return
           { parameters with
             cfg = Int63.of_int 7
           ; denoise = Int63.of_int 30
           ; steps = Int63.of_int 50
           ; width = Int63.(parameters.width * of_int 2)
           ; height = Int63.(parameters.height * of_int 2)
           }))
    graph
;;

let component (local_ graph) =
  let lease_pool, hosts_view, queue_view, _kill_all = Sd_chain.hosts_and_queue graph in
  let state, inject =
    Bonsai.state_machine
      ~default_model:(0, Int.Map.empty)
      ~apply_action:(fun _ctx (id, map) action ->
        match action with
        | `Add image_and_params -> id + 1, Map.set map ~key:id ~data:image_and_params
        | `Remove target -> id, Map.remove map target)
      graph
  in
  let%sub current_id, images = state in
  let max_id_seen, inject_id_seen =
    Bonsai.state_machine ~default_model:0 ~apply_action:(fun _ctx -> Int.max) graph
  in
  let mostly_caught_up =
    let%arr current_id and max_id_seen in
    current_id - max_id_seen < 10
  in
  let txt2img =
    Txt2img_screen.component
      ~lease_pool
      ~inject:
        (let%arr inject in
         fun image params -> inject (`Add (image, params)))
      graph
  in
  let%sub ~view, ~generate_action = txt2img in
  let images =
    Bonsai.assoc
      (module Int)
      images
      ~f:(fun id data graph ->
        let on_remove =
          let%arr inject and id in
          inject (`Remove id)
        in
        match%sub data with
        | Error e, _ ->
          Bonsai.Edge.lifecycle
            ~on_activate:
              (let%arr sleep = Bonsai.Clock.sleep graph
               and on_remove in
               let%bind.Effect () = sleep (Time_ns.Span.of_sec 1.0) in
               on_remove)
            graph;
          let%arr e in
          Vdom.Node.sexp_for_debugging (Error.sexp_of_t e)
        | Ok image, parameters ->
          let tapper = tapper ~image ~parameters ~inject ~lease_pool ~on_remove graph in
          let%arr image and tapper and id and inject_id_seen in
          tapper
            (Sd.Image.to_vdom
               ~attrs:
                 [ Style.image
                 ; Visibility_attr.create ~on_become_visible:(inject_id_seen id)
                 ]
               image))
      graph
  in
  let last_duration, set_last_duration = Bonsai.state (Time_ns.Span.of_sec 5.0) graph in
  let%sub () =
    match%sub mostly_caught_up, Lease_pool.all lease_pool >>| Map.length with
    | false, _ | _, 0 -> Bonsai.return ()
    | _, n ->
      let generate_action =
        let%arr generate_action
        and set_last_duration
        and num_queued = Lease_pool.queued_jobs lease_pool >>| List.length in
        if num_queued >= 1
        then Effect.Ignore
        else (
          match%bind.Effect generate_action with
          | None -> Effect.Ignore
          | Some duration -> set_last_duration duration)
      in
      Bonsai.Clock.every
        ~trigger_on_activate:true
        ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
        (let%arr n and last_duration in
         Time_ns.Span.(last_duration / Float.of_int n))
        generate_action
        graph;
      Bonsai.return ()
  in
  let edit_modal =
    let content ~close _graph =
      let%arr queue_view and hosts_view and view and close in
      let host_monitor =
        View.hbox
          ~cross_axis_alignment:Center
          [ queue_view
          ; hosts_view
          ; {%html| <div style="margin-left:auto;" > %{Feather.svg X} </div> |}
          ]
      in
      View.vbox
        ~attrs:[ {%css| font-size:16px; |} ]
        [ view ~host_monitor
        ; {%html| <div style="font-size: 2em; border:1px solid white; margin:0.5em 0; text-align:center; border-radius:5px;" on_click=%{fun _ -> close }> Done </div>|}
        ]
    in
    Toplayer.Modal.create ~overflow_auto_wrapper:(Bonsai.return true) ~content graph
  in
  let open_edit_modal =
    let%arr open_edit_modal = edit_modal.open_ in
    let style =
      {%css| display:flex; justify-content: center; align-content:center; border-radius:20px; background:black; |}
    in
    {%html| <div %{style} style="position:absolute; top:0; right:0" on_click=%{fun _ -> open_edit_modal}> %{Feather.svg ~size:(`Px 75) Edit} </div> |}
  in
  let%arr images
  and open_edit_modal
  and last_duration
  and num_queued = Lease_pool.queued_jobs lease_pool >>| List.length
  and num_active = Lease_pool.leased_out lease_pool >>| Map.length in
  let content = Vdom.Node.Map_children.div ~attrs:[ Style.image_container ] images in
  Snips.body
    ~attr:{%css| scrollbar-width: none; |}
    (View.vbox
       ~attrs:[ {%css| overflow:clip; |} ]
       [ content
       ; {%html|<div>Active: %{num_active #Int}</div>|}
       ; {%html|<div>Queued: %{num_queued #Int}</div>|}
       ; {%html|<div>Last duration: #{Time_ns.Span.to_string_hum last_duration}</div>|}
       ; open_edit_modal
       ])
  |> Snips.render
;;
