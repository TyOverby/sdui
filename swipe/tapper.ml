open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Feather = Feather_icon
module Resize_to_fit = Bonsai_web_ui_element_size_hooks.Resize_to_fit
module Size_tracker = Bonsai_web_ui_element_size_hooks.Size_tracker

module Style =
  [%css
  stylesheet
    {|
    .container {
      display: grid;
      user-select:none;
      transition: 0.1s ease-in-out max-height;
      overflow:clip;
    }

    .container > * {
      grid-area: 1 / 1;
    }

    .overlay {
      display:flex;
      opacity: 1.0;
      flex-direction: row;
      padding: 5px;
    }

    .close, .delete, .upscale, .refine, .reimagine {
      flex-grow:1;
      display: flex;
      align-items: center;
      justify-content: center;

      opacity:0.0;

      margin: 5px;
      border: 1px solid white;
      background: rgba(255,255,255, 0.5);
      /*backdrop-filter: blur(10px);*/
      border-radius: 10px;
      color: black;
    }

    .upscale.clicked, .refine.clicked, .reimagine.clicked {
      animation: bg-pulse 0.1s ease-in-out;
    }

    @keyframes bg-pulse {
      0%   { opacity: 0.0; }
      50%  { opacity: 1.0; } 
      100% { opacity: 0.0; }
    }
|}]

let component ~on_remove ~refine ~reimagine ~upscale (local_ graph) =
  let sleep = Bonsai.Clock.sleep graph in
  let modify_effect effect =
    let clicked, set_clicked = Bonsai.state false graph in
    let%arr clicked and set_clicked and effect and sleep in
    Vdom.Attr.many
      [ (if clicked then Style.clicked else Vdom.Attr.empty)
      ; Vdom.Attr.on_click (fun _ ->
          Effect.Many
            [ effect
            ; set_clicked true
            ; (let%bind.Effect () = sleep (Time_ns.Span.of_sec 0.2) in
               set_clicked false)
            ])
      ]
  in
  let refine = modify_effect refine in
  let reimagine = modify_effect reimagine in
  let upscale = modify_effect upscale in
  let height, set_height = Bonsai.state_opt graph in
  let restricted_height, set_restricted_height = Bonsai.state_opt graph in
  let on_remove =
    let%arr on_remove
    and height
    and set_restricted_height
    and sleep
    and wait_frame = Bonsai.Edge.wait_after_display graph in
    match height with
    | Some height ->
      let%bind.Effect () = set_restricted_height (Some height) in
      let%bind.Effect () = wait_frame in
      let%bind.Effect () = set_restricted_height (Some 0.0) in
      let%bind.Effect () = sleep (Time_ns.Span.of_sec 1000.0) in
      on_remove
    | None -> on_remove
  in
  let%arr on_remove
  and restricted_height
  and refine
  and reimagine
  and upscale
  and set_height in
  fun img ->
    let size = `Px 20 in
    let extra_attrs =
      [ Resize_to_fit.attr ~behavior:Grow_or_shrink_to_match_parent_size ()
      ; {%css| transform-origin: center !important;|}
      ]
    in
    let size_tracker =
      Size_tracker.on_change (fun { border_box = { height; _ }; _ } ->
        set_height (Some height))
    in
    let max_height =
      match restricted_height with
      | None -> None
      | Some restricted_height ->
        Some (Vdom.Attr.style (Css_gen.max_height (`Px_float restricted_height)))
    in
    {%html|
    <div %{Style.container} %{size_tracker} ?{max_height}>
      %{img}
      <div %{Style.overlay}> 
        <div style="display:flex; flex-direction: column; flex-grow:1;">
            <div %{Style.close} on_click=%{fun _ -> Effect.Many [ Effect.Stop_propagation ]}> 
              %{Feather.svg ~extra_attrs ~size X} 
            </div>
            <div %{Style.delete} on_click=%{fun _ -> on_remove }> %{Feather.svg ~extra_attrs ~size Trash_2} </div>
        </div>
        <div style="display:flex; flex-direction: column; flex-grow:1;">
            <div %{Style.upscale} %{upscale}> %{Feather.svg ~extra_attrs ~size Maximize_2} </div>
            <div %{Style.refine} %{refine}> %{Feather.svg ~extra_attrs ~size Chevrons_up} </div>
            <div %{Style.reimagine} %{reimagine}> %{Feather.svg ~extra_attrs ~size Refresh_cw} </div>
        </div>
      </div>
    </div>|}
;;
