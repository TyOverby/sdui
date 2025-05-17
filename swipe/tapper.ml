open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Feather = Feather_icon

module Style =
  [%css
  stylesheet
    {|
    .container {
      display: grid;
      user-select:none;
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
      animation: bg-pulse 0.2s ease-in-out;
    }

    @keyframes bg-pulse {
      0%   { opacity: 0.0; }
      50%  { opacity: 1.0; } 
      100% { opacity: 0.0; }
    }
|}]

let component ~on_remove ~refine ~reimagine ~upscale (local_ graph) =
  let modify_effect effect =
    let clicked, set_clicked = Bonsai.state false graph in
    let%arr clicked
    and set_clicked
    and effect
    and sleep = Bonsai.Clock.sleep graph in
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
  let%arr on_remove and refine and reimagine and upscale in
  fun img ->
    let size = `Vw (Percent.of_mult 0.3) in
    {%html|
    <div %{Style.container}>
      %{img}
      <div %{Style.overlay}> 
        <div style="display:flex; flex-direction: column; flex-grow:1;">
            <div %{Style.close} on_click=%{fun _ -> Effect.Many [ Effect.Stop_propagation ]}> 
              %{Feather.svg ~size X} 
            </div>
            <div %{Style.delete} on_click=%{fun _ -> on_remove }> %{Feather.svg ~size Trash_2} </div>
        </div>
        <div style="display:flex; flex-direction: column; flex-grow:1;">
            <div %{Style.upscale} %{upscale}> %{Feather.svg ~size Maximize_2} </div>
            <div %{Style.refine} %{refine}> %{Feather.svg ~size Chevrons_up} </div>
            <div %{Style.reimagine} %{reimagine}> %{Feather.svg ~size Refresh_cw} </div>
        </div>
      </div>
    </div>|}
;;
