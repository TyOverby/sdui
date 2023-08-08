open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component ~width ~height ~ongoing progress =
  let view ~reset =
    match%sub ongoing with
    | false -> Bonsai.const None
    | true ->
      let%sub () = Bonsai.Edge.lifecycle ~on_deactivate:reset () in
      let%sub width, height = Bonsai.freeze (Value.both width height) in
      (match%sub progress with
       | Error _ -> Bonsai.const None
       | Ok { Progress.current_image; progress; _ } ->
         let%arr current_image = current_image
         and width = width
         and height = height
         and progress = progress in
         let image_view =
           match current_image with
           | Some b64_image -> Base64_image.to_vdom b64_image ~width ~height
           | None -> Vdom.Node.p [ Vdom.Node.text "preparing image..." ]
         in
         let info_view =
           Vdom.Node.p [ Vdom.Node.textf "%.1f%% complete" (progress *. 100.) ]
         in
         Some (View.vbox [ image_view; info_view ]))
  in
  Bonsai.with_model_resetter' view
;;
