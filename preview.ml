open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component ~ongoing progress =
  match%sub ongoing with
  | None -> Bonsai.const None
  | Some { Txt2img.Query.width; height; _ } ->
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
;;
