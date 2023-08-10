open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let swap_opt_val = function
  | None -> Value.return None
  | Some v -> Value.map v ~f:Option.some
;;

let component ?params progress =
  let params = swap_opt_val params in
  match%sub progress with
  | Error _ -> Bonsai.const None
  | Ok { Progress.current_image; progress; _ } ->
    let%sub image_view =
      let%arr current_image = current_image
      and params = params in
      match current_image, params with
      | Some b64_image, Some { Txt2img.Query.width; height; _ } ->
        Base64_image.to_vdom b64_image ~width ~height
      | Some b64_image, None -> Base64_image.to_vdom b64_image
      | None, _ -> Vdom.Node.p [ Vdom.Node.text "preparing image..." ]
    in
    let%arr progress = progress
    and image_view = image_view
    and params = params in
    let info_view =
      Vdom.Node.p [ Vdom.Node.textf "%.1f%% complete" (progress *. 100.) ]
    in
    let prior_session_view =
      match params with
      | None -> Vdom.Node.p [ Vdom.Node.text "(from prior session)" ]
      | Some _ -> Vdom.Node.none
    in
    Some (View.vbox [ image_view; info_view; prior_session_view ])
;;
