open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

module Style = [%css stylesheet {|
  body {
    padding: 0;
    margin: 0;
  }
|}]

let host_and_port = Value.return "http://localhost:7860"

let blurry_transparent_background =
  View.Theme.override_constants_for_computation ~f:(fun constants ->
    let make_transparent pct c =
      `Name
        (sprintf
           "color-mix(in oklab, transparent %d%%, %s)"
           pct
           (Css_gen.Color.to_string_css c))
    in
    { constants with
      primary =
        { constants.primary with
          background = make_transparent 20 constants.primary.background
        }
    ; extreme =
        { constants.primary with
          background = make_transparent 50 constants.extreme.background
        }
    })
;;

let component =
  let%sub progress = Progress.state ~host_and_port in
  let%sub { form; form_view; width; height } =
    blurry_transparent_background (Parameters.component ~host_and_port)
  in
  let%sub { ongoing; wrap_request; add_images; view = elements } =
    Gallery.component ~host_and_port ~set_params:(form >>| Form.set)
  in
  let%sub submit_effect =
    let%sub form = Bonsai.yoink form in
    let%arr form = form
    and add_images = add_images
    and host_and_port = host_and_port
    and wrap_request = wrap_request in
    Some
      (wrap_request
         (match%bind.Effect form with
          | Inactive -> Effect.Ignore
          | Active form ->
            (match Form.value form with
             | Error e -> Effect.print_s [%sexp (e : Error.t)]
             | Ok query ->
               (match%bind.Effect Txt2img.dispatch ~host_and_port query with
                | Ok images ->
                  add_images ~params:query ~images:(List.map images ~f:Result.return)
                | Error e -> add_images ~params:query ~images:[ Error e ]))))
  in
  let%sub preview =
    let%sub in_progress_image =
      match%sub progress with
      | Ok { current_image = Some current_image; _ } ->
        Bonsai.pure Option.some current_image
      | _ -> Bonsai.const None
    in
    let%sub in_progress_image =
      Bonsai.most_recent_some ~equal:phys_equal in_progress_image ~f:Fn.id
    in
    match%sub Value.both in_progress_image ongoing with
    | Some current_image, true ->
      let%arr current_image = current_image
      and width = width
      and height = height in
      Some (Base64_image.to_vdom ~width ~height current_image)
    | _ -> Bonsai.const None
  in
  let%arr preview = preview
  and form_view = form_view
  and submit_effect = submit_effect
  and elements = elements in
  let on_submit = Option.value submit_effect ~default:Effect.Ignore in
  Vdom.Node.div [ form_view ~on_submit; elements ~preview ]
;;

let () =
  Bonsai_web.Start.start
    (View.Theme.set_for_app
       (Value.return (Kado.theme ~style:Dark ~version:Bleeding ()))
       component)
;;
