open! Core
open! Bonsai_web
open! Async_kernel
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

let host_and_port = Value.return "http://localhost:7860"

let generate ~on_complete =
  let%sub ongoing =
    Bonsai.state_machine0 ~default_model:0 () ~apply_action:(fun _ctx model -> function
      | `Incr -> model + 1
      | `Decr -> model - 1)
  in
  let%sub { form; form_view; width; height } = Parameters.component ~host_and_port in
  let%sub images, add_images =
    Bonsai.state_machine0
      ~default_model:Int.Map.empty
      ~apply_action:(fun _ctx map images ->
        List.fold ~init:map images ~f:(fun acc image ->
          Map.add_exn acc ~key:(-Map.length acc) ~data:image))
      ()
  in
  let%sub images =
    Bonsai.assoc
      (module Int)
      images
      ~f:(fun _key data ->
        match%arr data with
        | Ok image -> Base64_image.to_vdom image
        | Error e -> Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e))
  in
  let%arr form = form
  and images = images
  and add_images = add_images
  and host_and_port = host_and_port
  and width = width
  and height = height
  and on_complete = on_complete
  and form_view = form_view
  and ongoing, inject_ongoing = ongoing in
  let submit_effect =
    match Form.value form with
    | Error _ -> None
    | Ok query ->
      Some
        (let%bind.Effect () = inject_ongoing `Incr in
         let%bind.Effect () = on_complete in
         let%bind.Effect () =
           match%bind.Effect Txt2img.dispatch ~host_and_port query with
           | Ok images -> add_images (List.map images ~f:Result.return)
           | Error e -> add_images [ Error e ]
         in
         inject_ongoing `Decr)
  in
  let submit_button =
    let attrs =
      match submit_effect with
      | None -> [ Vdom.Attr.disabled ]
      | Some effect -> [ Vdom.Attr.on_click (fun _ -> effect) ]
    in
    Vdom.Node.button ~attrs [ Vdom.Node.text "submit" ]
  in
  let view ~preview =
    let images =
      match preview with
      | None -> images
      | Some preview -> Map.set images ~key:(-Map.length images) ~data:preview
    in
    Vdom_node_with_map_children.make ~tag:"div" images
  in
  form, form_view, submit_button, submit_effect, view, width, height, ongoing
;;

let component =
  let%sub progress = Progress.state ~host_and_port in
  let%sub ( _form
          , form_view
          , _submit_button
          , submit_effect
          , elements
          , width
          , height
          , ongoing )
    =
    generate ~on_complete:(Value.return Effect.Ignore)
  in
  let%sub preview =
    let%sub in_progress_image =
      match%sub progress with
      | Ok { current_image = Some current_image; _ } ->
        return (current_image >>| Option.some)
      | _ -> Bonsai.const None
    in
    let%sub in_progress_image =
      Bonsai.most_recent_some ~equal:phys_equal in_progress_image ~f:Fn.id
    in
    match%sub Value.both in_progress_image ongoing with
    | Some current_image, _ongoing when _ongoing > 0 ->
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
       (Value.return (Kado.theme ~style:Light ~version:Bleeding ()))
       component)
;;
