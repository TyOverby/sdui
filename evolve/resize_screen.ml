open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let component (local_ graph) =
  let padding_left =
    Sd.Custom_form_elements.int_form
      ~title:"left"
      ~step:1
      ~default:(Int63.of_int 0)
      ~validate_or_correct:(fun s ->
        match Int63.of_string_opt s with
        | None -> Error (Int63.of_int 0)
        | Some i -> Ok i)
      ~length:(`Em 5)
      ~min:(Int63.of_int 0)
      ~max:(Int63.of_int 1024)
      graph
  in
  let padding_right =
    Sd.Custom_form_elements.int_form
      ~title:"right"
      ~step:1
      ~default:(Int63.of_int 0)
      ~validate_or_correct:(fun s ->
        match Int63.of_string_opt s with
        | None -> Error (Int63.of_int 0)
        | Some i -> Ok i)
      ~length:(`Em 5)
      ~min:(Int63.of_int 0)
      ~max:(Int63.of_int 1024)
      graph
  in
  let padding_top =
    Sd.Custom_form_elements.int_form
      ~title:"top"
      ~step:1
      ~default:(Int63.of_int 0)
      ~validate_or_correct:(fun s ->
        match Int63.of_string_opt s with
        | None -> Error (Int63.of_int 0)
        | Some i -> Ok i)
      ~length:(`Em 5)
      ~min:(Int63.of_int 0)
      ~max:(Int63.of_int 1024)
      graph
  in
  let padding_bottom =
    Sd.Custom_form_elements.int_form
      ~title:"bottom"
      ~step:1
      ~default:(Int63.of_int 0)
      ~validate_or_correct:(fun s ->
        match Int63.of_string_opt s with
        | None -> Error (Int63.of_int 0)
        | Some i -> Ok i)
      ~length:(`Em 5)
      ~min:(Int63.of_int 0)
      ~max:(Int63.of_int 1024)
      graph
  in
  let%arr padding_left
  and padding_right
  and padding_top
  and padding_bottom
  and theme = View.Theme.current graph in
  let left =
    Int63.to_int_trunc (Form.value_or_default ~default:(Int63.of_int 0) padding_left)
  in
  let right =
    Int63.to_int_trunc (Form.value_or_default ~default:(Int63.of_int 0) padding_right)
  in
  let top =
    Int63.to_int_trunc (Form.value_or_default ~default:(Int63.of_int 0) padding_top)
  in
  let bottom =
    Int63.to_int_trunc (Form.value_or_default ~default:(Int63.of_int 0) padding_bottom)
  in
  let effect image =
    Effect.lazy_
      (lazy
        (let%bind.Effect image =
           Sd.Load_image_effect.load_image (Sd.Image.to_string image)
         in
         let%bind.Effect canvas_image =
           Sd.Load_image_effect.load_image_generic
             (Canvas2d.Image.add_padding
                ~left
                ~right
                ~top
                ~bottom
                image
                ~fill_color:"white")
         in
         Effect.return
           (Sd.Image.of_string ~kind:Base64 (Canvas2d.Image.to_data_url canvas_image))))
  in
  let view ~get_images ~set_result =
    View.vbox
      [ Form.view padding_top
      ; Form.view padding_right
      ; Form.view padding_bottom
      ; Form.view padding_left
      ; View.button
          theme
          "resize"
          ~on_click:
            (let%bind.Effect input = get_images in
             let%bind.Effect output = effect input.Sd_chain.Paint.Images.image in
             set_result
               ~vertical_padding:(top + bottom)
               ~horizontal_padding:(left + right)
               output)
      ]
  in
  view, effect
;;
