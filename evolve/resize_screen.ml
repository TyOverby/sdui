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
  let keep_area =
    Sd.Custom_form_elements.bool_form ~title:"keep area" ~default:true graph
  in
  let%arr padding_left
  and padding_right
  and padding_top
  and padding_bottom
  and keep_area
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
         let old_width = Canvas2d.Image.width image in
         let old_height = Canvas2d.Image.width image in
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
         let image =
           Sd.Image.of_string ~kind:Base64 (Canvas2d.Image.to_data_url canvas_image)
         in
         Effect.return
           ( ~new_width:(Canvas2d.Image.width canvas_image)
           , ~new_height:(Canvas2d.Image.height canvas_image)
           , ~old_width
           , ~old_height
           , ~image )))
  in
  let nearest_multiple_of_8 i = if i % 8 = 0 then i else i + (8 - (i % 8)) in
  let view ~get_images ~set_result =
    View.vbox
      [ Form.view padding_top
      ; Form.view padding_right
      ; Form.view padding_bottom
      ; Form.view padding_left
      ; Form.view keep_area
      ; View.button
          theme
          "add padding"
          ~on_click:
            (let%bind.Effect input = get_images in
             let%bind.Effect ( ~new_width
                             , ~new_height
                             , ~old_width
                             , ~old_height
                             , ~image:output )
               =
               effect input.Sd_chain.Paint.Images.image
             in
             let new_width, new_height =
               if Form.value_or_default keep_area ~default:true
               then (
                 let old_area = Float.of_int (old_width * old_height) in
                 let new_area = Float.of_int (new_width * new_height) in
                 let scale = Float.sqrt (old_area /. new_area) in
                 let new_width = Int.of_float (Float.of_int new_width *. scale) in
                 let new_height = Int.of_float (Float.of_int new_height *. scale) in
                 nearest_multiple_of_8 new_width, nearest_multiple_of_8 new_height)
               else new_width, new_height
             in
             set_result
               ~new_width:(Int63.of_int new_width)
               ~new_height:(Int63.of_int new_height)
               output)
      ]
  in
  view, effect
;;
