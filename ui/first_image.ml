open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let component graph =
  let state, set_state = Bonsai.state_opt graph in
  let view =
    let%arr set_state = set_state
    and width_form =
      Form.Elements.Number.int
        ~allow_updates_when_focused:`Always
        ~step:8
        ~default:512
        ()
        graph
    and height_form =
      Form.Elements.Number.int
        ~allow_updates_when_focused:`Always
        ~step:8
        ~default:512
        ()
        graph
    and state = state
    and theme = View.Theme.current graph in
    let input =
      let file_upload =
        let process_file file =
          print_endline (Js_of_ocaml.Js.to_string file##._type);
          let file = Shared.File_data_url.create file in
          match%bind.Effect Bonsai_web_ui_file.contents file with
          | Error e -> set_state (Some (Error e))
          | Ok contents ->
            let image = Sd.Image.of_string ~kind:Base64 (Bigstring.to_string contents) in
            set_state (Some (Ok image))
        in
        Vdom.Node.div
          ~attrs:
            [ Shared.File_upload_zone.attr
                ()
                ~mime_types:[ "image/png" ]
                ~on_file_upload:(fun files ->
                  Effect.Many (List.map files ~f:process_file))
            ; {%css| width: 100px; height: 100px; background:red;|}
            ]
          [ Vdom_input_widgets.File_select.single
              ~on_input:(function
                | None -> set_state None
                | Some file ->
                  let file = Shared.File_data_url.create file in
                  (match%bind.Effect Bonsai_web_ui_file.contents file with
                   | Error e -> set_state (Some (Error e))
                   | Ok contents ->
                     let image =
                       Sd.Image.of_string ~kind:Base64 (Bigstring.to_string contents)
                     in
                     set_state (Some (Ok image))))
              ()
          ]
      in
      let empty_image_form =
        let generate_empty_image =
          lazy
            (let width = Form.value_or_default width_form ~default:0
             and height = Form.value_or_default height_form ~default:0 in
             let image_str = Paint.empty_white_image width height in
             let image =
               Sd.Image.of_string ~kind:Base64 (Js_of_ocaml.Js.to_string image_str)
             in
             set_state (Some (Ok image)))
        in
        View.vbox
          [ Form.view width_form
          ; Form.view height_form
          ; View.button theme "empty image" ~on_click:(Effect.lazy_ generate_empty_image)
          ]
      in
      View.hbox
        ~gap:(`Em 5)
        [ file_upload
        ; empty_image_form
        ; View.button theme "text description" ~on_click:(set_state None)
        ]
    in
    match state with
    | None -> input
    | Some (Error e) ->
      View.vbox [ input; Vdom.Node.sexp_for_debugging [%sexp (e : Error.t)] ]
    | Some (Ok img) -> View.vbox [ input; Sd.Image.to_vdom ~width:(Int63.of_int 256) img ]
  in
  let state =
    state
    >>| function
    | None -> None
    | Some (Error _) -> None
    | Some (Ok v) -> Some (Inc.Or_error_or_stale.Fresh v)
  in
  state, view
;;
