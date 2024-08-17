open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

type paint_view =
  { color_picker : Vdom.Node.t
  ; pen_size_slider : Vdom.Node.t
  ; layer_panel : Vdom.Node.t
  ; forward_button : Vdom.Node.t
  ; clear_button : Vdom.Node.t
  ; widget : Vdom.Node.t
  }

let paint_impl ~prev graph =
  match%sub prev with
  | Inc.Or_error_or_stale.Fresh prev_for_paint | Stale prev_for_paint ->
    let { Paint.images = t
        ; color_picker
        ; pen_size_slider
        ; layer_panel
        ; forward_button
        ; clear_button
        ; widget
        }
      =
      Paint.component ~prev:prev_for_paint graph
    in
    let%arr t = t
    and color_picker = color_picker
    and pen_size_slider = pen_size_slider
    and layer_panel = layer_panel
    and forward_button = forward_button
    and clear_button = clear_button
    and widget = widget in
    ( t
    , Some
        { color_picker
        ; pen_size_slider
        ; layer_panel
        ; forward_button
        ; clear_button
        ; widget
        } )
  | Error e ->
    let%arr e = e in
    Inc.Or_error_or_stale.Error e, None
  | Not_computed -> Bonsai.return (Inc.Or_error_or_stale.Not_computed, None)
;;

let img2img_impl ~direction ~pool ~prev ~mask ~prev_params graph =
  let t = Single.component ~direction ~pool ~prev ~mask ~default_size:512 graph in
  let%sub () =
    match%sub prev_params with
    | Ok prev_params ->
      let _ = Form.Dynamic.with_default prev_params t.form graph in
      return ()
    | Error _ -> return ()
  in
  t
;;

let sketch_impl ~prev ~prev_params graph =
  let%sub image, view = paint_impl ~prev graph in
  let%arr t = image
  and view = view
  and params = prev_params in
  let image, mask =
    Inc.Or_error_or_stale.(unzip (map t ~f:(fun { image; mask } -> image, mask)))
  in
  let mask =
    match mask with
    | Fresh None -> None
    | Fresh (Some a) -> Some (Inc.Or_error_or_stale.Fresh a)
    | Stale None -> None
    | Stale (Some a) -> Some (Stale a)
    | Error e -> Some (Error e)
    | Not_computed -> None
  in
  image, mask, view, params
;;

let component
  ~(index : int Bonsai.t)
  ~(pool : (Sd.Hosts.Host.t, 'a, 'b) Lease_pool.t)
  ~(prev : Sd.Image.t Inc.Or_error_or_stale.t Bonsai.t)
  ~(prev_params : (Single.Parameters.t, Error.t) result Bonsai.t)
  ~(reset : unit Ui_effect.t Bonsai.t)
  graph
  =
  let image, view, params =
    let%sub image, mask, sketch_view, _ = sketch_impl ~prev ~prev_params graph in
    let { Single.image; gallery_view; form_view; form } =
      img2img_impl
        ~direction:`Vertical
        ~pool
        ~prev:(image >>| Option.some)
        ~mask
        ~prev_params
        graph
    in
    let view =
      match%sub sketch_view with
      | None -> return Vdom.Node.none
      | Some
          { color_picker
          ; pen_size_slider
          ; layer_panel
          ; forward_button
          ; clear_button
          ; widget
          } ->
        Workspace.make
          ~color_picker
          ~pen_size_slider
          ~layer_panel
          ~forward_button
          ~clear_button
          ~widget
          ~gallery_view
          ~form_view
          graph
    in
    image, view, form >>| Form.value
  in
  let view =
    let%arr view = view
    and theme = View.Theme.current graph
    and reset = reset
    and index = index in
    let header =
      Vdom.Node.h2
        ~attrs:[ {%css| margin:0; |} ]
        [ Vdom.Node.textf "#%d " index; View.button theme "x" ~on_click:reset ]
    in
    Vdom.Node.div [ header; view ]
  in
  image, view, params
;;

let do_txt2img ~prev ~prev_params ~index ~pool ~reset ~recurse graph =
  let image, view, params = component ~index ~pool ~prev ~reset ~prev_params graph in
  let next = recurse index image params graph in
  let%arr view = view
  and image, view2 = next in
  image, view :: view2
;;

let _fix4 a b c d ~f graph =
  let abcd = Bonsai.map4 a b c d ~f:(fun a b c d -> a, b, c, d) in
  Bonsai.fix abcd graph ~f:(fun ~recurse abcd graph ->
    let%sub a, b, c, d = abcd in
    let recurse a b c d graph =
      let abcd = Bonsai.map4 a b c d ~f:(fun a b c d -> a, b, c, d) in
      recurse abcd graph
    in
    f ~recurse a b c d graph)
;;

let fix3 a b c ~f graph =
  let abc = Bonsai.map3 a b c ~f:(fun a b c -> a, b, c) in
  Bonsai.fix abc graph ~f:(fun ~recurse abc graph ->
    let%sub a, b, c = abc in
    let recurse a b c graph =
      let abc = Bonsai.map3 a b c ~f:(fun a b c -> a, b, c) in
      recurse abc graph
    in
    f ~recurse a b c graph)
;;

let component ~pool ~index ~prev ~prev_params graph =
  fix3 index prev prev_params graph ~f:(fun ~recurse index prev prev_params graph ->
    let index = index >>| ( + ) 1 in
    let scope, incr_scope =
      Bonsai.state_machine0
        ~default_model:0
        ~apply_action:(fun _ model () -> model + 1)
        graph
    in
    Bonsai.scope_model
      (module Int)
      graph
      ~on:scope
      ~for_:(fun graph ->
        Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
          let reset =
            Bonsai.map2 reset incr_scope ~f:(fun reset incr_scope ->
              Effect.Many [ reset; incr_scope () ])
          in
          match%sub prev with
          | Inc.Or_error_or_stale.Fresh _ | Stale _ ->
            do_txt2img ~prev ~prev_params ~index ~pool ~reset ~recurse graph
          | _ ->
            let%arr prev = prev in
            prev, [])))
;;

let create file =
  let open Core in
  let open Async_kernel in
  let open Js_of_ocaml in
  let read =
    Ui_effect.of_sync_fun (fun on_progress ->
      let file_reader = new%js File.fileReader in
      let result = Ivar.create () in
      let result =
        Bonsai_web.Effect.of_deferred_fun
          (fun () ->
            let call_on_progress ev =
              if Js.to_bool ev##.lengthComputable
              then
                on_progress
                  { Bonsai_web_ui_file.Progress.loaded = ev##.loaded; total = ev##.total }
                |> Ui_effect.Expert.handle
            in
            file_reader##.onprogress
            := Dom.handler (fun ev ->
                 call_on_progress ev;
                 Js._true);
            file_reader##.onerror
            := Dom.handler (fun _ev ->
                 let error =
                   Error.create_s
                     [%message
                       "Error reading file"
                         ~code:(file_reader##.error##.code : int)
                         ~message:
                           (Js.to_string
                              (Js.Unsafe.get file_reader##.error (Js.string "message")))]
                 in
                 Ivar.fill_if_empty
                   result
                   (Error (Bonsai_web_ui_file.Read_error.Error error));
                 Js._true);
            file_reader##.onload
            := Dom.handler (fun ev ->
                 call_on_progress ev;
                 (match
                    file_reader##.result |> File.CoerceTo.string |> Js.Opt.to_option
                  with
                  | None ->
                    raise_s
                      [%message "BUG: could not coerce fileReader result to arrayBuffer"]
                  | Some string ->
                    let contents = Bigstring.of_string (Js.to_string string) in
                    Ivar.fill_if_empty result (Ok contents));
                 Js._true);
            file_reader##readAsDataURL file;
            Ivar.read result)
          ()
      in
      let abort = Ui_effect.of_sync_fun (fun () -> file_reader##abort) () in
      { Bonsai_web_ui_file.Expert.result; abort })
  in
  Bonsai_web_ui_file.Expert.create ~read ~filename:(File.filename file |> Js.to_string)
;;

let component ~pool graph =
  let first_image, first_image_view =
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
          Vdom_input_widgets.File_select.single
            ~on_input:(function
              | None -> set_state None
              | Some file ->
                let file = create file in
                (match%bind.Effect Bonsai_web_ui_file.contents file with
                 | Error e -> set_state (Some (Error e))
                 | Ok contents ->
                   let image =
                     Sd.Image.of_string ~kind:Base64 (Bigstring.to_string contents)
                   in
                   set_state (Some (Ok image))))
            ()
        in
        let empty_image_form =
          View.hbox
            [ Form.view width_form
            ; Form.view height_form
            ; View.button
                theme
                "empty image"
                ~on_click:
                  (Effect.lazy_
                     (lazy
                       (let image =
                          Paint.empty_white_image
                            (Form.value_or_default width_form ~default:0)
                            (Form.value_or_default height_form ~default:0)
                        in
                        set_state
                          (Some
                             (Ok
                                (Sd.Image.of_string
                                   ~kind:Base64
                                   (Js_of_ocaml.Js.to_string image)))))))
            ]
        in
        View.vbox
          [ file_upload
          ; empty_image_form
          ; View.button theme "text description" ~on_click:(set_state None)
          ]
      in
      match state with
      | None -> input
      | Some (Error e) ->
        View.vbox [ input; Vdom.Node.sexp_for_debugging [%sexp (e : Error.t)] ]
      | Some (Ok img) -> View.vbox [ input; Sd.Image.to_vdom img ]
    in
    let state =
      state
      >>| function
      | None -> None
      | Some (Error _) -> None
      | Some (Ok v) -> Some (Inc.Or_error_or_stale.Fresh v)
    in
    state, view
  in
  let { Single.image; gallery_view; form_view; form } =
    img2img_impl
      ~direction:`Horizontal
      ~pool
      ~prev:first_image
      ~prev_params:(Bonsai.return (Error (Error.of_string "no previous params")))
      ~mask:(Bonsai.return None)
      graph
  in
  let rest =
    component
      ~pool
      ~index:(Bonsai.return 1)
      ~prev:image
      ~prev_params:(form >>| Form.value)
      graph
  in
  let%arr params = form_view
  and gallery = gallery_view
  and _, rest = rest
  and first_image_view = first_image_view in
  View.vbox [ first_image_view; params; gallery ] :: rest
;;
