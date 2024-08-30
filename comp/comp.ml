open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view

let rec insert_with_dedupe map ~key ~data =
  match Map.add map ~key ~data with
  | `Duplicate -> insert_with_dedupe map ~key:(key ^ "_") ~data
  | `Ok map -> map
;;

let load_image =
  let load_image_deferred url =
    let ivar = Async_kernel.Ivar.create () in
    Canvas2d.Image.of_url url ~on_load:(Async_kernel.Ivar.fill_if_empty ivar);
    Async_kernel.Ivar.read ivar
  in
  Effect.of_deferred_fun load_image_deferred
;;

let process_file file =
  let file = Shared.File_data_url.create file in
  match%bind.Effect Bonsai_web_ui_file.contents file with
  | Error e -> Effect.return (Error e)
  | Ok contents ->
    let%bind.Effect image = load_image (Bigstring.to_string contents) in
    (* Sd.Image.of_string ~kind:Base64 (Bigstring.to_string contents) in *)
    Effect.return (Ok image)
;;

let process_files ~files ~inject =
  let%bind.Effect files =
    Shared.Effect_utils.parallel_all
      (List.map files ~f:(fun file ->
         let%map.Effect processed = process_file file in
         Js_of_ocaml.Js.to_string file##.name, processed))
  in
  let map =
    List.fold files ~init:String.Map.empty ~f:(fun acc (key, data) ->
      insert_with_dedupe acc ~key ~data)
  in
  inject map
;;

let close_to_white (r, g, b, a) = a < 5 || (r > 250 && g > 250 && b > 250)
let printed = ref 0

let find_offset img1 img2 =
  let open Canvas2d in
  let _c1, ctx1 = Canvas.of_image ~will_read_frequently:true img1 in
  let c2, ctx2 = Canvas.of_image ~will_read_frequently:true img2 in
  let data1 = Ctx2d.get_image_data ctx1 in
  let data2 = Ctx2d.get_image_data ctx2 in
  let height = Int.min (Image_data.height data1) (Image_data.height data2) in
  let width1 = Image_data.width data1 in
  let offset =
    With_return.with_return (fun { return } ->
      let maximum_possible_x_offset =
        Int.min (Image_data.width data1) (Image_data.width data2)
      in
      for x_offset = 0 to maximum_possible_x_offset - 1 do
        for x = 0 to x_offset - 1 do
          let left_pos = width1 - x_offset + x in
          let right_pos = x in
          for y = 0 to height - 1 do
            let p1 = (Image_data.get_rgba [@inlined]) data1 ~x:left_pos ~y in
            let p2 = (Image_data.get_rgba [@inlined]) data2 ~x:right_pos ~y in
            if close_to_white p2
            then Image_data.set_a data2 ~x:right_pos ~y 0
            else if close_to_white p1
            then ()
            else return (x_offset - 1)
          done
        done
      done;
      maximum_possible_x_offset)
  in
  Ctx2d.put_image_data ctx2 data2 ~x:0 ~y:0;
  offset, c2
;;

let file_upload graph =
  let images, inject =
    Bonsai.state_machine0
      ~default_model:(String.Map.empty : Canvas2d.Image.t Or_error.t String.Map.t)
      ~apply_action:(fun _ctx _ files -> files)
      graph
  in
  let%sub images, reorderable_list =
    Bonsai_web_ui_reorderable_list.simple
      (module String)
      (images >>| Map.key_set)
      ~render:(fun ~index:_ ~source key _graph ->
        let data =
          let%arr images = images
          and key = key in
          Map.find images key
        in
        let%arr data = data
        and key = key
        and source = source in
        let view =
          match data with
          | None -> Vdom.Node.div ~attrs:[ source ] [ Vdom.Node.text key ]
          | Some (Ok img) ->
            let raw_image =
              Sd.Image.to_vdom
                ~attrs:[ source; {%css| max-width: 100px; |} ]
                (Sd.Image.of_string ~kind:Base64 (Canvas2d.Image.to_data_url img))
            in
            View.vbox [ Vdom.Node.text key; raw_image ]
          | Some (Error e) ->
            View.vbox
              ~attrs:[ source ]
              [ Vdom.Node.text key; Vdom.Node.sexp_for_debugging ([%sexp_of: Error.t] e) ]
        in
        data, view)
      graph
  in
  let images_for_processing, update_images_for_processing = Bonsai.state [] graph in
  let images_and_offsets =
    let%arr images = images_for_processing in
    let open Canvas2d in
    let good_images =
      List.filter_map images ~f:(function
        | _, Some (Ok img) -> Some img
        | _ -> None)
    in
    let height =
      List.max_elt ~compare:Int.compare (List.map good_images ~f:Image.height)
      |> Option.value ~default:1
    in
    let width = List.sum (module Int) good_images ~f:Image.width in
    let canvas = Canvas.create ~width ~height in
    let ctx = Canvas.ctx2d canvas in
    Ctx2d.set_fill_style ctx "white";
    Ctx2d.fill_rect
      ctx
      ~x:0.0
      ~y:0.0
      ~w:(Int.to_float (Ctx2d.width ctx))
      ~h:(Int.to_float (Ctx2d.height ctx));
    List.folding_map good_images ~init:(0, None) ~f:(fun (acc, prev) img ->
      let offset, canvas =
        match prev with
        | None ->
          let canvas, _ctx = Canvas.of_image img in
          0, canvas
        | Some prev -> find_offset prev img
      in
      let acc = acc - offset in
      (acc + Canvas.width canvas, Some img), (canvas, acc))
  in
  let gap_form =
    Form.Elements.Range.int
      ~allow_updates_when_focused:`Always
      ~min:(-100)
      ~max:100
      ~step:1
      ~default:0
      ()
      graph
  in
  let composed =
    let%arr images_and_offsets = images_and_offsets
    and gap = gap_form >>| Form.value_or_default ~default:0 in
    let open Canvas2d in
    let height =
      List.max_elt
        ~compare:Int.compare
        (List.map images_and_offsets ~f:(fun (canvas, _) -> Canvas.height canvas))
      |> Option.value ~default:1
    in
    let width =
      match List.last images_and_offsets with
      | None -> 1
      | Some (canvas, offset) -> Canvas.width canvas + offset
    in
    let width = (gap * (List.length images_and_offsets - 1)) + width in
    let canvas = Canvas.create ~width ~height in
    let ctx = Canvas.ctx2d canvas in
    Ctx2d.set_fill_style ctx "white";
    Ctx2d.fill_rect
      ctx
      ~x:0.0
      ~y:0.0
      ~w:(Int.to_float (Ctx2d.width ctx))
      ~h:(Int.to_float (Ctx2d.height ctx));
    List.iteri images_and_offsets ~f:(fun i (canvas, offset) ->
      Ctx2d.draw_canvas ctx canvas ~x:(Int.to_float (offset + (gap * i))) ~y:0.0);
    Sd.Image.of_string
      ~width:(Int63.of_int width)
      ~height:(Int63.of_int height)
      ~kind:Base64
      (Canvas.to_data_url canvas)
  in
  let%arr inject = inject
  and composed = composed
  and gap_form = gap_form >>| Form.view
  and reorderable_list = reorderable_list
  and images = images
  and theme = View.Theme.current graph
  and images_for_processing = images_for_processing
  and update_images_for_processing = update_images_for_processing in
  let upload_zone =
    Vdom.Node.div
      ~attrs:
        [ Shared.File_upload_zone.attr
            ()
            ~mime_types:[ "image/png" ]
            ~on_file_upload:(fun files -> process_files ~inject ~files)
        ; {%css| width: 100px;  height: 100px; background:red;|}
        ]
      [ Vdom_input_widgets.File_select.list
          ~on_input:(function
            | [] -> inject String.Map.empty
            | files -> process_files ~files ~inject)
          ()
      ]
  in
  View.vbox
    [ upload_zone
    ; reorderable_list
    ; (if phys_equal images images_for_processing
       then Vdom.Node.none
       else View.button theme "process" ~on_click:(update_images_for_processing images))
    ; gap_form
    ; Sd.Image.to_vdom composed
    ]
;;

let component graph =
  let%arr upload = file_upload graph in
  Snips.body upload |> Snips.render
;;
