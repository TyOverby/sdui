open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Form = Bonsai_web_ui_form.With_manual_view

module Style =
  [%css
  stylesheet
    ~dont_hash:[ "stack"; "mask_layer"; "blur_mask_layer"; "outline_layer" ]
    {|
        .stack {
            display: inline-grid;
            grid-template-rows: auto;
            grid-template-columns: auto;
        }

        .stack>canvas {
            grid-area: 1 / 1;
            cursor:none;
            image-rendering: pixelated;
        }

        * {
            touch-action: none;
        }

        .outline_layer {
          z-index:1;
          /* mix-blend-mode:difference;*/
        }

        .mask_layer {
          opacity: 0.5;
        }

        .blur_mask_layer {
          opacity: 0.5;
        }

        .disabled_mask_layer .mask_layer,
        .disabled_blur_mask_layer .blur_mask_layer
         {
          display:none;
        }
|}]

module Input = struct
  type t =
    { url : string
    ; on_color_change : Js.js_string Js.t -> unit Effect.t
    ; set_dirty : unit Effect.t
    ; set_wip_store :
        Js.js_string Js.t Js.Optdef.t
        * Js.js_string Js.t Js.Optdef.t
        * Js.js_string Js.t Js.Optdef.t
        -> unit Effect.t
    ; wip_store :
        Js.js_string Js.t Js.Optdef.t
        * Js.js_string Js.t Js.Optdef.t
        * Js.js_string Js.t Js.Optdef.t
    }
end

module Output = struct
  class type t = object
    method clear : unit Js.meth
    method updateImage : Js.js_string Js.t -> unit Js.meth
    method setPaintImage : Js.js_string Js.t -> unit Js.meth
    method composite : Js.js_string Js.t Js.meth
    method flipCanvas : unit Js.meth
    method getPaintLayer : Js.js_string Js.t Js.optdef Js.meth
    method getMaskLayer : Js.js_string Js.t Js.optdef Js.meth
    method getBlurMaskLayer : Js.js_string Js.t Js.optdef Js.meth
    method compositeMask : Js.js_string Js.t Js.optdef Js.meth
    method penSize : int Js.prop
    method color : Js.js_string Js.t Js.prop
    method onColorChange : (Js.js_string Js.t -> unit) Js.callback Js.prop
    method setDirty : (unit -> unit) Js.callback Js.prop
    method mode : Js.js_string Js.t Js.prop
    method alt : Js.js_string Js.t Js.prop
  end
end

module Widget :
  Bonsai_web_ui_low_level_vdom.Widget.S
  with type input = Input.t
   and type state = Output.t Js.t = struct
  type element = Dom_html.divElement
  type input = Input.t
  type state = Output.t Js.t

  external painter_init
    :  Js.js_string Js.t
    -> Js.js_string Js.t Js.Optdef.t
    -> Js.js_string Js.t Js.Optdef.t
    -> Js.js_string Js.t Js.Optdef.t
    -> state * element Js.t
    = "painter_init"

  let wrap_cb f =
    Js.wrap_callback (fun arg -> Effect.Expert.handle_non_dom_event_exn (f arg))
  ;;

  let init ~get_input:_ input =
    let state, element =
      painter_init
        (Js.string input.Input.url)
        (Tuple3.get1 input.wip_store)
        (Tuple3.get2 input.wip_store)
        (Tuple3.get3 input.wip_store)
    in
    state##.onColorChange := wrap_cb input.on_color_change;
    state##.setDirty := wrap_cb (fun () -> input.set_dirty);
    state, element
  ;;

  let update ~prev_input new_input state element =
    if not (String.equal prev_input.Input.url new_input.Input.url)
    then state##updateImage (Js.string new_input.url);
    if not (phys_equal prev_input.Input.on_color_change new_input.Input.on_color_change)
    then state##.onColorChange := wrap_cb new_input.on_color_change;
    if not (phys_equal prev_input.Input.set_dirty new_input.Input.set_dirty)
    then state##.setDirty := wrap_cb (fun () -> new_input.set_dirty);
    element
  ;;

  let destroy (input : Input.t) state _element =
    Effect.Expert.handle_non_dom_event_exn
      (input.set_wip_store
         (state##getPaintLayer, state##getMaskLayer, state##getBlurMaskLayer));
    ()
  ;;
end

module Id = Bonsai_extra.Id_gen (Int63) ()

module Alt_panel = struct
  type t =
    | Erase
    | Shuffle
  [@@deriving sexp_of, equal, enumerate, compare]

  module Style =
    [%css
    stylesheet
      {|
    .selected {
      background: var(--selected-bg);
      color: var(--selected-fg);
    }

    .layers {
      text-transform: uppercase;
      user-select:none;
      margin: 0.25em 0.5em;
    }

    .layer {
      padding: 0 0.5em;
    }
  |}]

  let component graph =
    let alt, set_alt = Bonsai.state Erase graph in
    let view =
      let%arr alt
      and set_alt
      and theme = View.Theme.current graph in
      let erase_box =
        View.hbox
          ~cross_axis_alignment:Center
          ~gap:(`Em_float 0.5)
          ~attrs:
            [ (if equal alt Erase then Style.selected else Vdom.Attr.empty)
            ; Vdom.Attr.on_click (fun _ -> set_alt Erase)
            ; Style.layer
            ]
          [ Feather_icon.svg ~size:(`Px 16) Trash; Vdom.Node.text "Erase" ]
      in
      let scramble_box =
        View.hbox
          ~cross_axis_alignment:Center
          ~gap:(`Em_float 0.5)
          ~attrs:
            [ (if equal alt Shuffle then Style.selected else Vdom.Attr.empty)
            ; Vdom.Attr.on_click (fun _ -> set_alt Shuffle)
            ; Style.layer
            ]
          [ Feather_icon.svg Minimize ~size:(`Px 16); Vdom.Node.text "Shuffle" ]
      in
      let colors =
        let { View.Fg_bg.foreground; background } = View.intent_colors theme Info in
        Style.Variables.set_all
          ~selected_bg:(Css_gen.Color.to_string_css background)
          ~selected_fg:(Css_gen.Color.to_string_css foreground)
      in
      View.hbox ~attrs:[ colors; Style.layers ] [ erase_box; scramble_box ]
    in
    alt, view
  ;;
end

module Layer_panel = struct
  type t =
    | Paint
    | Mask
    | Blur
  [@@deriving sexp_of, equal, enumerate, compare]

  module Style =
    [%css
    stylesheet
      {|
    .selected {
      background: var(--selected-bg);
      color: var(--selected-fg);
    }

    .layers {
      text-transform: uppercase;
      user-select:none;
    }

    .layer {
      padding: 0 0.5em;
    }
  |}]

  let component graph =
    let current_layer, set_current_layer = Bonsai.state Paint graph in
    let paint_visible, toggle_paint_visible = Bonsai.toggle ~default_model:true graph in
    let mask_visible, toggle_mask_visible = Bonsai.toggle ~default_model:true graph in
    let blur_visible, toggle_blur_visible = Bonsai.toggle ~default_model:true graph in
    let view =
      let%arr current_layer
      and set_current_layer
      and paint_visible
      and toggle_paint_visible
      and mask_visible
      and toggle_mask_visible
      and blur_visible
      and toggle_blur_visible
      and theme = View.Theme.current graph in
      let paint_layer =
        View.hbox
          ~cross_axis_alignment:Center
          ~gap:(`Em_float 0.5)
          ~attrs:
            [ (if equal current_layer Paint then Style.selected else Vdom.Attr.empty)
            ; Vdom.Attr.on_click (fun _ -> set_current_layer Paint)
            ; Style.layer
            ]
          [ Feather_icon.svg
              (if paint_visible then Eye else Eye_off)
              ~size:(`Px 16)
              ~extra_attrs:
                [ Vdom.Attr.on_click (fun _ ->
                    Effect.Many [ toggle_paint_visible; Effect.Stop_propagation ])
                ]
          ; Vdom.Node.text "Paint"
          ]
      in
      let mask_layer =
        View.hbox
          ~cross_axis_alignment:Center
          ~gap:(`Em_float 0.5)
          ~attrs:
            [ (if equal current_layer Mask then Style.selected else Vdom.Attr.empty)
            ; Vdom.Attr.on_click (fun _ -> set_current_layer Mask)
            ; Style.layer
            ]
          [ Feather_icon.svg
              (if mask_visible then Eye else Eye_off)
              ~size:(`Px 16)
              ~extra_attrs:
                [ Vdom.Attr.on_click (fun _ ->
                    Effect.Many [ toggle_mask_visible; Effect.Stop_propagation ])
                ]
          ; Vdom.Node.text "Mask"
          ]
      in
      let blur_layer =
        View.hbox
          ~cross_axis_alignment:Center
          ~gap:(`Em_float 0.5)
          ~attrs:
            [ (if equal current_layer Blur then Style.selected else Vdom.Attr.empty)
            ; Vdom.Attr.on_click (fun _ -> set_current_layer Blur)
            ; Style.layer
            ]
          [ Feather_icon.svg
              (if blur_visible then Eye else Eye_off)
              ~size:(`Px 16)
              ~extra_attrs:
                [ Vdom.Attr.on_click (fun _ ->
                    Effect.Many [ toggle_blur_visible; Effect.Stop_propagation ])
                ]
          ; Vdom.Node.text "Blur"
          ]
      in
      let colors =
        let { View.Fg_bg.foreground; background } = View.intent_colors theme Info in
        Style.Variables.set_all
          ~selected_bg:(Css_gen.Color.to_string_css background)
          ~selected_fg:(Css_gen.Color.to_string_css foreground)
      in
      View.vbox ~attrs:[ colors; Style.layers ] [ paint_layer; mask_layer; blur_layer ]
    in
    current_layer, view, mask_visible, blur_visible, set_current_layer
  ;;
end

module Images = struct
  type t =
    { image : Sd.Image.t
    ; mask : Sd.Image.t option
    ; blur_mask : Sd.Image.t option
    }
end

module View_ = struct
  type t =
    { color_picker : Vdom.Node.t
    ; pen_size_slider : Vdom.Node.t
    ; blur_radius_slider : Vdom.Node.t
    ; layer_panel : Vdom.Node.t
    ; alt_panel : Vdom.Node.t
    ; flip_button : Vdom.Node.t
    ; forward_button : Vdom.Node.t
    ; clear_button : Vdom.Node.t
    ; clone_button : Vdom.Node.t
    ; blur_button : Vdom.Node.t
    ; widget : Vdom.Node.t
    }
end

type t =
  { images : Images.t Inc.t
  ; get_images : Images.t Effect.t Bonsai.t
  ; view : View_.t Bonsai.t
  ; set_paint_image :
      (which:[ `Paint | `Underlying ] -> Sd.Image.t -> unit Effect.t) Bonsai.t
  ; requesting_set_paint_image : (Sd.Image.t -> unit Effect.t) option Bonsai.t
  }

module Canvas_ops = struct
  open Canvas2d

  let intersect img ~mask =
    let result, ctx = Canvas.clone mask in
    Ctx2d.set_global_composite_operation ctx "source-in";
    Ctx2d.draw_canvas ctx img ~x:0.0 ~y:0.0;
    result
  ;;

  let union a b =
    let result, ctx = Canvas.clone a in
    Ctx2d.draw_canvas ctx b ~x:0.0 ~y:0.0;
    result
  ;;

  let overlay a b ~mask = union a (intersect b ~mask)

  let blur img ~by =
    let result = Canvas.create ~width:(Canvas.width img) ~height:(Canvas.height img) in
    let ctx = Canvas.ctx2d result in
    Ctx2d.set_filter ctx (sprintf "blur(%dpx)" by);
    Ctx2d.draw_canvas ctx img ~x:0.0 ~y:0.0;
    result
  ;;

  let no_transparency img =
    let result = Canvas.create ~width:(Canvas.width img) ~height:(Canvas.height img) in
    let ctx = Canvas.ctx2d ~will_read_frequently:true result in
    Ctx2d.draw_canvas ctx img ~x:0.0 ~y:0.0;
    let img_data = Ctx2d.get_image_data ctx in
    for y = 0 to Image_data.height img_data - 1 do
      for x = 0 to Image_data.width img_data - 1 do
        Image_data.set_a img_data ~x ~y 255
      done
    done;
    Ctx2d.put_image_data ctx img_data ~x:0 ~y:0;
    result
  ;;
end

let blur_by_mask ~(image : Canvas2d.Image.t) ~(mask : Canvas2d.Image.t) ~by ~on_load =
  let open Canvas2d in
  let open Canvas_ops in
  let image, _ = Canvas.of_image image
  and mask, _ = Canvas.of_image mask in
  let chopped = intersect image ~mask in
  let blurred = blur chopped ~by in
  let no_transparency = no_transparency blurred in
  let stacked = overlay image no_transparency ~mask in
  let final = stacked in
  Ctx2d.to_image (Canvas.ctx2d final) ~on_load
;;

let run_on_change_and_on_init value ~equal ~widget ~f (local_ graph) =
  Bonsai.Edge.on_change
    value
    ~equal
    ~callback:
      (let%arr { Bonsai_web_ui_low_level_vdom.Widget.modify; _ } = widget in
       fun value -> modify (fun _ state -> f state value))
    graph;
  Bonsai.Edge.lifecycle
    graph
    ~on_activate:
      (let%arr value
       and { Bonsai_web_ui_low_level_vdom.Widget.modify; _ } = widget in
       modify (fun _ state -> f state value))
;;

let component ~prev:(image : Sd.Image.t Bonsai.t) graph =
  let color_picker = Form.Elements.Color_picker.hex () graph in
  let value, inject =
    Bonsai.state_machine0
      ~default_model:Inc.Or_error_or_stale.Not_computed
      ~apply_action:(fun _ model -> function
        | `Set_value t -> Inc.Or_error_or_stale.Fresh t
        | `Invalidate ->
          (match model with
           | Fresh s -> Stale s
           | other -> other))
      graph
  in
  Bonsai.Edge.on_change
    image
    ~equal:Sd.Image.equal
    ~callback:
      (let%arr inject in
       fun _ -> inject `Invalidate)
    graph;
  let wip_store, set_wip_store =
    Bonsai.state
      ( Js_of_ocaml.Js.Optdef.empty
      , Js_of_ocaml.Js.Optdef.empty
      , Js_of_ocaml.Js.Optdef.empty )
      graph
  in
  let current_layer, layer_view, mask_visible, blur_mask_visible, set_current_layer =
    Layer_panel.component graph
  in
  let on_color_change =
    let%arr color_picker and set_current_layer in
    fun js_string ->
      Effect.Many
        [ set_current_layer Layer_panel.Paint
        ; Form.set color_picker (`Hex (Js.to_string js_string))
        ]
  in
  let widget =
    let input =
      let%arr url = image >>| Sd.Image.to_string
      and set_dirty = inject >>| fun inject -> inject `Invalidate
      and set_wip_store
      and wip_store
      and on_color_change in
      { Input.url; on_color_change; set_dirty; set_wip_store; wip_store }
    in
    Bonsai_web_ui_low_level_vdom.Widget.component (module Widget) input graph
  in
  let blur_radius_slider =
    Form.Elements.Range.int
      ~default:(Bonsai.return 10)
      ~min:(Bonsai.return 1)
      ~max:(Bonsai.return 100)
      ~step:(Bonsai.return 1)
      ~allow_updates_when_focused:`Never
      ()
      graph
  in
  let pen_size_slider =
    Form.Elements.Range.int
      ~default:(Bonsai.return 20)
      ~min:(Bonsai.return 1)
      ~max:(Bonsai.return 100)
      ~step:(Bonsai.return 1)
      ~allow_updates_when_focused:`Never
      ()
      graph
  in
  let alt, alt_panel = Alt_panel.component graph in
  run_on_change_and_on_init
    (pen_size_slider >>| Form.value)
    ~equal:[%equal: int Or_error.t]
    ~widget
    graph
    ~f:(fun state -> function
    | Error _ -> ()
    | Ok value -> state##.penSize := value);
  run_on_change_and_on_init
    current_layer
    ~equal:[%equal: Layer_panel.t]
    ~widget
    graph
    ~f:(fun state -> function
    | Paint -> state##.mode := Js.string "paint"
    | Mask -> state##.mode := Js.string "mask"
    | Blur -> state##.mode := Js.string "blur");
  run_on_change_and_on_init
    alt
    ~equal:[%equal: Alt_panel.t]
    ~widget
    graph
    ~f:(fun state -> function
    | Erase -> state##.alt := Js.string "erase"
    | Shuffle -> state##.alt := Js.string "shuffle");
  run_on_change_and_on_init
    (color_picker >>| Form.value)
    ~equal:[%equal: [ `Hex of string ] Or_error.t]
    ~widget
    ~f:(fun state -> function
      | Error _ -> ()
      | Ok (`Hex color) -> state##.color := Js.string color)
    graph;
  let unique_id, next_id =
    Bonsai.state_machine0 ~default_model:0 ~apply_action:(fun _ i () -> i + 1) graph
  in
  let is_dirty =
    let%arr value in
    match value with
    | Fresh _ -> false
    | Stale _ | Error _ | Not_computed -> true
  in
  let widget_view =
    let%arr unique_id
    and { view; _ } = widget
    and path = Bonsai.path_id graph
    and mask_visible
    and blur_mask_visible in
    Vdom.Node.div
      ~key:(Int.to_string unique_id ^ path)
      ~attrs:
        [ (if mask_visible then Vdom.Attr.empty else Style.disabled_mask_layer)
        ; (if blur_mask_visible then Vdom.Attr.empty else Style.disabled_blur_mask_layer)
        ]
      [ view ]
  in
  let get_images_effect =
    let%arr prev = image
    and { Bonsai_web_ui_low_level_vdom.Widget.read; _ } = widget in
    let%bind.Effect effects =
      read (fun _input state ->
        let image = Sd.Image.of_string ~kind:Base64 (Js.to_string state##composite) in
        let%bind.Effect mask =
          match
            Js.Optdef.to_option state##compositeMask
            |> Option.value_map ~f:Js.to_string ~default:""
          with
          | "" -> Effect.return None
          | mask_string ->
            Effect.return (Some (Sd.Image.of_string ~kind:Base64 mask_string))
        in
        let blur_mask =
          match Js.Optdef.to_option state##getBlurMaskLayer with
          | None -> None
          | Some blur_mask_layer ->
            Some (Sd.Image.of_string ~kind:Base64 (Js.to_string blur_mask_layer))
        in
        Effect.return { Images.image; mask; blur_mask })
    in
    match effects with
    | [] -> Effect.return { Images.image = prev; mask = None; blur_mask = None }
    | t :: _ -> t
  in
  let forward_effect =
    let%arr get_images_effect and inject in
    let%bind.Effect images = get_images_effect in
    inject (`Set_value images)
  in
  let forward_button =
    let%arr is_dirty
    and forward = forward_effect
    and theme = View.Theme.current graph in
    if is_dirty then View.button theme "forward" ~on_click:forward else Vdom.Node.none
  in
  let clear_button =
    let%arr theme = View.Theme.current graph
    and next_id in
    View.button theme "clear" ~on_click:(next_id ())
  in
  let flip_button =
    let%arr theme = View.Theme.current graph
    and { modify; _ } = widget in
    View.button theme "flip" ~on_click:(modify (fun _ state -> state##flipCanvas))
  in
  let blur_button =
    let%arr theme = View.Theme.current graph
    and get_images_effect
    and { modify; _ } = widget
    and blur_radius = blur_radius_slider >>| Form.value_or_default ~default:10 in
    View.button
      theme
      "blur"
      ~on_click:
        (match%bind.Effect get_images_effect with
         | { image = _; mask = _; blur_mask = None } -> Effect.return ()
         | { image; mask = _; blur_mask = Some blur_mask } ->
           let%bind.Effect image =
             Sd.Load_image_effect.load_image (Sd.Image.to_string image)
           in
           let%bind.Effect blur_mask =
             Sd.Load_image_effect.load_image (Sd.Image.to_string blur_mask)
           in
           let%bind.Effect output =
             Sd.Load_image_effect.load_image_generic
               (blur_by_mask ~image ~mask:blur_mask ~by:blur_radius)
           in
           modify (fun _ state ->
             let data_url = Canvas2d.Image.to_data_url output in
             state##setPaintImage (Js.string data_url)))
  in
  let set_paint_image =
    let%arr get_widget = Bonsai.peek widget graph
    and next_id
    and wait_after_display = Bonsai.Edge.wait_after_display graph in
    fun ~which image ->
      match%bind.Effect get_widget with
      | Inactive -> Effect.return ()
      | Active { modify; _ } ->
        (match which with
         | `Paint ->
           modify (fun _ state ->
             state##setPaintImage (Js.string (Sd.Image.to_string image)))
         | `Underlying ->
           let%bind.Effect () = next_id () in
           let%bind.Effect () = wait_after_display in
           modify (fun _ state ->
             state##updateImage (Js.string (Sd.Image.to_string image))))
  in
  let requesting_set_paint_image, clone_button =
    let enabled, set_enabled = Bonsai.state false graph in
    let set_paint_image =
      let%arr set_paint_image and enabled and set_enabled in
      match enabled with
      | false -> None
      | true ->
        Some
          (fun image ->
            let%bind.Effect () = set_enabled false in
            set_paint_image ~which:`Paint image)
    in
    let clone_button =
      let%arr enabled
      and set_enabled
      and theme = View.Theme.current graph in
      match enabled with
      | false -> View.button theme "clone" ~on_click:(set_enabled true)
      | true -> View.button theme "stop clone" ~on_click:(set_enabled false)
    in
    set_paint_image, clone_button
  in
  let view =
    let%arr color_picker = color_picker >>| Form.view
    and pen_size_slider = pen_size_slider >>| Form.view
    and blur_radius_slider = blur_radius_slider >>| Form.view
    and layer_panel = layer_view
    and alt_panel
    and forward_button
    and clear_button
    and flip_button
    and clone_button
    and blur_button
    and widget = widget_view in
    { View_.color_picker
    ; pen_size_slider
    ; blur_radius_slider
    ; alt_panel
    ; layer_panel
    ; flip_button
    ; forward_button
    ; clone_button
    ; clear_button
    ; blur_button
    ; widget
    }
  in
  { images = value
  ; get_images = get_images_effect
  ; view
  ; set_paint_image
  ; requesting_set_paint_image
  }
;;

external empty_white_image : int -> int -> Js.js_string Js.t = "empty_white_image"

module View = View_
