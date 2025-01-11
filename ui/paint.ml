open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Form = Bonsai_web_ui_form.With_manual_view

module Style =
  [%css
  stylesheet
    ~dont_hash:[ "stack"; "mask_layer"; "outline_layer" ]
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
        }

        .mask_layer {
          opacity: 0.5;
        }

        .disabled_mask_layer .mask_layer {
          display:none;
        }
|}]

module Input = struct
  type t =
    { url : string
    ; on_color_change : Js.js_string Js.t -> unit Effect.t
    ; set_dirty : unit Effect.t
    ; set_wip_store :
        Js.js_string Js.t Js.Optdef.t * Js.js_string Js.t Js.Optdef.t -> unit Effect.t
    ; wip_store : Js.js_string Js.t Js.Optdef.t * Js.js_string Js.t Js.Optdef.t
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
    -> state * element Js.t
    = "painter_init"

  let wrap_cb f =
    Js.wrap_callback (fun arg -> Effect.Expert.handle_non_dom_event_exn (f arg))
  ;;

  let init ~get_input:_ input =
    let state, element =
      painter_init
        (Js.string input.Input.url)
        (Tuple2.get1 input.wip_store)
        (Tuple2.get2 input.wip_store)
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
      (input.set_wip_store (state##getPaintLayer, state##getMaskLayer));
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
    let view =
      let%arr current_layer
      and set_current_layer
      and paint_visible
      and toggle_paint_visible
      and mask_visible
      and toggle_mask_visible
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
      let colors =
        let { View.Fg_bg.foreground; background } = View.intent_colors theme Info in
        Style.Variables.set_all
          ~selected_bg:(Css_gen.Color.to_string_css background)
          ~selected_fg:(Css_gen.Color.to_string_css foreground)
      in
      View.vbox ~attrs:[ colors; Style.layers ] [ paint_layer; mask_layer ]
    in
    current_layer, view, mask_visible, set_current_layer
  ;;
end

module Images = struct
  type t =
    { image : Sd.Image.t
    ; mask : Sd.Image.t option
    }
end

module View_ = struct
  type t =
    { color_picker : Vdom.Node.t
    ; pen_size_slider : Vdom.Node.t
    ; layer_panel : Vdom.Node.t
    ; alt_panel : Vdom.Node.t
    ; flip_button : Vdom.Node.t
    ; forward_button : Vdom.Node.t
    ; clear_button : Vdom.Node.t
    ; clone_button : Vdom.Node.t
    ; padding : Vdom.Node.t
    ; widget : Vdom.Node.t
    }
end

type t =
  { images : Images.t Inc.t
  ; get_images : Images.t Effect.t Bonsai.t
  ; view : View_.t Bonsai.t
  ; set_paint_image : (Sd.Image.t -> unit Effect.t) option Bonsai.t
  }

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
    Bonsai.state (Js_of_ocaml.Js.Optdef.empty, Js_of_ocaml.Js.Optdef.empty) graph
  in
  let current_layer, layer_view, mask_visible, set_current_layer =
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
      let%arr url = image >>| Sd.Image.data_url
      and set_dirty = inject >>| fun inject -> inject `Invalidate
      and set_wip_store
      and wip_store
      and on_color_change in
      { Input.url; on_color_change; set_dirty; set_wip_store; wip_store }
    in
    Bonsai_web_ui_low_level_vdom.Widget.component (module Widget) input graph
  in
  let slider =
    Form.Elements.Range.int
      ~extra_attrs:
        (Bonsai.return [ Vdom.Attr.style (Css_gen.create ~field:"" ~value:"") ])
      ~default:(Bonsai.return 20)
      ~min:(Bonsai.return 1)
      ~max:(Bonsai.return 100)
      ~step:(Bonsai.return 1)
      ~allow_updates_when_focused:`Never
      ()
      graph
  in
  let alt, alt_panel = Alt_panel.component graph in
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
  run_on_change_and_on_init
    (slider >>| Form.value)
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
    | Mask -> state##.mode := Js.string "mask");
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
    and mask_visible in
    Vdom.Node.div
      ~key:(Int.to_string unique_id ^ path)
      ~attrs:[ (if mask_visible then Vdom.Attr.empty else Style.disabled_mask_layer) ]
      [ view ]
  in
  let get_images_effect =
    let%arr padding_left =
      padding_left >>| Form.value_or_default ~default:(Int63.of_int 0)
    and padding_right = padding_left >>| Form.value_or_default ~default:(Int63.of_int 0)
    and padding_top = padding_left >>| Form.value_or_default ~default:(Int63.of_int 0)
    and padding_bottom = padding_left >>| Form.value_or_default ~default:(Int63.of_int 0)
    and prev = image
    and { Bonsai_web_ui_low_level_vdom.Widget.read; _ } = widget in
    let%bind.Effect effects =
      read (fun _input state ->
        let%bind.Effect image =
          Sd.Load_image_effect.load_image (Js.to_string state##composite)
        in
        let%bind.Effect image =
          Sd.Load_image_effect.load_image_generic
            (Canvas2d.Image.add_padding
               ~left:(Int63.to_int_trunc padding_left)
               ~right:(Int63.to_int_trunc padding_right)
               ~top:(Int63.to_int_trunc padding_top)
               ~bottom:(Int63.to_int_trunc padding_bottom)
               image
               ~fill_color:"white")
        in
        print_s [%message "" (Canvas2d.Image.width image : int)];
        let image = Sd.Image.of_string ~kind:Base64 (Canvas2d.Image.to_data_url image) in
        let%bind.Effect mask =
          match
            Js.Optdef.to_option state##compositeMask
            |> Option.value_map ~f:Js.to_string ~default:""
          with
          | "" -> Effect.return None
          | mask_string ->
            let%bind.Effect mask = Sd.Load_image_effect.load_image mask_string in
            let%bind.Effect mask =
              Sd.Load_image_effect.load_image_generic
                (Canvas2d.Image.add_padding
                   ~left:(Int63.to_int_trunc padding_left)
                   ~right:(Int63.to_int_trunc padding_right)
                   ~top:(Int63.to_int_trunc padding_top)
                   ~bottom:(Int63.to_int_trunc padding_bottom)
                   mask
                   ~fill_color:"white")
            in
            Effect.return
              (Some (Sd.Image.of_string ~kind:Base64 (Canvas2d.Image.to_data_url mask)))
        in
        Effect.return { Images.image; mask })
    in
    match effects with
    | [] -> Effect.return { Images.image = prev; mask = None }
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
  let padding =
    let%arr padding_left and padding_right and padding_top and padding_bottom in
    View.vbox
      [ View.hbox [ Form.view padding_left; Form.view padding_right ]
      ; View.hbox [ Form.view padding_top; Form.view padding_bottom ]
      ]
  in
  let set_paint_image, clone_button =
    let enabled, set_enabled = Bonsai.state false graph in
    let set_paint_image =
      let%arr { modify; _ } = widget
      and enabled
      and set_enabled in
      match enabled with
      | false -> None
      | true ->
        Some
          (fun image ->
            let%bind.Effect () = set_enabled false in
            modify (fun _ state ->
              state##setPaintImage (Js.string (Sd.Image.data_url image))))
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
    and pen_size_slider = slider >>| Form.view
    and layer_panel = layer_view
    and alt_panel
    and forward_button
    and clear_button
    and flip_button
    and clone_button
    and widget = widget_view
    and padding in
    { View_.color_picker
    ; pen_size_slider
    ; alt_panel
    ; layer_panel
    ; flip_button
    ; forward_button
    ; clone_button
    ; clear_button
    ; widget
    ; padding
    }
  in
  { images = value; get_images = get_images_effect; view; set_paint_image }
;;

external empty_white_image : int -> int -> Js.js_string Js.t = "empty_white_image"

module View = View_
