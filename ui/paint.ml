open! Core
open! Bonsai_web.Cont
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
    }
end

module Output = struct
  class type t = object
    method clear : unit Js.meth
    method updateImage : Js.js_string Js.t -> unit Js.meth
    method composite : Js.js_string Js.t Js.meth
    method compositeMask : Js.js_string Js.t Js.meth
    method penSize : int Js.prop
    method color : Js.js_string Js.t Js.prop
    method onColorChange : (Js.js_string Js.t -> unit) Js.callback Js.prop
    method setDirty : (unit -> unit) Js.callback Js.prop
    method mode : Js.js_string Js.t Js.prop
  end
end

module Widget :
  Bonsai_web_ui_widget.S with type input = Input.t and type state = Output.t Js.t = struct
  type element = Dom_html.divElement
  type input = Input.t
  type state = Output.t Js.t

  external painter_init : Js.js_string Js.t -> state * element Js.t = "painter_init"

  let wrap_cb on_color_change =
    Js.wrap_callback (fun arg ->
      Effect.Expert.handle_non_dom_event_exn (on_color_change arg))
  ;;

  let init ~get_input:_ input =
    let state, element = painter_init (Js.string input.Input.url) in
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

  let destroy _input _state _element = ()
end

module Id = Bonsai_extra.Id_gen (Int63) ()

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
      let%arr current_layer = current_layer
      and set_current_layer = set_current_layer
      and paint_visible = paint_visible
      and toggle_paint_visible = toggle_paint_visible
      and mask_visible = mask_visible
      and toggle_mask_visible = toggle_mask_visible
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
    current_layer, view, mask_visible
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
    ; forward_button : Vdom.Node.t
    ; clear_button : Vdom.Node.t
    ; widget : Vdom.Node.t
    }
end

type t =
  { images : Images.t Inc.t
  ; view : View_.t Bonsai.t
  }

let component ~prev:(image : Sd.Image.t Bonsai.t) graph =
  let color_picker = Form.Elements.Color_picker.hex () graph in
  let value, inject =
    Bonsai.state_machine0
      ~default_model:Inc.Or_error_or_stale.Not_computed
      ~apply_action:(fun _ model ->
        function
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
      (let%arr inject = inject in
       fun _ -> inject `Invalidate)
    graph;
  let widget =
    let input =
      let%arr url = image >>| Sd.Image.data_url
      and set_dirty = inject >>| fun inject -> inject `Invalidate
      and on_color_change =
        color_picker
        >>| Form.set
        >>| fun setter js_string -> setter (`Hex (Js.to_string js_string))
      in
      { Input.url; on_color_change; set_dirty }
    in
    Bonsai_web_ui_widget.component (module Widget) input graph
  in
  let slider =
    Form.Elements.Range.int
      ~extra_attrs:
        (Bonsai.return [ Vdom.Attr.style (Css_gen.create ~field:"" ~value:"") ])
      ~default:20
      ~min:1
      ~max:100
      ~step:1
      ~allow_updates_when_focused:`Never
      ()
      graph
  in
  let current_layer, layer_view, mask_visible = Layer_panel.component graph in
  Bonsai.Edge.on_change
    (slider >>| Form.value >>| Or_error.ok)
    ~equal:[%equal: int option]
    ~callback:
      (let%arr { modify; _ } = widget in
       function
       | None -> Effect.Ignore
       | Some width -> modify (fun _ state -> state##.penSize := width))
    graph;
  Bonsai.Edge.on_change
    current_layer
    ~equal:[%equal: Layer_panel.t]
    ~callback:
      (let%arr { modify; _ } = widget in
       function
       | Layer_panel.Paint -> modify (fun _ state -> state##.mode := Js.string "paint")
       | Mask -> modify (fun _ state -> state##.mode := Js.string "mask"))
    graph;
  Bonsai.Edge.on_change
    (color_picker >>| Form.value >>| Or_error.ok)
    ~equal:[%equal: [ `Hex of string ] option]
    ~callback:
      (let%arr { modify; _ } = widget in
       function
       | None -> Effect.Ignore
       | Some (`Hex color) -> modify (fun _ state -> state##.color := Js.string color))
    graph;
  let unique_id, next_id =
    Bonsai.state_machine0 ~default_model:0 ~apply_action:(fun _ i () -> i + 1) graph
  in
  let is_dirty =
    let%arr value = value in
    match value with
    | Fresh _ -> false
    | Stale _ | Error _ | Not_computed -> true
  in
  let widget_view =
    let%arr unique_id = unique_id
    and { view; _ } = widget
    and path = Bonsai.path_id graph
    and mask_visible = mask_visible in
    Vdom.Node.div
      ~key:(Int.to_string unique_id ^ path)
      ~attrs:[ (if mask_visible then Vdom.Attr.empty else Style.disabled_mask_layer) ]
      [ view ]
  in
  let forward_button =
    let%arr theme = View.Theme.current graph
    and inject = inject
    and is_dirty = is_dirty
    and { Bonsai_web_ui_widget.read; _ } = widget in
    let forward =
      let%bind.Effect effects =
        read (fun _input state ->
          let image = Sd.Image.of_string ~kind:Base64 (Js.to_string state##composite) in
          let mask =
            match Js.to_string state##compositeMask with
            | "" -> None
            | mask_string -> Some (Sd.Image.of_string ~kind:Base64 mask_string)
          in
          inject (`Set_value { Images.image; mask }))
      in
      Effect.all_unit effects
    in
    if is_dirty then View.button theme "forward" ~on_click:forward else Vdom.Node.none
  in
  let clear_button =
    let%arr theme = View.Theme.current graph
    and next_id = next_id in
    View.button theme "clear" ~on_click:(next_id ())
  in
  let view =
    let%arr color_picker = color_picker >>| Form.view
    and pen_size_slider = slider >>| Form.view
    and layer_panel = layer_view
    and forward_button = forward_button
    and clear_button = clear_button
    and widget = widget_view in
    { View_.color_picker
    ; pen_size_slider
    ; layer_panel
    ; forward_button
    ; clear_button
    ; widget
    }
  in
  { images = value; view }
;;

external empty_white_image : int -> int -> Js.js_string Js.t = "empty_white_image"

module View = View_
