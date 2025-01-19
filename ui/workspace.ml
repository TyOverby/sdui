open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module Snips = Shared.Snips

type t = hosts:Vdom.Node.t -> queue:Vdom.Node.t -> route:Vdom.Node.t -> Vdom.Node.t

module Style = [%css stylesheet {|
.widget {
  transform: scale(var(--scale));
}

|}]

let make
  ~(index : int Bonsai.t)
  ~(reset : unit Effect.t Bonsai.t)
  ~(gallery_view : Vdom.Node.t Bonsai.t)
  ~(form_view : Vdom.Node.t Bonsai.t)
  ~(color_picker : Vdom.Node.t Bonsai.t)
  ~(pen_size_slider : Vdom.Node.t Bonsai.t)
  ~(layer_panel : Vdom.Node.t Bonsai.t)
  ~(forward_button : Vdom.Node.t Bonsai.t)
  ~(clear_button : Vdom.Node.t Bonsai.t)
  ~(widget : Vdom.Node.t Bonsai.t)
  graph
  =
  let zoom_form =
    Form.Elements.Range.float
      ~step:(Bonsai.return 0.001)
      ~min:(Bonsai.return 0.5)
      ~default:(Bonsai.return 1.0)
      ~max:(Bonsai.return 5.0)
      ~allow_updates_when_focused:`Always
      ()
      graph
  in
  let _ = index, reset in
  let%arr gallery_view
  and form_view
  and color_picker
  and pen_size_slider
  and layer_panel
  and forward_button
  and clear_button
  and widget
  and zoom_form in
  fun ~hosts:_ ~queue ~route ->
    let open Snips.Infix in
    let paint_controls =
      View.vbox
        [ color_picker
        ; pen_size_slider
        ; layer_panel
        ; forward_button
        ; clear_button
        ; Form.view zoom_form
        ]
    in
    Snips.top (View.hbox [ queue ])
    |+| Snips.bottom route
    |+| Snips.right form_view
    |+| Snips.right paint_controls
    |+| Snips.body
          (View.vbox
             [ Vdom.Node.div
                 ~attrs:
                   [ {%css|
          display: flex;
          justify-content: center;
          align-content: center;
          align-items: center;
          min-height: 75vh;

          --color-gray: #80808050;
          background: repeating-conic-gradient(#80808050 0% 25%, transparent 0% 50%) 50% / 20px 20px;
          overflow:clip;
          |}
                   ]
                 [ Vdom.Node.div
                     ~attrs:
                       [ Style.widget
                       ; Style.Variables.set_all
                           ~scale:
                             (Virtual_dom.Dom_float.to_string
                                (Form.value_or_default zoom_form ~default:1.0))
                       ]
                     [ widget ]
                 ]
             ; gallery_view
             ])
    |> Snips.render
;;

let for_first_node ~first_image_view ~form_view ~gallery =
  Fn.id (fun ~hosts ~queue ~route ->
    let open Snips.Infix in
    Snips.top (View.hbox [ hosts; queue ])
    |+| Snips.bottom route
    |+| Snips.right form_view
    |+| Snips.body (View.vbox [ first_image_view; gallery ])
    |> Snips.render)
;;

let empty ~hosts:_ ~queue:_ ~route:_ = Vdom.Node.none
let finalize = Fn.id
