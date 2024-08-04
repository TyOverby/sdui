open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module Style =
  [%css
  stylesheet
    {|
.wrapper {
  min-height: 100vh;
  width: 100%;
  /* background: red; */
  padding: 1em;
  display: flex;
  flex-direction: column;
  scroll-snap-align: start end;
}

.workspace {
  flex-grow: 1;
  /* background: green; */
  display: flex;
  min-height: 80vh;
}

.painting-area {
  flex-grow: 1;
  /* background: blue; */
  display: flex;
}

.painting-widget {
  flex-grow: 1;
  display: flex;
  /* background: orange; */
  align-items: center;
  justify-content: center;
  overflow:clip;
}

.painting-controls {
  /* background: purple; */
}

.widget {
  transform: scale(var(--scale));
}

|}]

let make
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
      ~step:0.001
      ~min:0.5
      ~default:1.0
      ~max:5.0
      ~allow_updates_when_focused:`Always
      ()
      graph
  in
  let%arr gallery_view = gallery_view
  and form_view = form_view
  and color_picker = color_picker
  and pen_size_slider = pen_size_slider
  and layer_panel = layer_panel
  and forward_button = forward_button
  and clear_button = clear_button
  and widget = widget
  and zoom_form = zoom_form in
  Vdom.Node.div
    ~attrs:[ Style.wrapper ]
    [ Vdom.Node.div
        ~attrs:[ Style.workspace ]
        [ Vdom.Node.div
            ~attrs:[ Style.painting_area ]
            [ Vdom.Node.div
                ~attrs:[ Style.painting_widget ]
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
            ; View.vbox
                ~attrs:[ Style.painting_controls ]
                [ color_picker
                ; pen_size_slider
                ; layer_panel
                ; forward_button
                ; clear_button
                ; Form.view zoom_form
                ]
            ]
        ; form_view
        ]
    ; gallery_view
    ]
;;
