open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

module Label_modifications =
  [%css
  stylesheet
    {|
  input, select, button {
    font-size: 0.8em !important;
  }

  fieldset.muted-label > legend {
    text-transform: uppercase;
    font-size:0.5em;
    letter-spacing: 1px;
    padding-left: 5px;
    padding-right: 3px;
    margin-left: 5px;
  }

  fieldset.muted-label:not(:focus-within) > legend {
    color: color-mix(in oklab, var(--border) 30%, var(--fg));
  }

|}]

let int_form
  ?(input_attrs = [])
  ?(container_attrs = [])
  ~title
  ~default
  ~step
  ~length
  ~min
  ~max
  ~(validate_or_correct : string -> (Int63.t, Int63.t) Result.t)
  ()
  =
  let%sub theme = View.Theme.current in
  let%sub state, set_state = Bonsai.state (Int63.to_string default) in
  let%sub id = Bonsai.path_id in
  let%arr theme = theme
  and state = state
  and set_state = set_state
  and id = id in
  let value_or_corrected = validate_or_correct state in
  let is_error = Result.is_ok value_or_corrected in
  let fix_on_blur =
    match value_or_corrected with
    | Ok _ -> Vdom.Attr.empty
    | Error corrected ->
      Vdom.Attr.on_blur (fun _ -> set_state (Int63.to_string corrected))
  in
  let view =
    Kado.Unstable.Input.textbox
      ~constants:(View.constants theme)
      ~input_attr:
        (Vdom.Attr.many
           ([ fix_on_blur
            ; Vdom.Attr.max (Float.of_int63 max)
            ; Vdom.Attr.min (Float.of_int63 min)
            ; Vdom.Attr.create "step" (Int.to_string step)
            ; Vdom.Attr.type_ "number"
            ; Vdom.Attr.style (Css_gen.width length)
            ]
            @ input_attrs))
      ~container_attr:
        (Vdom.Attr.many
           ([ Label_modifications.muted_label
            ; Label_modifications.Variables.set_all
                ~border:
                  (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
                ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
            ; (if is_error then Vdom.Attr.class_ "error" else Vdom.Attr.empty)
            ; Vdom.Attr.style (Css_gen.create ~field:"height" ~value:"fit-content")
            ]
            @ container_attrs))
      ~title:(Some title)
      ~on_change:set_state
      ~value:state
  in
  let value =
    match value_or_corrected with
    | Ok v -> v
    | Error v -> v
  in
  let form =
    Form.Expert.create
      ~value:(Ok value)
      ~set:(fun i ->
        set_state
          (Int63.to_string
             (match validate_or_correct (Int63.to_string i) with
              | Ok v -> v
              | Error v -> v)))
      ~view:(Form.View.of_vdom ~id view)
  in
  form, view
;;

module Kado_textarea =
  [%css
  stylesheet
    {|
  fieldset.cusom-textarea-fieldset {
    width: fit-content;
    padding: 0px;
    margin: 0.6em 0.25em 0.25em;
    border: 1px solid var(--border);
    border-radius: 3px;
    overflow: hidden;
    background: var(--bg);
    display: flex;
    align-items: stretch;
  }

  fieldset.cusom-textarea-fieldset:focus-within {
      outline: var(--touch) solid 3px;
      outline-offset: -2px;
  }

  fieldset.cusom-textarea-fieldset:focus-within > legend {
    color: var(--touch);
  }

 fieldset.cusom-textarea-fieldset:focus-within > legend::before {
    content: "";
    display: block;
    position: absolute;
    background: var(--bg);
    width: 100%;
    height: 4px;
    left: 0px;
    top: calc(0.5em - 1px);
    z-index: 1;
}

  textarea.custom-textarea {
    border-radius: 0px;
    appearance: none;
    position: relative;
    background: transparent;
    border: 0px;
    outline: none;
    font-size: 1em;
    font-family: inherit;
    padding: 0.25em 0.25em 0.125em;
    z-index: 2;
  }

  legend.custom-textarea-legend {
    height: 0px;
    overflow: visible;
    position: relative;
    font-size: 0.8em;
    top: -0.55em;
    margin: 0px 0.25em;
    line-height: 1em;
    font-weight: bold;
    white-space: pre;
  }

  legend.custom-textarea-legend > span {
    position: relative;
    z-index: 3;
    margin: 0px -1px;
  }

  div.stack {
    position: relative;
    display: grid;
    margin: 0.25em 0.25em 0.125em;
    width: 100%;
  }

  div.stack > * {
    all: unset;
    grid-area: 1/1;
    /* opacity: 0.5; */
    white-space: pre;
    text-wrap: wrap;
    font-size: 0.8em !important;
  }

  div.stack > pre {
    visibility: hidden;
  }

  div.stack > textarea {
    overflow:clip;
  }
|}]

let touch = Css_gen.Color.to_string_css (`Hex "#1BA1F2")

let textarea theme ~attrs ~label ~value ~on_change =
  Vdom.Node.fieldset
    ~attrs:
      ([ Kado_textarea.cusom_textarea_fieldset
       ; Kado_textarea.Variables.set_all
           ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
           ~bg:(Css_gen.Color.to_string_css (View.extreme_colors theme).background)
           ~touch
       ; Label_modifications.muted_label
       ; Label_modifications.Variables.set_all
           ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
           ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
       ]
       @ attrs)
    [ (match label with
       | None -> Vdom.Node.none
       | Some label ->
         Vdom.Node.legend
           ~attrs:[ Kado_textarea.custom_textarea_legend ]
           [ Vdom.Node.span [ Vdom.Node.text label ] ])
    ; Vdom.Node.div
        ~attrs:[ Kado_textarea.stack ]
        [ Vdom.Node.pre [ Vdom.Node.text (value ^ ".") ]
        ; Vdom.Node.textarea
            ~attrs:
              [ Kado_textarea.custom_textarea
              ; Vdom.Attr.value_prop value
              ; Vdom.Attr.on_input (fun _ s -> on_change s)
              ; Vdom.Attr.on_click (fun evt ->
                  let open Js_of_ocaml in
                  let r = Js.Optdef.to_option evt##.which in
                  match r with
                  | Some Dom_html.Middle_button ->
                    Js.Opt.iter evt##.target (fun element ->
                      Js.Opt.iter (Dom_html.CoerceTo.textarea element) (fun textarea ->
                        textarea##select));
                    Effect.Prevent_default
                  | _ -> Effect.Ignore)
              ]
            []
        ]
    ]
;;

let textarea ?(attrs = []) ?label () =
  let%sub theme = View.Theme.current in
  let%sub state, set_state = Bonsai.state "" in
  let%sub id = Bonsai.path_id in
  let%arr theme = theme
  and state = state
  and set_state = set_state
  and id = id in
  let view = textarea theme ~attrs ~label ~value:state ~on_change:set_state in
  let form =
    Form.Expert.create ~value:(Ok state) ~set:set_state ~view:(Form.View.of_vdom ~id view)
  in
  form, view
;;
