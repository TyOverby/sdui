open! Bonsai_web

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

let textarea theme ~attrs ~label ~value ~on_change ~on_blur =
  Vdom.Node.fieldset
    ~attrs:
      ([ Kado_textarea.cusom_textarea_fieldset
       ; Kado_textarea.Variables.set_all
           ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
           ~bg:(Css_gen.Color.to_string_css (View.extreme_colors theme).background)
           ~touch
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
              ; Vdom.Attr.on_blur (fun _ -> on_blur)
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
