open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module Label_modifications =
  [%css
  stylesheet
    {|
  input, select, button, .checkbox-container {
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
  ?(container_attrs = fun ~state:_ ~set_state:_ -> [])
  ~title
  ~default
  ~step
  ~length
  ~min
  ~max
  ~(validate_or_correct : string -> (Int63.t, Int63.t) Result.t)
  graph
  =
  let%sub theme = View.Theme.current graph in
  let state, set_state = Bonsai.state (Int63.to_string default) graph in
  let%arr theme = theme
  and state = state
  and set_state = set_state in
  let value_or_corrected = validate_or_correct state in
  let is_error = Result.is_ok value_or_corrected in
  let fix_on_blur =
    match value_or_corrected with
    | Ok _ -> Vdom.Attr.empty
    | Error corrected ->
      Vdom.Attr.on_blur (fun _ -> set_state (Int63.to_string corrected))
  in
  let value =
    match value_or_corrected with
    | Ok v -> v
    | Error v -> v
  in
  let set i =
    set_state
      (Int63.to_string
         (match validate_or_correct (Int63.to_string i) with
          | Ok v -> v
          | Error v -> v))
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
            @ container_attrs ~state:value ~set_state:set))
      ~title:(Some title)
      ~on_change:set_state
      ~value:state
  in
  { Form.value = Ok value; set; view }
;;

let textarea
  ?colorize
  theme
  ~container_attrs
  ~textarea_attrs
  ~label
  ~value
  ~on_change
  ~on_blur
  =
  let extra_attrs =
    [ Label_modifications.muted_label
    ; Label_modifications.Variables.set_all
        ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
        ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
    ]
  in
  Shared.Raw_textarea.textarea
    ?colorize
    theme
    ~container_attrs:(container_attrs @ extra_attrs)
    ~textarea_attrs
    ~label
    ~value
    ~on_change
    ~on_blur
;;

let textarea ?validate ?(container_attrs = []) ?(textarea_attrs = []) ?label graph =
  let theme = View.Theme.current graph in
  let state, set_state = Bonsai.state "" graph in
  let%arr theme = theme
  and state = state
  and set_state = set_state in
  let on_blur =
    match validate with
    | None -> Effect.Ignore
    | Some f -> Effect.lazy_ (lazy (set_state (f state)))
  in
  let view ?colorize () =
    textarea
      theme
      ?colorize
      ~container_attrs
      ~textarea_attrs
      ~label
      ~value:state
      ~on_change:set_state
      ~on_blur
  in
  let value =
    match validate with
    | None -> state
    | Some f -> f state
  in
  { Form.value = Ok value; set = set_state; view }
;;

let bool_form ?(input_attrs = []) ?(container_attrs = []) ~title ~default graph =
  let theme = View.Theme.current graph in
  let state, set_state = Bonsai.state default graph in
  let%arr theme = theme
  and state = state
  and set_state = set_state in
  let view =
    Kado.Unstable.Input.checkbox
      ~constants:(View.constants theme)
      ~input_attr:(Vdom.Attr.many input_attrs)
      ~container_attr:
        (Vdom.Attr.many ([ Label_modifications.checkbox_container ] @ container_attrs))
      ~label:(Vdom.Node.text title)
      ~on_change:set_state
      ~checked:state
  in
  { Form.value = Ok state; set = set_state; view }
;;
