open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

module Theme_id = struct
  type t =
    | Default
    | Kado
    | Kado_light
    | Kado_white_bg
  [@@deriving sexp, equal, enumerate, compare]
end

let theme_var ~default =
  Persistent_var.create
    (module Theme_id)
    `Local_storage
    ~unique_id:"bonsai-view-theme"
    ~default
;;

module Style =
  [%css
  stylesheet
    {|
      .container {
        padding: 0.5em 1em;
      }

      .container.standalone {
        position: fixed;
        padding: 0.5em 1em;
        width: fit-content;
        top: 0;
        background: var(--bg);
        border: 1px solid var(--border);
        border-top: 0;
        z-index: 1;
        border-bottom-left-radius: 3px;
        border-bottom-right-radius: 3px;
      }

      .container select {
        width: unset !important;
        font-size: inherit;
        padding: 0.2em 0.3em;
      }
      |}]

let component ?(default = Theme_id.Default) ?(standalone = false) () graph =
  let var = theme_var ~default in
  let var_value = Persistent_var.value var in
  let picker =
    Form.Elements.Dropdown.enumerable
      (module Theme_id)
      ~init:(`This var_value)
      ~to_string:(function
        | Default -> "Default"
        | Kado -> "Kado"
        | Kado_light -> "Kado (light)"
        | Kado_white_bg -> "Kado (light white background)")
      graph
  in
  let%sub () =
    Bonsai_extra.mirror
      ()
      ~sexp_of_model:[%sexp_of: Theme_id.t]
      ~equal:[%equal: Theme_id.t]
      ~store_value:var_value
      ~store_set:(Bonsai.return (Persistent_var.effect var))
      ~interactive_value:(picker >>| Form.value_or_default ~default:Theme_id.Default)
      ~interactive_set:(picker >>| Form.set)
      graph
  in
  let%arr picker_view = picker >>| Form.view
  and theme_id = var_value in
  let theme =
    match theme_id with
    | Default -> View.Expert.default_theme
    | Kado -> Kado.theme ~version:Bleeding ()
    | Kado_light -> Kado.theme ~style:Light ~version:Bleeding ()
    | Kado_white_bg ->
      View.Expert.override_constants
        (Kado.theme ~style:Light ~version:Bleeding ~set_min_height_to_100vh:() ())
        ~f:(fun constants ->
          { constants with
            primary = { constants.primary with background = `Name "white" }
          })
  in
  let vars =
    Style.Variables.set
      ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
      ~bg:(Css_gen.Color.to_string_css (View.primary_colors theme).background)
      ()
  in
  let view =
    View.hbox
      ~gap:(`Em 1)
      ~attrs:
        [ (if standalone then Style.standalone else Vdom.Attr.empty)
        ; Style.container
        ; vars
        ]
      (Vdom.Node.text "Pick a Theme" :: Form.View.to_vdom_plain picker_view)
  in
  theme, view
;;
