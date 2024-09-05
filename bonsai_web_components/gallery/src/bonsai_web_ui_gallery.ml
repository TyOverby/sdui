open! Core

let lazy_deprecated = (Bonsai_web.Proc.Bonsai.lazy_ [@alert "-deprecated"])

open! Bonsai_web
open! Bonsai.Let_syntax
module Underlying_codemirror = Codemirror
module Codemirror = Bonsai_web_ui_codemirror
module Form = Bonsai_web_ui_form.With_automatic_view

module type Demo = sig
  val name : string
  val description : string
  val view : Bonsai.graph -> (Vdom.Node.t * string) Bonsai.t
  val selector : string option
  val filter_attrs : (string -> string -> bool) option
end

let if_empty_then_none constructor text =
  if String.is_empty (String.strip text)
  then Vdom.Node.None
  else constructor [ Vdom.Node.text text ]
;;

let wrap_application ?(attr = Bonsai.return Vdom.Attr.empty) ~theme_picker nodes graph =
  let theme = View.Theme.current graph in
  let%arr nodes = nodes
  and theme_picker = theme_picker
  and theme = theme
  and attr = attr in
  Vdom.Node.div
    ~attrs:
      [ Style.app
      ; Style.Variables.set
          ~bg:(Css_gen.Color.to_string_css (View.primary_colors theme).background)
          ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
          ~extreme_primary_border:
            (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
          ~extreme_bg:(Css_gen.Color.to_string_css (View.extreme_colors theme).background)
          ()
      ; attr
      ]
    [ theme_picker; Vdom.Node.div ~attrs:[ Style.container ] nodes ]
;;

let make_sections ~theme_picker sections graph =
  let app graph =
    Bonsai.all
      (List.map sections ~f:(fun (section_title, description, subsections) ->
         let subsections =
           Bonsai.all (List.map subsections ~f:(fun section -> section graph))
         in
         let%arr subsections = subsections in
         Vdom.Node.div
           [ if_empty_then_none Vdom.Node.h1 section_title
           ; if_empty_then_none Vdom.Node.p description
           ; Vdom.Node.div ~attrs:[ Style.section ] subsections
           ]))
  in
  wrap_application ~theme_picker (app graph) graph
;;

let codemirror ~language ~content graph =
  let open Underlying_codemirror in
  let with_conversion_of_bool b =
    With_conversion.create ~t_to_js:Gen_js_api.Ojs.bool_to_js b
  in
  let extensions =
    [ Gutter.highlight_active_line_gutter ()
    ; History.history (History.Config.create ())
    ; State.Facet.of_
        State.Editor_state.allow_multiple_selections
        (with_conversion_of_bool true)
    ; Highlight.syntax_highlighting
        ~options:(Highlight.Syntax_highlighting_options.create ~fallback:true ())
        Highlight.default_highlight_style
        ()
    ; State.Facet.of_ View.Editor_view.editable (with_conversion_of_bool false)
    ; View.Editor_view.line_wrapping
    ; language
    ]
  in
  let codemirror =
    let codemirror_theme =
      let%map.Bonsai theme = Bonsai_web.View.Theme.current graph in
      Bonsai_web.View.For_components.Codemirror.theme theme
    in
    Codemirror.with_dynamic_extensions
      (module struct
        type t = Codemirror_themes.t option [@@deriving equal, sexp]
      end)
      ~equal:[%equal: Codemirror_themes.t option]
      ~name:"codemirror for demo"
      codemirror_theme
      ~compute_extensions:
        (Bonsai.return (fun theme_opt ->
           let extra_extensions =
             match theme_opt with
             | None -> []
             | Some theme -> [ Codemirror_themes.get theme ]
           in
           extra_extensions @ extensions))
      ~initial_state:(State.Editor_state.create (State.Editor_state_config.create ()))
      graph
  in
  let () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      content
      ~callback:
        (let%map codemirror = codemirror in
         fun demo -> Codemirror.set_lines codemirror (String.split_lines demo))
      graph
  in
  codemirror
;;

type t =
  { code : Vdom.Node.t
  ; demo : Vdom.Node.t
  }

let make_demo'
  ?selector
  ?filter_attrs
  ?(ocaml_label = Some "OCaml")
  ?(hide_html = false)
  ~demo
  ~code
  ()
  graph
  =
  let filter_printed_attributes ~key:k ~data:v =
    (not (String.equal k "custom-css-vars"))
    && (Option.value filter_attrs ~default:(fun _ _ -> true)) k v
  in
  let pick_interesting_nodes =
    match selector with
    | None -> fun node -> [ node ]
    | Some selector ->
      fun node -> Virtual_dom_test_helpers.Node_helpers.select ~selector node
  in
  let ocaml_codemirror =
    codemirror
      ~content:code
      ~language:
        Underlying_codemirror.(
          Mllike.ocaml
          |> Stream_parser.Stream_language.define
          |> Stream_parser.Stream_language.to_language
          |> Language.extension)
      graph
  in
  let rendered_or_html =
    if hide_html
    then Bonsai.return (Form.return `Rendered)
    else
      Form.Elements.Radio_buttons.enumerable
        ~init:`Rendered
        ~layout:`Horizontal
        ~extra_container_attrs:(Bonsai.return [ Style.rendred_or_html_picker ])
        (module struct
          type t =
            [ `Rendered
            | `Html
            ]
          [@@deriving sexp, equal, compare, enumerate]
        end)
        graph
  in
  let display_which =
    match%sub rendered_or_html >>| Form.value_or_default ~default:`Rendered with
    | `Rendered -> demo
    | `Html ->
      lazy_deprecated
        (lazy
          (* If [make_demo (module M)] is called where [M.view] is a constant value, then
             we'll constant fold the entire `Html branch at compile time.

             The [to_string_html] logic is expensive (and this lives within a match%sub which
             defaults to not being visible), so we'd prefer to compute it when we actually
             render it. *)
          (fun graph ->
            let html_codemirror =
              let content =
                let%arr demo = demo in
                demo
                |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
                |> pick_interesting_nodes
                |> List.map
                     ~f:
                       (Virtual_dom_test_helpers.Node_helpers.to_string_html
                          ~filter_printed_attributes)
                |> String.concat ~sep:"\n"
              in
              codemirror
                ~content
                ~language:Underlying_codemirror.(Lang_html.html () |> Language.extension)
                graph
            in
            let%arr html_codemirror = html_codemirror in
            Bonsai_web_ui_codemirror.view html_codemirror))
        graph
  in
  let%arr ocaml_codemirror = ocaml_codemirror
  and display_which = display_which
  and rendered_or_html = rendered_or_html in
  let toggler = rendered_or_html |> Form.view |> Form.View.to_vdom_plain in
  let ocaml_code =
    let legend =
      match ocaml_label with
      | None -> Vdom.Node.none_deprecated [@alert "-deprecated"]
      | Some s -> Vdom.Node.create "legend" [ Vdom.Node.text s ]
    in
    Vdom.Node.fieldset
      ~attrs:[ Style.gallery_fieldset ]
      [ legend; Bonsai_web_ui_codemirror.view ocaml_codemirror ]
  in
  let output =
    let legend =
      match hide_html with
      | true -> Vdom.Node.none_deprecated [@alert "-deprecated"]
      | false -> Vdom.Node.create "legend" toggler
    in
    Vdom.Node.fieldset
      ~attrs:[ Style.output; Style.gallery_fieldset ]
      [ legend; display_which ]
  in
  { code = ocaml_code; demo = output }
;;

let make_demo (module M : Demo) graph =
  let%sub demo, code = M.view graph in
  let code_and_demo =
    make_demo' ?filter_attrs:M.filter_attrs ?selector:M.selector ~demo ~code () graph
  in
  let%arr { code; demo } = code_and_demo in
  Vdom.Node.div
    [ if_empty_then_none Vdom.Node.h2 M.name
    ; if_empty_then_none Vdom.Node.p M.description
    ; code
    ; demo
    ]
;;

module Theme_picker = Theme_picker
