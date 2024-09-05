open! Core
open Js_of_ocaml
open Codemirror
open Gen_js_api

module Create_theme = struct
  let f ~dark ~theme_name ~highlight_name =
    let spec = Js.Unsafe.get Js.Unsafe.global (Js.string theme_name) in
    let bg_extension =
      View.Editor_view.theme
        ~spec
        ~options:(View.Editor_view.Theme_options.create ~dark ())
        ()
    in
    let specs : unit -> Ojs.t Js.js_array Js.t =
      fun () ->
      Js.Unsafe.fun_call (Js.Unsafe.get Js.Unsafe.global (Js.string highlight_name)) [||]
    in
    let fg_extension =
      Highlight.syntax_highlighting
        (Highlight.Highlight_style.define
           ~specs:(Js.to_array (specs ()) |> Array.to_list))
        ()
    in
    let dark_theme_facet =
      State.Facet.of_
        View.Editor_view.dark_theme
        (With_conversion.create ~t_to_js:Gen_js_api.Ojs.bool_to_js true)
    in
    State.Extension.of_list [ bg_extension; fg_extension; dark_theme_facet ]
  ;;
end

let basic_dark =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_basicDarkTheme"
       ~highlight_name:"cm6_themes_basicDarkHighlightStyle")
;;

let basic_light =
  lazy
    (Create_theme.f
       ~dark:false
       ~theme_name:"cm6_themes_basicLightTheme"
       ~highlight_name:"cm6_themes_basicLightHighlightStyle")
;;

let gruvbox_dark =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_gruvboxDarkTheme"
       ~highlight_name:"cm6_themes_gruvboxDarkHighlightStyle")
;;

let nord =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_nordTheme"
       ~highlight_name:"cm6_themes_nordHighlightStyle")
;;

let solarized_dark =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_solarizedDarkTheme"
       ~highlight_name:"cm6_themes_solarizedDarkHighlightStyle")
;;

let solarized_light =
  lazy
    (Create_theme.f
       ~dark:false
       ~theme_name:"cm6_themes_solarizedLightTheme"
       ~highlight_name:"cm6_themes_solarizedLightHighlightStyle")
;;

let material_dark =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_materialDarkTheme"
       ~highlight_name:"cm6_themes_materialDarkHighlightStyle")
;;

let vscode_dark =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_vscodeDarkTheme"
       ~highlight_name:"cm6_themes_vscodeDarkHighlightStyle")
;;

let vscode_light =
  lazy
    (Create_theme.f
       ~dark:true
       ~theme_name:"cm6_themes_vscodeLightTheme"
       ~highlight_name:"cm6_themes_vscodeLightHighlightStyle")
;;

module Stable = struct
  module V1 = struct
    type t = Bonsai_web_ui_view.Expert.For_codemirror.Theme.t =
      | Basic_dark
      | Basic_light
      | Gruvbox_dark
      | Nord
      | Solarized_dark
      | Solarized_light
      | Material_dark
      | Vscode_dark
      | Vscode_light
    [@@deriving bin_io, compare, enumerate, equal, sexp, sexp_grammar, typed_variants]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 0860df590b807beed95f9c1cef1b6a8f |}]
    ;;
  end
end

include Stable.V1

let get = function
  | Basic_dark -> Lazy.force basic_dark
  | Basic_light -> Lazy.force basic_light
  | Gruvbox_dark -> Lazy.force gruvbox_dark
  | Nord -> Lazy.force nord
  | Solarized_dark -> Lazy.force solarized_dark
  | Solarized_light -> Lazy.force solarized_light
  | Material_dark -> Lazy.force material_dark
  | Vscode_dark -> Lazy.force vscode_dark
  | Vscode_light -> Lazy.force vscode_light
;;

let to_string = function
  | Basic_dark -> "basic dark"
  | Basic_light -> "basic light"
  | Gruvbox_dark -> "gruvbox dark"
  | Nord -> "nord"
  | Solarized_dark -> "solarized dark"
  | Solarized_light -> "solarized light"
  | Material_dark -> "material dark"
  | Vscode_dark -> "vscode dark"
  | Vscode_light -> "vscode light"
;;
