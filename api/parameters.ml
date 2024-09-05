open! Core
open! Bonsai_web
open! Async_kernel
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

type t =
  ( Txt2img.Query.t
    , on_submit:unit Effect.t -> hosts_panel:Vdom.Node.t -> Vdom.Node.t )
    Form.t

module Size_presets = struct
  module Style =
    [%css
    stylesheet
      {|
      .preset {
        background: var(--bg);
        align-self: center;
        border-radius:3px;
        margin: 3px;
        border:1px solid var(--border);
        cursor: pointer;
      }
      .preset:hover {
        border-color: var(--touch);
        background: color-mix(in oklab, var(--bg) 80%, var(--touch))
      }

      .landscape {
        width: 32px;
        height: 20px;
      }
      .small-square {
        width: 20px;
        height: 20px;
      }
      .big-square {
        width: 32px;
        height: 32px;
      }
      .portrait{
        height: 32px;
        width: 20px;
      }
    |}]

  let view theme ~set_width ~set_height =
    let colors =
      Style.Variables.set_all
        ~border:(Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
        ~bg:(Css_gen.Color.to_string_css (View.extreme_colors theme).background)
        ~touch:(Css_gen.Color.to_string_css (`Hex "#1BA1F2"))
    in
    let set_dims w h =
      Vdom.Attr.on_click (fun _ ->
        Effect.Many [ set_width (Int63.of_int w); set_height (Int63.of_int h) ])
    in
    [ Vdom.Node.div
        ~attrs:[ Style.preset; Style.small_square; colors; set_dims 512 512 ]
        []
    ; Vdom.Node.div ~attrs:[ Style.preset; Style.landscape; colors; set_dims 800 512 ] []
    ; Vdom.Node.div ~attrs:[ Style.preset; Style.portrait; colors; set_dims 512 800 ] []
    ; Vdom.Node.div ~attrs:[ Style.preset; Style.big_square; colors; set_dims 800 800 ] []
    ]
  ;;
end

module Top_bar =
  [%css
  stylesheet
    {|
    .container {
      padding: 0.25em;
      padding-bottom: 0.5em;
      margin-bottom: 0.5em;
      position:sticky; 
      top:0;
      border-bottom: 1px solid color-mix(in srgb, transparent 50%, var(--border));
      z-index: 1;
      background: var(--bg);
      backdrop-filter: blur(5px);
    } 

    .collapse-button {
      left: 50%;
      bottom: 0;
      background: none;
      border: 0;
    }
  |}]

module Submit_button = struct
  module Style =
    [%css
    stylesheet
      {|
    .button {
    font-size: 0.8em;
    align-self:center;
    text-transform: uppercase;
    width: 100%;
    color: white;
    text-align: center;
    margin: 3px;
    } 
  |}]

  let make theme ~on_submit =
    View.button theme ~intent:Info ~attrs:[ Style.button ] ~on_click:on_submit "generate"
  ;;
end

module Upscale_style =
  [%css
  stylesheet
    {|
    .upscale_style {
      display:flex;
      flex-direction:column;
      border: 1px solid var(--border);
      border-radius: 3px;
    } 
  |}]

let multiple_of_8 s =
  match Int63.of_string_opt s with
  | None -> Error (Int63.of_int 512)
  | Some i when Int63.(i < of_int 128) -> Error (Int63.of_int 128)
  | Some i ->
    if Int63.(i % of_int 8 = of_int 0)
    then Ok i
    else Error Int63.(i + (of_int 8 - (i % of_int 8)))
;;

let between_inclusive s ~min:min_v ~max:max_v =
  match Int63.of_string_opt s with
  | None -> Error (Int63.of_int 512)
  | Some i when Int63.(i < min_v) -> Error min_v
  | Some i when Int63.(i > max_v) -> Error max_v
  | Some i -> Ok i
;;

let validate_prompt s = s

let _validate_prompt prompt =
  let translate_line line =
    line
    |> String.split ~on:','
    |> List.map ~f:String.strip
    |> List.filter_map ~f:(function
      | "" -> None
      | s -> Some s)
    |> String.concat ~sep:", "
    |> function
    | "" -> ""
    | s -> s ^ ","
  in
  prompt |> String.split_lines |> List.map ~f:translate_line |> String.concat ~sep:"\n"
;;

module Individual = struct
  let width_height_form ?(default = 512) ~label graph =
    Custom_form_elements.int_form
      ~title:label
      ~step:8
      ~default:(Int63.of_int default)
      ~validate_or_correct:multiple_of_8
      ~length:(`Em 4)
      ~min:(Int63.of_int 128)
      ~max:(Int63.of_int 2048)
      ~input_attrs:[ Vdom.Attr.create "data-kind" label ]
      graph
  ;;

  let min_1_form ~default ~max ~label graph =
    let min = Int63.of_int 1 in
    let max = Int63.of_int max in
    Custom_form_elements.int_form
      ~title:label
      ~step:1
      ~default
      ~validate_or_correct:(between_inclusive ~min ~max)
      ~length:(`Em 3)
      ~min
      ~max
      graph
  ;;

  let seed_form ?container_attrs graph =
    Custom_form_elements.int_form
      ~title:"seed"
      ~step:1
      ~default:(Int63.of_int 0)
      ~validate_or_correct:(fun s ->
        match Int63.of_string_opt s with
        | None -> Error (Int63.of_int (-1))
        | Some i -> Ok i)
      ~length:`Initial
      ~min:Int63.zero
      ~max:(Int63.of_int 1_000_000)
      ?container_attrs
      ~input_attrs:[ Vdom.Attr.style (Css_gen.font_family [ "monospace" ]) ]
      graph
  ;;

  let prompt_form ?default ?textarea_attrs ?container_attrs ~label graph =
    Custom_form_elements.textarea
      ~validate:validate_prompt
      ?default
      ?container_attrs
      ?textarea_attrs
      ~label
      graph
    >>| Form.map_view ~f:(fun view -> view ?colorize:None ())
  ;;
end

open Individual

let component
  ~(hosts : Hosts.t Bonsai.t)
  ~(available_hosts : Hosts.Host.Set.t Bonsai.t)
  graph
  =
  let seed_form = seed_form graph in
  let width_form = width_height_form ~label:"width" graph in
  let height_form = width_height_form ~label:"height" graph in
  let data_url_form =
    Custom_form_elements.textarea
      ~validate:Fn.id
      ~container_attrs:
        [ Vdom.Attr.style (Css_gen.max_width (`Px 30))
        ; Vdom.Attr.style (Css_gen.max_height (`Px 100))
        ; Vdom.Attr.style (Css_gen.min_width (`Px 100))
        ; Vdom.Attr.style (Css_gen.width (`Px 100))
        ; Vdom.Attr.style (Css_gen.display `None)
        ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "data-url" ]
      ~label:"data url"
      graph
  in
  let positive_prompt_form =
    prompt_form
      ~container_attrs:[ Vdom.Attr.style (Css_gen.flex_item ~grow:4.0 ()) ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "prompt" ]
      ~label:"positive prompt"
      graph
  in
  let negative_prompt_form = prompt_form ~label:"negative prompt" graph in
  let sampling_steps_form =
    min_1_form ~default:(Int63.of_int 25) ~max:150 ~label:"steps" graph
  in
  let cfg_scale_form = min_1_form ~default:(Int63.of_int 7) ~max:30 ~label:"cfg" graph in
  let%sub sampler_form, sampler_form_view = Samplers.form ~hosts graph in
  let upscaler_form = Upscaler.form ~hosts graph in
  let%sub styles_form = Styles.form ~hosts graph in
  let hr_form = Custom_form_elements.bool_form ~title:"upscale" ~default:false graph in
  let%sub denoising_strength =
    Form.Elements.Range.float
      ~allow_updates_when_focused:`Always
      ~min:0.0
      ~max:1.0
      ~step:0.01
      ~default:0.7
      ~extra_attrs:
        (Bonsai.return [ Vdom.Attr.style (Css_gen.create ~field:"flex-grow" ~value:"1") ])
      ()
      graph
  in
  let models_form = Models.form ~hosts ~available_hosts graph in
  let form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Txt2img.Query.Typed_field

        type field_view = unit
        type resulting_view = unit

        let strip_view (type a v) (form : (a, v) Form.t) : (a, unit) Form.t =
          Form.map_view form ~f:(Fn.const ())
        ;;

        type form_of_field_fn = { f : 'a. 'a Typed_field.t -> ('a, unit) Form.t Bonsai.t }

        let finalize_view _ _ = Bonsai.return ()

        let form_for_field
          : type a. a Typed_field.t -> Bonsai.graph -> (a, unit) Form.t Bonsai.t
          =
          fun field _graph ->
          match field with
          | Prompt -> positive_prompt_form >>| strip_view
          | Negative_prompt -> negative_prompt_form >>| strip_view
          | Width -> width_form >>| strip_view
          | Height -> height_form >>| strip_view
          | Cfg_scale -> cfg_scale_form >>| strip_view
          | Steps -> sampling_steps_form >>| strip_view
          | Sampler -> sampler_form >>| strip_view
          | Seed -> seed_form >>| strip_view
          | Subseed_strength -> Bonsai.return (Form.return 0.0)
          | Regional_prompter -> Bonsai.return (Form.return None)
          | Styles -> styles_form >>| strip_view
          | Enable_hr -> hr_form >>| strip_view
          | Ctrlnet ->
            data_url_form
            >>| strip_view
            >>| Form.project
                  ~parse_exn:(fun s ->
                    if String.for_all s ~f:Char.is_whitespace
                    then None
                    else Some { Alwayson_scripts.Ctrlnet.Query.image = s })
                  ~unparse:(function
                    | None -> ""
                    | Some { Alwayson_scripts.Ctrlnet.Query.image } -> image)
          | Hr_upscaler -> upscaler_form >>| strip_view
          | Denoising_strength -> denoising_strength >>| strip_view
        ;;
      end)
      graph
  in
  let theme = View.Theme.current graph in
  let collapsed, toggle_collapsed = Bonsai.toggle ~default_model:false graph in
  let form_view =
    let%arr { view = width; _ } = width_form
    and { view = height; _ } = height_form
    and width_form = width_form
    and height_form = height_form
    and { view = positive_prompt_view; _ } = positive_prompt_form
    and { view = negative_prompt_view; _ } = negative_prompt_form
    and { view = cfg_scale_view; _ } = cfg_scale_form
    and { view = sampling_steps_view; _ } = sampling_steps_form
    and { view = seed_form_view; _ } = seed_form
    and { view = upscaler_form_view; _ } = upscaler_form
    and theme = theme
    and sampler = sampler_form_view
    and styles_form = styles_form
    and { view = hr_form_view; _ } = hr_form
    and collapsed = collapsed
    and { view = data_url_view; _ } = data_url_form
    and toggle_collapsed = toggle_collapsed
    and denoising_strength = denoising_strength
    and { view = models_form_view; _ } = models_form in
    fun ~on_submit ~hosts_panel ->
      let hijack_ctrl_enter =
        Vdom.Attr.on_keypress (fun evt ->
          let open Js_of_ocaml in
          match Option.map (Js.Optdef.to_option evt##.code) ~f:Js.to_string with
          | Some "Enter" when Js.to_bool evt##.ctrlKey ->
            Effect.Many [ on_submit; Effect.Prevent_default ]
          | _ -> Effect.Ignore)
      in
      let form_elements =
        let attrs =
          if collapsed
          then [ Vdom.Attr.style (Css_gen.display `None) ]
          else
            [ hijack_ctrl_enter
            ; Top_bar.container
            ; Top_bar.Variables.set_all
                ~border:
                  (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
                ~bg:(Css_gen.Color.to_string_css (View.primary_colors theme).background)
            ]
        in
        View.hbox
          ~attrs
          [ positive_prompt_view
          ; data_url_view ()
          ; negative_prompt_view
          ; hosts_panel
          ; View.vbox
              ~cross_axis_alignment:Stretch
              [ View.hbox
                  ~main_axis_alignment:Space_between
                  (Size_presets.view
                     theme
                     ~set_width:(Form.set width_form)
                     ~set_height:(Form.set height_form)
                   @ [ width; height ])
              ; View.hbox
                  ~main_axis_alignment:Space_between
                  [ sampler; cfg_scale_view; sampling_steps_view ]
              ; View.hbox
                  ~main_axis_alignment:Space_between
                  [ seed_form_view; Submit_button.make theme ~on_submit ]
              ; models_form_view
              ; styles_form |> Form.view
              ; Vdom.Node.fieldset
                  ~attrs:
                    [ Upscale_style.upscale_style
                    ; Upscale_style.Variables.set_all
                        ~border:
                          (View.extreme_primary_border_color theme
                           |> Css_gen.Color.to_string_css)
                    ]
                  [ Vdom.Node.legend [ hr_form_view ]
                  ; upscaler_form_view
                  ; View.hbox
                      ~main_axis_alignment:Space_between
                      ~cross_axis_alignment:Stretch
                      ~gap:(`Em 1)
                      [ Vdom.Node.span [ Vdom.Node.text "denoise str" ]
                      ; Form.view denoising_strength
                      ]
                  ]
              ]
          ]
      in
      View.vbox
        ~attrs:[ Vdom.Attr.style (Css_gen.position `Relative) ]
        [ form_elements
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.on_click (fun _ -> toggle_collapsed); Top_bar.collapse_button ]
            [ Vdom.Node.text "^^^" ]
        ]
  in
  let query =
    let%arr form = form
    and form_view = form_view in
    { Form.value = form.value; set = form.set; view = form_view }
  in
  query, models_form
;;
