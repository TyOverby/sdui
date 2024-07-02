open! Core
open! Bonsai_web.Cont
open! Async_kernel
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

type t =
  { form : Txt2img.Query.t Form.t
  ; form_view : on_submit:unit Effect.t -> hosts_panel:Vdom.Node.t -> Vdom.Node.t
  }

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

let validate_prompt prompt =
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

let component ~(request_host : Hosts.request_host Bonsai.t) ~available_hosts graph =
  let width_height_form title =
    Custom_form_elements.int_form
      ~title
      ~step:8
      ~default:(Int63.of_int 512)
      ~validate_or_correct:multiple_of_8
      ~length:(`Em 4)
      ~min:(Int63.of_int 128)
      ~max:(Int63.of_int 2048)
      ~input_attrs:[ Vdom.Attr.create "data-kind" title ]
      graph
  in
  let min_1_form ~default ~max title =
    let min = Int63.of_int 1 in
    let max = Int63.of_int max in
    Custom_form_elements.int_form
      ~title
      ~step:1
      ~default
      ~validate_or_correct:(between_inclusive ~min ~max)
      ~length:(`Em 3)
      ~min
      ~max
      graph
  in
  let%sub seed_form, seed_form_view =
    Custom_form_elements.int_form
      ~title:"seed"
      ~step:1
      ~default:(Int63.of_int (-1))
      ~validate_or_correct:(fun s ->
        match Int63.of_string_opt s with
        | None -> Error (Int63.of_int (-1))
        | Some i -> Ok i)
      ~length:(`Em 11)
      ~min:Int63.min_value
      ~max:Int63.max_value
      ~input_attrs:[ Vdom.Attr.style (Css_gen.font_family [ "monospace" ]) ]
      graph
  in
  let%sub width_form, width_form_view = width_height_form "width" in
  let%sub height_form, height_form_view = width_height_form "height" in
  let%sub data_url, data_url_view =
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
  let%sub positive_prompt, positive_prompt_view =
    Custom_form_elements.textarea
      ~validate:validate_prompt
      ~container_attrs:[ Vdom.Attr.style (Css_gen.flex_item ~grow:4.0 ()) ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "prompt" ]
      ~label:"positive prompt"
      graph
  in
  let%sub negative_prompt, negative_prompt_view =
    Custom_form_elements.textarea ~validate:validate_prompt ~label:"negative prompt" graph
  in
  let%sub sampling_steps, sampling_steps_view =
    min_1_form ~default:(Int63.of_int 25) ~max:150 "steps"
  in
  let%sub cfg_scale, cfg_scale_view =
    min_1_form ~default:(Int63.of_int 7) ~max:30 "cfg"
  in
  let%sub sampler_form, sampler_form_view = Samplers.form ~request_host graph in
  let%sub upscaler_form, upscaler_form_view = Upscaler.form ~request_host graph in
  let%sub styles_form = Styles.form ~request_host graph in
  let%sub hr_form, hr_form_view =
    Custom_form_elements.bool_form ~title:"upscale" ~default:false graph
  in
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
  let%sub _models_form, models_form_view =
    Models.form ~request_host ~available_hosts graph
  in
  let form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Txt2img.Query.Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun field _graph -> match field with
          | Prompt -> positive_prompt
          | Negative_prompt -> negative_prompt
          | Width -> width_form
          | Height -> height_form
          | Cfg_scale -> cfg_scale
          | Steps -> sampling_steps
          | Sampler -> sampler_form
          | Seed -> seed_form
          | Subseed_strength -> Bonsai.return (Form.return 0.0)
          | Styles -> styles_form
          | Enable_hr -> hr_form
          | Data_url -> data_url
          | Hr_upscaler -> upscaler_form
          | Denoising_strength -> denoising_strength 
        ;;
      end)
      graph
  in
  let theme = View.Theme.current graph in
  let collapsed, toggle_collapsed = Bonsai.toggle ~default_model:false graph in
  let%sub form_view =
    let%arr width = width_form_view
    and height = height_form_view
    and width_form = width_form
    and height_form = height_form
    and positive_prompt_view = positive_prompt_view
    and negative_prompt_view = negative_prompt_view
    and cfg_scale_view = cfg_scale_view
    and sampling_steps_view = sampling_steps_view
    and seed_form_view = seed_form_view
    and theme = theme
    and sampler = sampler_form_view
    and styles_form = styles_form
    and hr_form_view = hr_form_view
    and collapsed = collapsed
    and data_url_view = data_url_view
    and toggle_collapsed = toggle_collapsed
    and upscaler_form_view = upscaler_form_view
    and denoising_strength = denoising_strength
    and models_form_view = models_form_view in
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
          [ positive_prompt_view ()
          ; data_url_view ()
          ; negative_prompt_view ()
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
              ; View.hbox (styles_form |> Form.view |> Form.View.to_vdom_plain)
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
                      (Vdom.Node.span [ Vdom.Node.text "denoise str" ]
                       :: (Form.view denoising_strength |> Form.View.to_vdom_plain))
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
  let%arr form = form
  and form_view = form_view in
  { form; form_view }
;;
