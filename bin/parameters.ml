open! Core
open! Bonsai_web
open! Async_kernel
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

type t =
  { form : Txt2img.Query.t Form.t
  ; form_view : on_submit:unit Effect.t -> Vdom.Node.t
  ; width : int
  ; height : int
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
      Vdom.Attr.on_click (fun _ -> Effect.Many [ set_width w; set_height h ])
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

let multiple_of_8 s =
  match Int.of_string_opt s with
  | None -> Error 512
  | Some i when i < 128 -> Error 128
  | Some i -> if i mod 8 = 0 then Ok i else Error (i + (8 - (i mod 8)))
;;

let between_inclusive s ~min ~max =
  match Int.of_string_opt s with
  | None -> Error 512
  | Some i when i < min -> Error min
  | Some i when i > max -> Error max
  | Some i -> Ok i
;;

let component ~host_and_port =
  let width_height_form title =
    Custom_form_elements.int_form
      ~title
      ~step:8
      ~default:512
      ~validate_or_correct:multiple_of_8
      ~length:(`Em 4)
      ~min:128
      ~max:2048
      ()
  in
  let min_1_form ~default ~max title =
    Custom_form_elements.int_form
      ~title
      ~step:1
      ~default
      ~validate_or_correct:(between_inclusive ~min:1 ~max)
      ~length:(`Em 3)
      ~min:1
      ~max
      ()
  in
  let%sub seed_form, seed_form_view =
    Custom_form_elements.int_form
      ~title:"seed"
      ~step:1
      ~default:(-1)
      ~validate_or_correct:(fun s ->
        match Int.of_string_opt s with
        | None -> Error (-1)
        | Some i -> Ok i)
      ~length:(`Em 11)
      ~min:(Int.of_int32_trunc Int32.min_value)
      ~max:(Int.of_int32_trunc Int32.max_value)
      ~input_attrs:[ Vdom.Attr.style (Css_gen.font_family [ "monospace" ]) ]
      ()
  in
  let%sub width_form, width_form_view = width_height_form "width" in
  let%sub height_form, height_form_view = width_height_form "height" in
  let%sub positive_prompt, positive_prompt_view =
    Custom_form_elements.textarea
      ~attrs:[ Vdom.Attr.style (Css_gen.flex_item ~grow:4.0 ()) ]
      ~label:"positive prompt"
      ()
  in
  let%sub negative_prompt, negative_prompt_view =
    Custom_form_elements.textarea
      ~attrs:[ Vdom.Attr.style (Css_gen.flex_item ~grow:2.0 ()) ]
      ~label:"negative prompt"
      ()
  in
  let%sub sampling_steps, sampling_steps_view = min_1_form ~default:25 ~max:150 "steps" in
  let%sub cfg_scale, cfg_scale_view = min_1_form ~default:7 ~max:30 "cfg" in
  let%sub sampler_form, sampler_form_view = Samplers.form ~host_and_port in
  let%sub form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Txt2img.Query.Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | Prompt -> return positive_prompt
          | Negative_prompt -> return negative_prompt
          | Width -> return width_form
          | Height -> return height_form
          | Cfg_scale -> return cfg_scale
          | Steps -> return sampling_steps
          | Sampler -> return sampler_form
          | Seed -> return seed_form
        ;;
      end)
  in
  let%sub theme = View.Theme.current in
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
    and sampler = sampler_form_view in
    fun ~on_submit ->
      let hijack_ctrl_enter =
        Vdom.Attr.on_keypress (fun evt ->
          let open Js_of_ocaml in
          match Option.map (Js.Optdef.to_option evt##.code) ~f:Js.to_string with
          | Some "Enter" when Js.to_bool evt##.ctrlKey ->
            Effect.Many [ on_submit; Effect.Prevent_default ]
          | _ -> Effect.Ignore)
      in
      View.hbox
        ~attrs:[ hijack_ctrl_enter ]
        [ positive_prompt_view
        ; negative_prompt_view
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
            ]
        ]
  in
  let%arr width = width_form
  and height = height_form
  and form = form
  and form_view = form_view in
  let width = Form.value_or_default width ~default:128 in
  let height = Form.value_or_default height ~default:128 in
  { form; form_view; width; height }
;;
