open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module P = Sd.Parameters.Individual

type t =
  { seed : Int63.t
  ; pos_prompt : string
  ; neg_prompt : string
  ; width : Int63.t
  ; height : Int63.t
  ; steps : Int63.t
  ; cfg : Int63.t
  ; denoise : Int63.t
  ; ratios : string
  ; num_images : int
  }
[@@deriving typed_fields]

let num_images form =
  match Form.value form with
  | Ok { num_images; _ } -> num_images
  | Error _ -> 1
;;

let component graph =
  let is_localhost =
    String.equal
      "localhost"
      (Js_of_ocaml.Js.to_string Js_of_ocaml.Dom_html.window##.location##.hostname)
  in
  let default_size = if is_localhost then 128 else 256 in
  let seed =
    P.seed_form
      ~container_attrs:(fun ~state ~set_state ->
        [ Vdom.Attr.on_double_click (fun _ -> set_state Int63.(state + of_int 4)) ])
      graph
  and pos_prompt =
    P.prompt_form
      ~default:"score_9, score_8_up, score_7_up,\n"
      ~container_attrs:[ {%css| flex-grow: 2 |} ]
      ~label:"prompt"
      graph
  and neg_prompt =
    P.prompt_form
      ~default:"score_1, score_2, score_3,\n"
      ~container_attrs:[ {%css| flex-grow: 1 |} ]
      ~label:"negative"
      graph
  and width = P.width_height_form ~default:default_size ~label:"width" graph
  and height = P.width_height_form ~default:default_size ~label:"height" graph
  and steps =
    P.min_1_form
      ~default:(Int63.of_int (if is_localhost then 1 else 25))
      ~max:150
      ~label:"steps"
      graph
  and cfg = P.min_1_form ~default:(Int63.of_int 7) ~max:30 ~label:"cfg" graph
  and denoise = P.min_1_form ~default:(Int63.of_int 70) ~max:100 ~label:"deno" graph
  and num_images =
    P.min_1_form
      ~default:(Int63.of_int (if is_localhost then 1 else 4))
      ~max:25
      ~label:"# imgs"
      graph
    >>| Form.project ~parse_exn:Int63.to_int_exn ~unparse:Int63.of_int
  and ratios =
    Sd.Custom_form_elements.textarea ~label:"ratios" graph
    >>| Form.map_view ~f:(fun view -> view ?colorize:None ())
  in
  Form.Typed.Record.make
    (module struct
      module Typed_field = Typed_field

      type field_view = Vdom.Node.t

      type resulting_view =
        direction:[ `Vertical | `Horizontal ]
        -> theme:View.Theme.t
        -> reset:unit Effect.t
        -> Vdom.Node.t

      type form_of_field_fn =
        { f : 'a. 'a Typed_field.t -> ('a, Vdom.Node.t) Form.t Bonsai.t }

      let form_for_field (type a) (field : a Typed_field.t) _graph
        : (a, Vdom.Node.t) Form.t Bonsai.t
        =
        match field with
        | Seed -> seed
        | Pos_prompt -> pos_prompt
        | Neg_prompt -> neg_prompt
        | Width -> width
        | Height -> height
        | Steps -> steps
        | Cfg -> cfg
        | Denoise -> denoise
        | Ratios -> ratios
        | Num_images -> num_images
      ;;

      let finalize_view { f } _graph =
        let%arr width = f Width
        and height = f Height
        and steps = f Steps
        and cfg = f Cfg
        and denoise = f Denoise
        and seed = f Seed
        and ratios = f Ratios
        and pos_prompt = f Pos_prompt
        and num_images = f Num_images
        and neg_prompt = f Neg_prompt in
        fun ~direction ~theme ~reset ->
          let vbox, hbox =
            match direction with
            | `Horizontal -> (fun a -> View.vbox a), fun a -> View.hbox a
            | `Vertical -> (fun a -> View.hbox a), fun a -> View.vbox a
          in
          let size_modification s ~f =
            let on_click =
              let modify form =
                Form.value_or_default form ~default:Int63.zero |> f |> Form.set form
              in
              Effect.Many [ modify width; modify height ]
            in
            View.button theme s ~on_click
          in
          let two_x_button = size_modification "2" ~f:Int63.(( * ) (of_int 2)) in
          let div2_button = size_modification "1/2" ~f:Int63.(fun x -> x / of_int 2) in
          let div3_button = size_modification "1/3" ~f:Int63.(fun x -> x / of_int 3) in
          Vdom.Node.div
            [ vbox
                [ hbox
                    [ View.button theme "reset" ~on_click:reset
                    ; vbox
                        [ Form.view width
                        ; Form.view height
                        ; two_x_button
                        ; div2_button
                        ; div3_button
                        ]
                    ; vbox [ Form.view steps; Form.view cfg ]
                    ; vbox [ Form.view denoise; Form.view seed ]
                    ; Form.view num_images
                    ; Form.view ratios
                    ; Form.view pos_prompt
                    ; Form.view neg_prompt
                    ]
                ]
            ]
      ;;
    end)
    graph
;;

let for_img2img t =
  let { seed
      ; pos_prompt
      ; neg_prompt
      ; width
      ; height
      ; steps
      ; cfg
      ; denoise
      ; ratios = _
      ; num_images = _
      }
    =
    t
  in
  let denoising_strength = Int63.to_float denoise /. 100.0 in
  { Sd.Img2img.Query.init_images = []
  ; mask = None
  ; prompt = pos_prompt
  ; negative_prompt = neg_prompt
  ; width
  ; height
  ; steps
  ; cfg_scale = cfg
  ; seed
  ; denoising_strength
  ; sampler = Sd.Samplers.default
  ; subseed_strength = 0.0
  ; styles = Sd.Styles.none
  }
;;

let for_txt2img t =
  let { seed
      ; pos_prompt
      ; neg_prompt
      ; width
      ; height
      ; steps
      ; cfg
      ; denoise
      ; ratios
      ; num_images = _
      }
    =
    t
  in
  let denoising_strength = Int63.to_float denoise /. 100.0 in
  let regional_prompter =
    if String.for_all ratios ~f:Char.is_whitespace
    then None
    else (
      match ratios |> String.lowercase |> String.strip |> String.lsplit2 ~on:' ' with
      | None -> None
      | Some (mode, ratios) ->
        let matrix_mode =
          match mode with
          | "row" | "rows" -> `Rows
          | "col" | "column" | "columns" -> `Columns
          | _ -> `Rows
        in
        let ratios = String.filter ratios ~f:(Fn.non Char.is_whitespace) in
        Some { Sd.Alwayson_scripts.Regional_prompter.Query.matrix_mode; ratios })
  in
  { Sd.Txt2img.Query.prompt = pos_prompt
  ; negative_prompt = neg_prompt
  ; width
  ; height
  ; steps
  ; cfg_scale = cfg
  ; seed
  ; denoising_strength
  ; sampler = Sd.Samplers.default
  ; subseed_strength = 0.0
  ; enable_hr = false
  ; ctrlnet = None
  ; hr_upscaler = Sd.Upscaler.default
  ; styles = Sd.Styles.none
  ; regional_prompter
  }
;;
