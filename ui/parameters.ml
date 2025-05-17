open! Core
open! Bonsai_web
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
  ; sampler : Sd.Samplers.t
  ; specific_model : Sd.Hosts.Current_model.t option
  ; ctrlnet : Sd.Alwayson_scripts.Ctrlnet.Query.t option
  }
[@@deriving typed_fields, equal, sexp_of, fields ~getters]

let num_images { num_images; _ } = num_images

let component
  ?(samplers = Bonsai.return [ Sd.Samplers.default ])
  ?(models = Bonsai.return Sd.Hosts.Current_model.Set.empty)
  graph
  =
  let is_localhost = false in
  let default_size = if is_localhost then 128 else 256 in
  let sampler =
    Form.Elements.Dropdown.list_opt
      (module Sd.Samplers)
      samplers
      ~to_string:Sd.Samplers.to_string
      ~equal:Sd.Samplers.equal
      graph
  in
  let sampler =
    sampler
    >>| Form.project
          ~parse_exn:(Option.value ~default:Sd.Samplers.default)
          ~unparse:Option.some
  in
  let pos_prompt =
    P.prompt_form
      ~default:(Bonsai.return "! score_9, score_8_up, score_7_up,\n")
      ~container_attrs:[ {%css| flex-grow: 2; |} ]
      ~textarea_attrs:[ Vdom.Attr.create "data-kind" "prompt" ]
      ~label:"prompt"
      graph
  and neg_prompt =
    P.prompt_form
      ~default:(Bonsai.return "! score_1, score_2, score_3,\n")
      ~container_attrs:[ {%css| flex-grow: 1; |} ]
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
  and cfg = P.min_1_form ~default:(Int63.of_int 14) ~max:30 ~label:"cfg" graph
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
  and specific_model =
    Form.Elements.Dropdown.list_opt
      (module Sd.Hosts.Current_model)
      ~equal:Sd.Hosts.Current_model.equal
      (models >>| Set.to_list)
      graph
  in
  let seed =
    P.seed_form
      ~container_attrs:
        (let%arr num_images in
         fun ~state ~set_state ->
           [ Vdom.Attr.on_double_click (fun _ ->
               set_state
                 Int63.(state + of_int (Form.value_or_default num_images ~default:1)))
           ])
      graph
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
        | Specific_model -> specific_model
        | Sampler -> sampler
        | Ctrlnet ->
          Bonsai.return
            { Form.value = Ok None
            ; set = (fun _ -> Effect.return ())
            ; view = Vdom.Node.none
            }
      ;;

      let finalize_view { f } _graph =
        let%arr width = f Width
        and height = f Height
        and steps = f Steps
        and cfg = f Cfg
        and denoise = f Denoise
        and _seed = f Seed
        (*and ratios = f Ratios*)
        and sampler = f Sampler
        and model = f Specific_model
        and pos_prompt = f Pos_prompt
        and _num_images = f Num_images
        and neg_prompt = f Neg_prompt in
        fun ~direction ~theme ~reset:_ ->
          let vbox, hbox =
            match direction with
            | `Horizontal ->
              (fun ?gap a -> View.vbox ?gap a), fun ?gap a -> View.hbox ?gap a
            | `Vertical ->
              (fun ?gap a -> View.hbox ?gap a), fun ?gap a -> View.vbox ?gap a
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
          let flip_button =
            let on_click =
              let w = Form.value_or_default width ~default:Int63.zero in
              let h = Form.value_or_default height ~default:Int63.zero in
              Effect.Many [ Form.set width h; Form.set height w ]
            in
            View.button theme "flip" ~on_click
          in
          let preset ~w ~h =
            let on_click =
              Effect.Many
                [ Form.set width (Int63.of_int w); Form.set height (Int63.of_int h) ]
            in
            let label = sprintf "%dx%d" w h in
            View.button theme label ~on_click
          in
          let two_x_button = size_modification "2" ~f:Int63.(( * ) (of_int 2)) in
          let div2_button = size_modification "1/2" ~f:Int63.(fun x -> x / of_int 2) in
          let div3_button = size_modification "1/3" ~f:Int63.(fun x -> x / of_int 3) in
          let x640_1536 = preset ~w:640 ~h:1536 in
          let x768_1344 = preset ~w:768 ~h:1344 in
          let x832_1216 = preset ~w:832 ~h:1216 in
          let x896_1152 = preset ~w:896 ~h:1152 in
          Vdom.Node.div
            [ vbox
                [ hbox
                    [ (* View.button theme "reset" ~on_click:reset ; *)
                      vbox
                        [ Form.view model
                        ; Form.view sampler
                        ; Form.view width
                        ; Form.view height
                        ; hbox
                            ~gap:(`Em 1)
                            [ vbox [ flip_button; two_x_button; div2_button; div3_button ]
                            ; vbox [ x640_1536; x768_1344; x832_1216; x896_1152 ]
                            ]
                        ]
                    ; vbox [ Form.view steps; Form.view cfg; Form.view denoise ]
                      (* ; vbox [ Form.view seed ] *)
                      (*                     ; Form.view num_images *)
                      (*; Form.view ratios *)
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
      ; sampler
      ; cfg
      ; denoise
      ; ratios = _
      ; num_images = _
      ; specific_model = _
      ; ctrlnet
      }
    =
    t
  in
  let denoising_strength = Int63.to_float denoise /. 100.0 in
  { Sd.Img2img.Query.image = Sd.Image.empty
  ; mask = None
  ; prompt = pos_prompt
  ; negative_prompt = neg_prompt
  ; width
  ; height
  ; steps
  ; cfg_scale = cfg
  ; seed
  ; denoising_strength
  ; sampler
  ; subseed_strength = 0.0
  ; styles = Sd.Styles.none
  ; ctrlnet
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
      ; sampler
      ; num_images = _
      ; specific_model = _
      ; ctrlnet = _
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
  ; sampler
  ; subseed_strength = 0.0
  ; enable_hr = false
  ; ctrlnet = None
  ; hr_upscaler = Sd.Upscaler.default
  ; styles = Sd.Styles.none
  ; regional_prompter
  }
;;
