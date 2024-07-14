open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Ctrlnet = struct
  type arg =
    { enabled : bool
    ; module_ : string [@key "module"]
    ; model : string
    ; weight : float
    ; image : string
    ; resize_mode : int [@key "resize_mode"]
    ; lowvram : bool
    ; processor_res : int [@key "processor_res"]
    ; threshold_a : int [@key "threshold_a"]
    ; threshold_b : int [@key "threshold_b"]
    ; guidance_start : float [@key "guidance_start"]
    ; guidance_end : float [@key "guidance_end"]
    ; control_mode : int [@key "control_mode"]
    ; pixel_perfect : bool [@key "pixel_perfect"]
    }
  [@@deriving yojson_of, sexp]

  type t = { args : arg list } [@@deriving yojson_of, sexp]

  module Query = struct
    type t = { image : string } [@@deriving sexp, equal]

    let to_arg { image } =
      { args =
          [ { pixel_perfect = false
            ; control_mode = 0
            ; guidance_start = 0.0
            ; guidance_end = 0.8
            ; threshold_a = 64
            ; threshold_b = 64
            ; processor_res = 64
            ; lowvram = false
            ; resize_mode = 1
            ; image
            ; weight = 1.0
            ; model = "control_sd15_depth [fef5e48e]"
            ; module_ = "depth_zoe"
            ; enabled = true
            }
          ]
      }
    ;;
  end
end

module Regional_prompter = struct
  type arg =
    { active : bool
    ; debug : bool
    ; mode : string
    ; matrix_mode : string
    ; mask_mode : string
    ; prompt_mode : string
    ; ratios : string
    ; base_ratios : string
    ; use_base : bool
    ; use_common : bool
    ; use_neg_common : bool
    ; calcmode : string
    ; not_change_and : bool
    ; lora_textencoder : string
    ; lora_unet : string
    ; threshold : string
    ; mask : string
    ; lora_stop_step : string
    ; lora_hires_sto_step : string
    ; flip : bool
    }
  [@@deriving yojson_of, sexp]

  let yojson_of_arg t =
    match yojson_of_arg t with
    | `Assoc list -> `List (List.unzip list |> Tuple2.get2)
    | other -> other
  ;;

  type t = { args : arg } [@@deriving yojson_of, sexp]

  module Query = struct
    type t =
      { ratios : string
      ; matrix_mode : [ `Horizontal | `Vertical | `Columns | `Rows ]
      }
    [@@deriving sexp, equal]

    let to_arg { ratios; matrix_mode } =
      let mode =
        match `Matrix with
        | `Matrix -> "Matrix"
        | `Mask -> "Mask"
        | `Prompt -> "Prompt"
      in
      let matrix_mode =
        match matrix_mode with
        | `Horizontal -> "Horizontal"
        | `Vertical -> "Vertical"
        | `Columns -> "Columns"
        | `Rows -> "Rows"
      in
      let calcmode =
        match `Attention with
        | `Attention -> "Attention"
        | `Latent -> "Latent"
      in
      { args =
          { active = true
          ; debug = true
          ; mode
          ; matrix_mode
          ; mask_mode = "Mask"
          ; prompt_mode = "Prompt"
          ; ratios
          ; base_ratios = "0.5"
          ; use_base = false
          ; use_common = true
          ; use_neg_common = false
          ; calcmode
          ; not_change_and = false
          ; lora_textencoder = "0"
          ; lora_unet = "0"
          ; threshold = "0"
          ; mask = ""
          ; lora_stop_step = "0"
          ; lora_hires_sto_step = "0"
          ; flip = false
          }
      }
    ;;
  end
end

type t =
  { controlnet : Ctrlnet.t option [@option]
  ; regional_prompter : Regional_prompter.t option [@option] [@key "Regional Prompter"]
  }
[@@deriving yojson_of, sexp]

let create ~ctrlnet ~regional_prompter () =
  let controlnet = Option.map ctrlnet ~f:Ctrlnet.Query.to_arg in
  let regional_prompter =
    Option.map regional_prompter ~f:Regional_prompter.Query.to_arg
  in
  match controlnet, regional_prompter with
  | None, None -> None
  | _, _ -> Some { controlnet; regional_prompter }
;;
