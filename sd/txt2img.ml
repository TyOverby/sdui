open! Core
open! Async_kernel
open! Bonsai_web
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Shared

module Info = struct
  type t =
    { seed : Int63.t
    ; enable_hr : bool
    }
  [@@deriving sexp]
end

module Query = struct
  type t =
    { prompt : string
    ; negative_prompt : string
    ; width : Int63.t
    ; height : Int63.t
    ; steps : Int63.t
    ; cfg_scale : Int63.t
    ; sampler : Samplers.t
    ; seed : Int63.t
    ; subseed_strength : float
    ; styles : Styles.t
    ; enable_hr : bool
    ; data_url : string
    ; hr_upscaler : Upscaler.t
    }
  [@@deriving sexp, typed_fields]

  module Underlying = struct
    type query = t

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

      type mid = { args : arg list } [@@deriving yojson_of, sexp]
      type high = { controlnet : mid } [@@deriving yojson_of, sexp]

      let create image =
        { controlnet =
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
        }
      ;;
    end

    type t =
      { prompt : string
      ; negative_prompt : string [@key "negative_prompt"]
      ; width : int
      ; height : int
      ; steps : int
      ; cfg_scale : int [@key "cfg_scale"]
      ; sampler : Samplers.t
      ; sampler_index : Samplers.t [@key "sampler_index"]
      ; sampler_name : Samplers.t [@key "sampler_name"]
      ; hr_upscaler : Upscaler.t [@key "hr_upscaler"]
      ; seed : int64
      ; subseed_strength : float [@key "subseed_strength"]
      ; styles : Styles.t
      ; enable_hr : bool
      ; always_on_scripts : Ctrlnet.high option [@key "alwayson_scripts"] [@option]
      }
    [@@deriving yojson_of, sexp, typed_fields]

    let yojson_of_t t =
      let record = yojson_of_t t in
      Yojson_safe.merge_objects ~template:Constants.txt2img_query record
    ;;

    let of_query (query : query) : t =
      let always_on_scripts =
        match String.strip query.data_url with
        | "" -> None
        | data_url -> Some (Ctrlnet.create data_url)
      in
      { prompt = query.prompt
      ; negative_prompt = query.negative_prompt
      ; width = Int63.to_int_exn query.width
      ; height = Int63.to_int_exn query.height
      ; steps = Int63.to_int_exn query.steps
      ; cfg_scale = Int63.to_int_exn query.cfg_scale
      ; sampler = query.sampler
      ; sampler_index = query.sampler
      ; sampler_name = query.sampler
      ; hr_upscaler = query.hr_upscaler
      ; seed = Int63.to_int64 query.seed
      ; subseed_strength = query.subseed_strength
      ; styles = query.styles
      ; enable_hr = query.enable_hr
      ; always_on_scripts
      }
    ;;
  end

  let apply_info t info = { t with seed = info.Info.seed; enable_hr = info.enable_hr }
end

module Response = struct
  module Info = struct
    type t = { seed : int64 }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

    let t_of_yojson (t : Yojson_safe.t) =
      match t with
      | `String s -> s |> Yojson_safe.from_string |> t_of_yojson
      | _ -> failwith "info should be encoded as a string"
    ;;
  end

  module Parameters = struct
    type t = { enable_hr : bool [@key "enable_hr"] }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
  end

  type t =
    { images : string list
    ; info : Info.t
    ; parameters : Parameters.t
    }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

let strip_images_field_from_json json =
  let obj = Js_of_ocaml.Json.unsafe_input (Js_of_ocaml.Js.string json) in
  let stripped = Js_of_ocaml.Js.Unsafe.get obj (Js_of_ocaml.Js.string "images") in
  Js_of_ocaml.Js.Unsafe.set
    obj
    (Js_of_ocaml.Js.string "images")
    (new%js Js_of_ocaml.Js.array_empty);
  ( Js_of_ocaml.Json.output obj |> Js_of_ocaml.Js.to_string
  , Js_of_ocaml.Js.to_array stripped
    |> Array.to_list
    |> List.map ~f:Js_of_ocaml.Js.to_string )
;;

let dispatch (host_and_port, query) =
  Deferred.Or_error.try_with (fun () ->
    let%bind.Deferred response =
      let body =
        Async_js.Http.Post_body.String
          (query
           |> Query.Underlying.of_query
           |> Query.Underlying.yojson_of_t
           |> Yojson_safe.to_string)
      in
      Async_js.Http.request
        ~response_type:Default
        ~headers:[ "Content-Type", "application/json" ]
        (Post (Some body))
        ~url:(sprintf "%s/sdapi/v1/txt2img" host_and_port)
      |> Deferred.Or_error.ok_exn
    in
    let response_content, images = strip_images_field_from_json response.content in
    Yojson.Safe.from_string response_content
    |> Response.t_of_yojson
    |> (fun response -> { response with Response.images })
    |> (fun { Response.images; parameters; info } ->
         let info =
           { Info.seed = Int63.of_int64_trunc info.seed
           ; enable_hr = parameters.enable_hr
           }
         in
         List.map images ~f:(fun s ->
           let width, height =
             if query.enable_hr
             then Int63.(query.width * of_int 2, query.height * of_int 2)
             else query.width, query.height
           in
           Base64_image.of_string ~width ~height s, info))
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch
let dispatch ~host_and_port query = dispatch (host_and_port, query)
