open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Shared
module Info = Txt2img.Info

module Query = struct
  type t =
    { init_images : Image.t list
    ; prompt : string
    ; negative_prompt : string
    ; width : Int63.t
    ; height : Int63.t
    ; steps : Int63.t
    ; cfg_scale : Int63.t
    ; sampler : Samplers.t
    ; seed : Int63.t
    ; subseed_strength : float
    ; denoising_strength : float
    ; styles : Styles.t
    ; mask : Image.t option
    }
  [@@deriving sexp, typed_fields, equal]

  let of_txt2img (other : Txt2img.Query.t) ~init_images ~mask =
    { init_images
    ; prompt = other.prompt
    ; negative_prompt = other.negative_prompt
    ; width = other.width
    ; height = other.height
    ; steps = other.steps
    ; cfg_scale = other.cfg_scale
    ; sampler = other.sampler
    ; seed = other.seed
    ; subseed_strength = other.subseed_strength
    ; denoising_strength = other.denoising_strength
    ; styles = other.styles
    ; mask
    }
  ;;

  module Underlying = struct
    type query = t

    type t =
      { init_images : string list [@key "init_images"]
      ; include_init_images : bool [@key "include_init_images"]
      ; prompt : string
      ; negative_prompt : string [@key "negative_prompt"]
      ; width : int
      ; height : int
      ; steps : int
      ; cfg_scale : int [@key "cfg_scale"]
      ; image_cfg_scale : float [@key "image_cfg_scale"]
      ; sampler : Samplers.t
      ; sampler_index : Samplers.t [@key "sampler_index"]
      ; sampler_name : Samplers.t [@key "sampler_name"]
      ; seed : int64
      ; subseed_strength : float [@key "subseed_strength"]
      ; denoising_strength : float [@key "denoising_strength"]
      ; styles : Styles.t
      ; mask : Image.t option [@key "mask"] [@option]
      ; inpainting_mask_invert : int [@key "inpainting_mask_invert"]
      ; inpainting_fill : int [@key "inpainting_fill"]
      ; resize_mode : int [@key "resize_mode"]
      ; inpaint_full_res_padding : int [@key "inpaint_full_res_padding"]
      ; inpaint_full_res : bool [@key "inpaint_full_res"]
      ; initial_noise_multiplier : float [@key "initial_noise_multiplier"]
      }
    [@@deriving yojson_of, sexp, typed_fields]

    let yojson_of_t t =
      let record = yojson_of_t t in
      Yojson_safe.merge_objects ~template:Constants.txt2img_query record
    ;;

    let of_query (query : query) : t =
      { init_images = List.map query.init_images ~f:Image.data_url
      ; prompt = query.prompt
      ; negative_prompt = query.negative_prompt
      ; width = Int63.to_int_exn query.width
      ; height = Int63.to_int_exn query.height
      ; steps = Int63.to_int_exn query.steps
      ; cfg_scale = Int63.to_int_exn query.cfg_scale
      ; sampler = query.sampler
      ; sampler_index = query.sampler
      ; sampler_name = query.sampler
      ; seed = Int63.to_int64 query.seed
      ; subseed_strength = query.subseed_strength
      ; styles = query.styles
      ; denoising_strength = query.denoising_strength
      ; mask = query.mask
      ; image_cfg_scale = 0.7
      ; inpainting_fill = 1
      ; resize_mode = 0
      ; inpaint_full_res_padding = 32
      ; inpaint_full_res = true
      ; inpainting_mask_invert = 1
      ; include_init_images = true
      ; initial_noise_multiplier = 1.0
      }
    ;;
  end

  let apply_info t info = { t with seed = info.Info.seed }
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

  type t =
    { images : string list
    ; image_paths : string list [@default []]
    ; info : Info.t
    }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

let strip_images_field_from_json json =
  let obj = Js_of_ocaml.Json.unsafe_input (Js_of_ocaml.Js.string json) in
  Js_of_ocaml.Firebug.console##log_2 (Js_of_ocaml.Js.string "before") obj;
  let stripped = Js_of_ocaml.Js.Unsafe.get obj (Js_of_ocaml.Js.string "images") in
  Js_of_ocaml.Js.Unsafe.set
    obj
    (Js_of_ocaml.Js.string "images")
    (new%js Js_of_ocaml.Js.array_empty);
  Js_of_ocaml.Js.Unsafe.set
    obj
    (Js_of_ocaml.Js.string "init_images")
    (new%js Js_of_ocaml.Js.array_empty);
  Js_of_ocaml.Js.Unsafe.set
    obj##.parameters
    (Js_of_ocaml.Js.string "init_images")
    (new%js Js_of_ocaml.Js.array_empty);
  Js_of_ocaml.Firebug.console##log_2 (Js_of_ocaml.Js.string "after") obj;
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
        ~url:(sprintf "%s/sdapi/v1/img2img" host_and_port)
      |> Deferred.Or_error.ok_exn
    in
    let response_content, images = strip_images_field_from_json response.content in
    Yojson.Safe.from_string response_content
    |> Response.t_of_yojson
    |> (fun response -> { response with Response.images })
    |> (fun { Response.images = base64_images; image_paths; info } ->
         let info = { Info.seed = Int63.of_int64_trunc info.seed; enable_hr = false } in
         let image_paths =
           List.map image_paths ~f:(fun path -> sprintf "%s/file=%s" host_and_port path)
         in
         let images, kind =
           if List.length image_paths >= List.length base64_images
           then image_paths, Image.Url
           else base64_images, Image.Base64
         in
         List.map images ~f:(fun s ->
           let width, height = query.width, query.height in
           Image.of_string ~width ~height ~kind s, info))
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch
let dispatch ~host_and_port query = dispatch (host_and_port, query)
