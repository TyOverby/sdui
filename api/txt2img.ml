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
    ; denoising_strength : float
    ; styles : Styles.t
    ; enable_hr : bool
    ; ctrlnet : Alwayson_scripts.Ctrlnet.Query.t option
    ; regional_prompter : Alwayson_scripts.Regional_prompter.Query.t option
    ; hr_upscaler : Upscaler.t
    }
  [@@deriving sexp, typed_fields, equal]

  module Underlying = struct
    type query = t

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
      ; denoising_strength : float [@key "denoising_strength"]
      ; styles : Styles.t
      ; enable_hr : bool
      ; always_on_scripts : Alwayson_scripts.t option [@key "alwayson_scripts"] [@option]
      }
    [@@deriving yojson_of, sexp, typed_fields]

    let yojson_of_t t =
      let record = yojson_of_t t in
      Yojson_safe.merge_objects ~template:Constants.txt2img_query record
    ;;

    let of_query (query : query) : t =
      let always_on_scripts =
        Alwayson_scripts.create
          ~ctrlnet:query.ctrlnet
          ~regional_prompter:query.regional_prompter
          ()
      in
      { prompt = Pre_process_prompt.strip_prompt query.prompt
      ; negative_prompt = Pre_process_prompt.strip_prompt query.negative_prompt
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
      ; denoising_strength = query.denoising_strength
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
    ; image_paths : string list [@default []]
    ; info : Info.t
    ; parameters : Parameters.t
    }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

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
    let response_content, images = Strip_images_from_json.f response.content in
    Yojson.Safe.from_string response_content
    |> Response.t_of_yojson
    |> (fun response -> { response with Response.images })
    |> (fun { Response.images = base64_images; image_paths = _; parameters; info } ->
         let info =
           { Info.seed = Int63.of_int64_trunc info.seed
           ; enable_hr = parameters.enable_hr
           }
         in
         let image_paths =
           []
           (* [image_paths] is only useful when all the work is being done on one machine  List.map image_paths ~f:(fun path -> sprintf "%s/file=%s" host_and_port path) *)
         in
         let images, kind =
           if List.length image_paths >= List.length base64_images
           then image_paths, Image.Url
           else base64_images, Image.Base64
         in
         List.map images ~f:(fun s ->
           let width, height =
             if query.enable_hr
             then Int63.(query.width * of_int 2, query.height * of_int 2)
             else query.width, query.height
           in
           Image.of_string ~width ~height ~kind s, info)
         |> List.hd_exn)
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch
let dispatch ~host_and_port query = dispatch (host_and_port, query)
