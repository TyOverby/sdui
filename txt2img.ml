open! Core
open! Async_kernel
open! Bonsai_web
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
    ; styles : Styles.t
    ; enable_hr : bool
    }
  [@@deriving sexp, typed_fields]

  module Underlying = struct
    type query = t

    type t =
      { prompt : string
      ; negative_prompt : string [@key "negative_prompt"]
      ; hr_prompt : string [@key "hr_prompt"]
      ; hr_negative_prompt : string [@key "hr_negative_prompt"]
      ; width : int
      ; height : int
      ; steps : int
      ; cfg_scale : int [@key "cfg_scale"]
      ; sampler : Samplers.t
      ; sampler_index : Samplers.t [@key "sampler_index"]
      ; sampler_name : Samplers.t [@key "sampler_name"]
      ; seed : int64
      ; styles : Styles.t
      ; enable_hr : bool
      }
    [@@deriving yojson_of, sexp, typed_fields]

    let yojson_of_t t =
      let record = yojson_of_t t in
      match record, Constants.txt2img_query with
      | `Assoc record, `Assoc template ->
        let init = String.Map.of_alist_exn template in
        record
        |> List.fold ~init ~f:(fun acc (key, data) -> Map.set acc ~key ~data)
        |> Map.to_alist
        |> fun alist -> `Assoc alist
      | record, template ->
        raise_s
          [%message
            "unexpected record or template %s %s"
              (record : Yojson_safe.t)
              (template : Yojson_safe.t)]
    ;;

    let of_query (query : query) : t =
      { prompt = query.prompt
      ; negative_prompt = query.negative_prompt
      ; hr_prompt = query.prompt
      ; hr_negative_prompt = query.negative_prompt
      ; width = Int63.to_int_exn query.width
      ; height = Int63.to_int_exn query.height
      ; steps = Int63.to_int_exn query.steps
      ; cfg_scale = Int63.to_int_exn query.cfg_scale
      ; sampler = query.sampler
      ; sampler_index = query.sampler
      ; sampler_name = query.sampler
      ; seed = Int63.to_int64 query.seed
      ; styles = query.styles
      ; enable_hr = query.enable_hr
      }
    ;;
  end
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
    ; info : Info.t
    }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

module Info = struct
  type t = { seed : Int63.t } [@@deriving sexp]
end

let dispatch (host_and_port, query) =
  let%bind.Deferred.Or_error response =
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
  in
  Deferred.Or_error.try_with (fun () ->
    Yojson.Safe.from_string response.content
    |> Response.t_of_yojson
    |> (fun { Response.images; info } ->
         let info = { Info.seed = Int63.of_int64_trunc info.seed } in
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
