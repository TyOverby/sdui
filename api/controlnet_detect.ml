open! Core
open! Async_kernel
open! Bonsai_web
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Shared
module Info = Txt2img.Info

module Query = struct
  type t =
    { module_ : string
    ; image : Image.t
    }
  [@@deriving sexp, typed_fields, equal]

  module Underlying = struct
    type query = t

    type t =
      { controlnet_module : string [@key "controlnet_module"]
      ; controlnet_input_images : string list [@key "controlnet_input_images"]
      }
    [@@deriving yojson_of, sexp, typed_fields]

    let yojson_of_t t =
      let record = yojson_of_t t in
      Yojson_safe.merge_objects ~template:Constants.txt2img_query record
    ;;

    let of_query (query : query) : t =
      { controlnet_input_images = [ Image.to_string query.image ]
      ; controlnet_module = query.module_
      }
    ;;
  end
end

module Response = struct
  type t =
    { images : string list
    ; info : string
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
        ~url:(sprintf "%s/controlnet/detect" host_and_port)
      |> Deferred.Or_error.ok_exn
    in
    print_s [%message response.content];
    let response_content, images = Strip_images_from_json.f response.content in
    Yojson.Safe.from_string response_content
    |> Response.t_of_yojson
    |> (fun response -> { response with Response.images })
    |> (fun { Response.images = base64_images; info = _ } ->
         let images, kind = base64_images, Image.Base64 in
         List.map images ~f:(fun s ->
           let width, height =
             match Image.size query.image with
             | None -> None, None
             | Some (w, h) -> Some w, Some h
           in
           Image.of_string ?width ?height ~kind s)
         |> List.hd_exn)
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch
let dispatch ~host_and_port query = dispatch (host_and_port, query)
