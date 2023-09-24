open! Core
open! Bonsai_web
open Async_kernel
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = string [@@deriving sexp, yojson]

module Api_response = struct
  type sampler = { name : string }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

  type t = sampler list [@@deriving of_yojson, sexp_of]
end

let dispatch host_and_port =
  let%bind.Deferred.Or_error response =
    Async_js.Http.get (sprintf "%s/sdapi/v1/samplers" host_and_port)
  in
  Deferred.Or_error.try_with (fun () ->
    response
    |> Yojson.Safe.from_string
    |> Api_response.t_of_yojson
    |> List.map ~f:(fun { name } -> name)
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch
