open! Core
open! Bonsai_web
open Async_kernel
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Form = Bonsai_web_ui_form.With_manual_view

type t = string option [@@deriving sexp, yojson, compare]

module T = struct
  type t = { sd_model_checkpoint : string }
  [@@yojson.allow_extra_fields] [@@deriving yojson, sexp_of]
end

let dispatch_get host_and_port =
  Deferred.Or_error.try_with_join (fun () ->
    let%bind.Deferred.Or_error response =
      Async_js.Http.get (sprintf "%s/sdapi/v1/options" host_and_port)
    in
    Deferred.Or_error.try_with (fun () ->
      response
      |> Yojson.Safe.from_string
      |> T.t_of_yojson
      |> fun { T.sd_model_checkpoint } ->
      String.split_on_chars ~on:[ ' '; '.' ] sd_model_checkpoint
      |> List.hd_exn
      |> Deferred.return))
;;

let dispatch_get = Effect.of_deferred_fun dispatch_get
