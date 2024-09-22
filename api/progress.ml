open! Core
open! Bonsai_web
open! Async_kernel
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Bonsai.Let_syntax
open Shared

type t =
  { progress : float
  ; eta_relative : float [@key "eta_relative"]
  ; state : Yojson_safe.t
  ; current_image : Image.t option [@key "current_image"]
  }
[@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

let dispatch host_and_port =
  Deferred.Or_error.try_with (fun () ->
    let%bind.Deferred response =
      sprintf "%s/sdapi/v1/progress?skip_current_image=false" host_and_port
      |> Async_js.Http.get
      |> Deferred.Or_error.ok_exn
    in
    Yojson.Safe.from_string response |> t_of_yojson |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch

let state ~host_and_port graph =
  let r, refresh =
    Bonsai.Edge.Poll.manual_refresh
      (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
      graph
      ~effect:
        (let%map host_and_port = host_and_port in
         dispatch host_and_port)
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:true
    (Time_ns.Span.of_sec 0.1)
    refresh
    graph;
  r
;;
