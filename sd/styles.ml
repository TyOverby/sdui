open! Core
open! Bonsai_web
open Async_kernel
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

type t = string list [@@deriving sexp, yojson, compare]

module Api_response = struct
  type sampler = { name : string }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

  type t = sampler list [@@deriving of_yojson, sexp_of]
end

let dispatch host_and_port =
  let%bind.Deferred.Or_error response =
    Async_js.Http.get (sprintf "%s/sdapi/v1/prompt-styles" host_and_port)
  in
  Deferred.Or_error.try_with (fun () ->
    response
    |> Yojson.Safe.from_string
    |> Api_response.t_of_yojson
    |> List.map ~f:(fun { name } -> name)
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch

let all ~(request_host : Hosts.request_host Value.t) =
  let%sub r, refresh =
    Bonsai.Edge.Poll.manual_refresh
      (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
      ~effect:
        (let%map request_host = request_host in
         let%bind.Effect work = request_host in
         work.f (fun host -> dispatch host))
  in
  let%sub () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:true
      (Time_ns.Span.of_min 1.0)
      refresh
  in
  return r
;;

let form ~request_host =
  let%sub all = all ~request_host in
  let%sub all =
    match%arr all with
    | Error _ -> []
    | Ok all -> all
  in
  let%sub form = Form.Elements.Typeahead.list (module String) ~all_options:all in
  return form
;;
