open! Core
open! Bonsai_web
open! Async_kernel
open Shared
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Websocket = Websocket

let template =
  Yojson_safe.from_string
    {|{
max_new_tokens: 256,
  do_sample: true,
  temperature: 0.7,
  top_p: 0.5,
  typical_p: 0.19,
  repetition_penalty: 1.1,
  repetition_penalty_range: 0,
  encoder_repetition_penalty: 1,
  top_k: 0,
  min_length: 0,
  no_repeat_ngram_size: 0,
  num_beams: 1,
  penalty_alpha: 0,
  length_penalty: 1,
  early_stopping: false,
  seed: -1,
  add_bos_token: false,
  stopping_strings: [ "\nYou:", "\n### Instruction:", "\n### Response:" ],
  truncation_length: 3500,
  ban_eos_token: true,
  skip_special_tokens: true,
  top_a: 0,
  tfs: 1,
  epsilon_cutoff: 0,
  eta_cutoff: 0,
  mirostat_mode: 0,
  mirostat_tau: 5,
  mirostat_eta: 0.1
  }|}
;;

module Config = struct
  type t =
    { prompt : string
    ; length_penalty : float option [@key "length_penalty"] [@yojson.option]
    ; early_stopping : bool option [@key "early_stopping"] [@yojson.option]
    ; max_new_tokens : int option [@key "max_new_tokens"] [@yojson.option]
    }
  [@@deriving yojson_of, sexp_of]
end

type kind =
  [ `Instruction
  | `Response
  | `Continue
  ]

let make
  ?length_penalty
  ?early_stopping
  ?max_new_tokens
  ~context
  ~(history : (kind * string) list)
  (kind : kind)
  =
  let history = (history :> ([ kind | `This_continue ] * string) list) in
  let prompt =
    [ Some context ]
    @ List.map
        (history
         @ [ (match kind with
              | `Continue -> `This_continue, ""
              | other -> (other :> [ kind | `This_continue ]), "")
           ])
        ~f:(fun (kind, text) ->
          match kind with
          | `Instruction -> Some ("### Instruction:\n" ^ text)
          | `Response -> Some ("### Response:\n" ^ text)
          | `This_continue -> None
          | `Continue -> Some text)
    |> List.filter_opt
    |> String.concat ~sep:"\n\n"
  in
  Yojson_safe.merge_objects
    ~template
    (Config.yojson_of_t { Config.prompt; length_penalty; early_stopping; max_new_tokens })
  |> Yojson_safe.pretty_to_string
;;

module Event = struct
  type t =
    { event : string
    ; text : string option [@yojson.option]
    }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

let prompt' (on_response, t) =
  let open Async_kernel in
  Js_of_ocaml.Firebug.console##log
    (Js_of_ocaml.Json.unsafe_input (Js_of_ocaml.Js.string t));
  let%bind.Deferred.Or_error { Websocket.reader; writer; close } =
    Websocket.connect (Uri.of_string "ws://localhost:5005/api/v1/stream")
  in
  let%bind () = Pipe.write writer t in
  let result = Ivar.create () in
  let%bind () =
    Pipe.iter_without_pushback reader ~f:(fun event ->
      let event = event |> Yojson_safe.from_string |> Event.t_of_yojson in
      match event.event, event.text with
      | "text_stream", Some text -> on_response text
      | "stream_end", _ ->
        close ();
        Ivar.fill_exn result (Ok ())
      | event, text ->
        Ivar.fill_exn
          result
          (Or_error.error_s
             [%message "unrecognized event kind" (event : string) (text : string option)]))
  in
  Ivar.read result
;;

let prompt' (on_response, t) =
  let on_response s = Effect.Expert.handle_non_dom_event_exn (on_response s) in
  Effect.of_deferred_fun prompt' (on_response, t)
;;

let send ~on_response t = prompt' (on_response, t)

module Token_counts = struct
  type result = { tokens : int }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

  type t = { results : result list }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

let count_tokens t =
  let%bind.Deferred.Or_error response =
    let body = Async_js.Http.Post_body.String t in
    Async_js.Http.request
      ~response_type:Default
      ~headers:[ "Content-Type", "application/json" ]
      (Post (Some body))
      ~url:"http://localhost:5000/api/v1/token-count"
  in
  Deferred.Or_error.try_with (fun () ->
    response
    |> (fun { Async_js.Http.Response.content; _ } -> content)
    |> Yojson.Safe.from_string
    |> Token_counts.t_of_yojson
    |> (function
         | { Token_counts.results = [ { tokens } ] } -> tokens
         | _ -> assert false)
    |> Deferred.return)
;;

let count_tokens = Effect.of_deferred_fun count_tokens

type t = string
