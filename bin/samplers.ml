open! Core
open! Bonsai_web
open Async_kernel
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

type t = string [@@deriving sexp, yojson]

let default = "Euler a"

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

let all ~host_and_port =
  let%sub r, refresh =
    Bonsai.Edge.Poll.manual_refresh
      (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
      ~effect:
        (let%map host_and_port = host_and_port in
         dispatch host_and_port)
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

let form ~host_and_port =
  let%sub all = all ~host_and_port in
  let%sub state, set_state = Bonsai.state default in
  let%sub theme = View.Theme.current in
  let%sub id = Bonsai.path_id in
  let%arr theme = theme
  and all = all
  and state = state
  and set_state = set_state
  and id = id in
  let all =
    match all with
    | Error _ -> [ default ]
    | Ok all -> all
  in
  let options =
    List.map all ~f:(fun sampler ->
      let view =
        sampler |> String.chop_suffix_if_exists ~suffix:"arras" |> Vdom.Node.text
      in
      sampler, String.equal sampler state, view)
  in
  let view =
    Kado.Unstable.Input.dropdown
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:
        (Vdom.Attr.many
           [ Custom_form_elements.Label_modifications.muted_label
           ; Vdom.Attr.style (Css_gen.create ~field:"height" ~value:"fit-content")
           ; Custom_form_elements.Label_modifications.Variables.set_all
               ~border:
                 (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
               ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
           ])
      ~title:(Some "sampler")
      ~on_change:set_state
      ~options
  in
  let form =
    Form.Expert.create ~value:(Ok state) ~set:set_state ~view:(Form.View.of_vdom ~id view)
  in
  form, view
;;
