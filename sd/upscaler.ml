open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

let default = "Latent"

include struct
  open! Core
  open! Bonsai_web
  open Async_kernel
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type t = string [@@deriving sexp, yojson]

  module Api_response = struct
    type upscaler = { name : string }
    [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]

    type t = upscaler list [@@deriving of_yojson, sexp_of]
  end

  let dispatch host_and_port =
    Deferred.Or_error.try_with (fun () ->
      let%bind.Deferred response =
        Async_js.Http.get (sprintf "%s/sdapi/v1/upscalers" host_and_port)
        |> Deferred.Or_error.ok_exn
      in
      response
      |> Yojson.Safe.from_string
      |> Api_response.t_of_yojson
      |> List.map ~f:(fun { name } -> name)
      |> Deferred.return)
  ;;

  let dispatch = Effect.of_deferred_fun dispatch
end

let all ~(request_host : Hosts.request_host Bonsai.t) graph =
  let r, refresh =
    Bonsai.Edge.Poll.manual_refresh
      (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
      ~effect:
        (let%map request_host = request_host in
         let%bind.Effect work = request_host in
         work.f (fun host -> dispatch (host :> string)))
      graph
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:true
    (Time_ns.Span.of_min 1.0)
    refresh
    graph;
  r
;;

let form ~request_host graph =
  let state, set_state = Bonsai.state default graph in
  let%arr theme = View.Theme.current graph
  and all = all ~request_host graph
  and state = state
  and set_state = set_state
  and unique_key = Bonsai.path_id graph in
  let all =
    match all with
    | Error _ -> [ default ]
    | Ok all -> "Latent" :: all
  in
  let options =
    List.map all ~f:(fun upscaler ->
      let view = Vdom.Node.text upscaler in
      upscaler, String.equal upscaler state, view)
  in
  let view =
    Kado.Unstable.Input.dropdown
      ~constants:(View.constants theme)
      ~input_attr:Vdom.Attr.empty
      ~container_attr:
        (Vdom.Attr.many
           [ Custom_form_elements.Label_modifications.muted_label
           ; Vdom.Attr.style (Css_gen.create ~field:"height" ~value:"fit-content")
           ; Vdom.Attr.style (Css_gen.create ~field:"margin-top" ~value:"0")
           ; Custom_form_elements.Label_modifications.Variables.set_all
               ~border:
                 (Css_gen.Color.to_string_css (View.extreme_primary_border_color theme))
               ~fg:(Css_gen.Color.to_string_css (View.primary_colors theme).foreground)
           ])
      ~title:None
      ~on_change:set_state
      ~options
  in
  let form =
    Form.Expert.create
      ~value:(Ok state)
      ~set:set_state
      ~view:(Form.View.of_vdom ~unique_key view)
  in
  form, view
;;
