open! Core
open! Bonsai_web
open Async_kernel
open Bonsai.Let_syntax
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Form = Bonsai_web_ui_form.With_manual_view

let default = "depth_anything_v2"
let to_string = Fn.id

type t = string [@@deriving sexp, yojson, equal]

module Api_response = struct
  type t = { module_list : string list }
  [@@yojson.allow_extra_fields] [@@deriving of_yojson, sexp_of]
end

let dispatch host_and_port =
  Deferred.Or_error.try_with (fun () ->
    let%bind.Deferred response =
      Async_js.Http.get (sprintf "%s/controlnet/module_list" host_and_port)
      |> Deferred.Or_error.ok_exn
    in
    response
    |> Yojson.Safe.from_string
    |> Api_response.t_of_yojson
    |> (fun { Api_response.module_list } -> module_list)
    |> Deferred.return)
;;

let dispatch = Effect.of_deferred_fun dispatch

let random_healthy_host hosts =
  Effect.of_thunk (fun () -> List.random_element (Map.keys hosts))
;;

let all (type a) ~(hosts : a Hosts.Host.Map.t Bonsai.t) graph =
  let r, refresh =
    Bonsai.Edge.Poll.manual_refresh
      (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
      ~effect:
        (let%map.Bonsai hosts in
         match%bind.Effect random_healthy_host hosts with
         | None -> Effect.return (Ok [])
         | Some host -> dispatch (host :> string))
      graph
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:true
    (Time_ns.Span.of_sec 1.0)
    refresh
    graph;
  r
;;

let form ~hosts graph =
  let all = all ~hosts graph in
  let state, set_state = Bonsai.state default graph in
  let%arr theme = View.Theme.current graph
  and all
  and state
  and set_state in
  let all =
    match all with
    | Error _ -> [ default ]
    | Ok all -> all
  in
  let options =
    List.map all ~f:(fun module_ ->
      let view = Vdom.Node.text module_ in
      module_, String.equal module_ state, view)
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
      ~title:(Some "module")
      ~on_change:set_state
      ~options
  in
  let value =
    match state with
    | "none" -> Ok None
    | other -> Ok (Some other)
  in
  let set_state = function
    | Some s -> set_state s
    | None -> set_state "none"
  in
  { Form.value; set = set_state; view }
;;
