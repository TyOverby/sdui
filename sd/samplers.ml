open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

let default = "Euler a"

include Samplers_request

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

let str_rep ~find ~replace s =
  String.Search_pattern.replace_all
    (String.Search_pattern.create find)
    ~in_:s
    ~with_:replace
;;

let form ~request_host graph =
  let all = all ~request_host graph in
  let state, set_state = Bonsai.state default graph in
  let%arr theme = View.Theme.current graph
  and all = all
  and state = state
  and set_state = set_state
  and unique_key = Bonsai.path_id graph in
  let all =
    match all with
    | Error _ -> [ default ]
    | Ok all -> all
  in
  let options =
    List.map all ~f:(fun sampler ->
      let view =
        sampler
        |> str_rep ~find:"Karras" ~replace:"K"
        |> str_rep ~find:"Exponential" ~replace:"E"
        |> str_rep ~find:"Heun" ~replace:"H"
        |> Vdom.Node.text
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
    Form.Expert.create
      ~value:(Ok state)
      ~set:set_state
      ~view:(Form.View.of_vdom ~unique_key view)
  in
  form, view
;;
