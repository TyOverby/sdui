open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let default = "Euler a"
let to_string = Fn.id

include Samplers_request

let random_healthy_host hosts =
  Effect.of_thunk (fun () -> List.random_element (Map.keys hosts))
;;

let all (type a) ~(hosts : a Hosts.Host.Map.t Bonsai.t) graph =
  let r, refresh =
    Bonsai.Edge.Poll.manual_refresh
      (Bonsai.Edge.Poll.Starting.initial (Error (Error.of_string "loading...")))
      ~effect:
        (let%map hosts in
         match%bind.Effect random_healthy_host hosts with
         | None -> Effect.return (Ok [])
         | Some host -> dispatch (host :> string))
      graph
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:true
    (Bonsai.return (Time_ns.Span.of_sec 1.0))
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
  { Form.value = Ok state; set = set_state; view }
;;
