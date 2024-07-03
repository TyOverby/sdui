open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module Base64_image = Base64_image
module Custom_form_elements = Custom_form_elements
module Hosts = Hosts
module Img2img = Img2img
module Txt2img = Txt2img
module Models = Models
module Parameters = Parameters
module Preview = Preview
module Progress = Progress
module Request_queue = Request_queue
module Samplers_request = Samplers_request
module Samplers = Samplers
module Styles = Styles
module Upscaler = Upscaler
module Gallery = Gallery

module Style = [%css stylesheet {|
  body {
    padding: 0;
    margin: 0;
  }
|}]

let component graph =
  let%sub { view = hosts_view; request = request_host; available_hosts } =
    Hosts.component graph
  in
  let parameters, _model_view =
    Parameters.component ~request_host ~available_hosts graph
  in
  let%sub { queue_request; view = gallery } =
    Gallery.component ~request_host ~set_params:(parameters >>| Form.set) graph
  in
  let%sub submit_effect =
    let%arr form = Bonsai.peek parameters graph
    and queue_request = queue_request in
    Some
      (match%bind.Effect form with
       | Inactive -> Effect.Ignore
       | Active form ->
         (match Form.value form with
          | Error e -> Effect.print_s [%sexp (e : Error.t)]
          | Ok query -> queue_request query))
  in
  let%arr parameters = parameters
  and submit_effect = submit_effect
  and gallery = gallery
  and hosts_view = hosts_view in
  let on_submit = Option.value submit_effect ~default:Effect.Ignore in
  Vdom.Node.div [ (Form.view parameters) ~on_submit ~hosts_panel:hosts_view; gallery ]
;;
