open! Core
open! Bonsai_web.Cont

module Base64_image = Base64_image
module Custom_form_elements  = Custom_form_elements
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

val component : Bonsai.graph -> Vdom.Node.t Bonsai.t
