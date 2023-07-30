open! Core
open! Bonsai_web
open! Async_kernel
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form

type t =
  { form : Txt2img.Query.t Form.t
  ; width : int
  ; height : int
  }

let component =
  let%sub width = Form.Elements.Number.int ~default:512 ~step:8 () in
  let%sub height = Form.Elements.Number.int ~default:512 ~step:8 () in
  let%sub form =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Txt2img.Query.Typed_field

        let label_for_field = `Inferred

        let form_for_field : type a. a Typed_field.t -> a Form.t Computation.t = function
          | Prompt -> Form.Elements.Textarea.string ()
          | Negative_prompt -> Form.Elements.Textarea.string ()
          | Width -> return width
          | Height -> return height
        ;;
      end)
  in
  let%arr width = width
  and height = height
  and form = form in
  let width = Form.value_or_default width ~default:128 in
  let height = Form.value_or_default height ~default:128 in
  { form; width; height }
;;
