open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml

module _ =
  [%css
  stylesheet
    ~dont_hash:[ "stack" ]
    {|
        .stack {
            display: inline-grid;
            grid-template-rows: auto;
            grid-template-columns: auto;
        }

        .stack>canvas {
            grid-area: 1 / 1;
            cursor:none;
            width:512px;
            height:512px;
        }

        * {
            touch-action: none;
        }
|}]

module Output = struct
  class type t = object
    method clear : unit Js.meth
    method get_data_url : Js.js_string Js.t Js.meth
    method updateImage : Js.js_string Js.t -> unit Js.meth
    method composite : Js.js_string Js.t Js.meth
  end
end

module Widget :
  Bonsai_web_ui_widget.S with type input = string and type state = Output.t Js.t = struct
  type element = Dom_html.divElement
  type input = string
  type state = Output.t Js.t

  external painter_init : Js.js_string Js.t -> state * element Js.t = "painter_init"

  let init ~get_input:_ input = painter_init (Js.string input)

  let update ~prev_input new_input state element =
    if phys_equal prev_input new_input
    then element
    else (
      state##updateImage (Js.string new_input);
      element)
  ;;

  let destroy _input _state _element = ()
end

module Id = Bonsai_extra.Id_gen (Int63) ()

let component (image : Sd.Base64_image.t Bonsai.t) graph =
  let data_url = image >>| Sd.Base64_image.data_url in
  let widget = Bonsai_web_ui_widget.component (module Widget) data_url graph in
  let unique_id, next_id =
    Bonsai.state_machine0 ~default_model:0 ~apply_action:(fun _ i () -> i + 1) graph
  in
  let value, inject =
    Bonsai.state_machine0
      ~default_model:Inc.Or_error_or_stale.Not_computed
      ~apply_action:(fun _ model ->
        function
        | `Set_value string ->
          Inc.Or_error_or_stale.Fresh (Sd.Base64_image.of_string string)
        | `Invalidate ->
          (match model with
           | Fresh s -> Stale s
           | other -> other))
      graph
  in
  let view =
    let%arr theme = View.Theme.current graph
    and unique_id = unique_id
    and next_id = next_id
    and inject = inject
    and { Bonsai_web_ui_widget.view = widget; read; _ } = widget in
    let forward =
      let%bind.Effect effects =
        read (fun _input state -> inject (`Set_value (Js.to_string state##composite)))
      in
      Effect.all_unit effects
    in
    Vdom.Node.div
      ~key:(Int.to_string unique_id)
      [ widget
      ; View.button theme "forward" ~on_click:forward
      ; View.button theme "clear" ~on_click:(next_id ())
      ]
  in
  value, view
;;
