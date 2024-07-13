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
  Bonsai.Edge.on_change
    image
    ~equal:Sd.Base64_image.equal
    ~callback:
      (let%arr inject = inject in
       fun _ -> inject `Invalidate)
    graph;
  let view =
    let%arr theme = View.Theme.current graph
    and unique_id = unique_id
    and next_id = next_id
    and inject = inject
    and _value = value
    and { Bonsai_web_ui_widget.view = widget; read; _ } = widget in
    let forward =
      let%bind.Effect effects =
        read (fun _input state -> inject (`Set_value (Js.to_string state##composite)))
      in
      Effect.all_unit effects
    in
    (*let is_dirty =
      match value with
      | Fresh _ -> false
      | _ -> true
      in *)
    let is_dirty = true in
    Vdom.Node.div
      ~key:(Int.to_string unique_id)
      [ widget
      ; (if is_dirty
         then View.button theme "forward" ~on_click:forward
         else Vdom.Node.none)
      ; View.button theme "clear" ~on_click:(next_id ())
      ]
  in
  value, view
;;

let do_the_assoc all graph =
  let out =
    Bonsai.assoc_list
      (module Int)
      ~get_key:Tuple2.get1
      all
      graph
      ~f:(fun _ image graph ->
        let%sub _, image = image in
        Tuple2.uncurry Bonsai.both (component image graph))
  in
  let%arr out = out in
  match out with
  | `Ok v ->
    let results, views = List.unzip v in
    let results = Inc.Or_error_or_stale.all results in
    ( results
    , View.hbox
        (List.mapi views ~f:(fun i view -> Vdom.Node.div ~key:(Int.to_string i) [ view ]))
    )
  | `Duplicate_key i ->
    let results =
      Inc.Or_error_or_stale.Error (Error.create_s [%message "duplicate key" (i : int)])
    in
    let view = Vdom.Node.textf "dupldate key: %d" i in
    results, view
;;

let multi
  ~(prev : Sd.Base64_image.t list Inc.Or_error_or_stale.t Bonsai.t)
  (graph : Bonsai.graph)
  : Sd.Base64_image.t list Inc.Or_error_or_stale.t Bonsai.t * Vdom.Node.t Bonsai.t
  =
  let prev = Inc.map_pure prev ~f:(List.mapi ~f:Tuple2.create) in
  let%sub a, b =
    match%sub prev with
    | Inc.Or_error_or_stale.Fresh all | Stale all ->
      let%sub results, view = do_the_assoc all graph in
      Bonsai.both (Inc.map2_pure prev results ~f:(fun _ r -> r)) view
    | Error e ->
      let%arr e = e in
      Inc.Or_error_or_stale.Error e, Vdom.Node.textf "Error: %s" (Error.to_string_hum e)
    | Not_computed ->
      Bonsai.return (Inc.Or_error_or_stale.Not_computed, Vdom.Node.text "not computed")
  in
  a, b
;;
