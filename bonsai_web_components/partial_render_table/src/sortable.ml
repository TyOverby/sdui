open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table_protocol

type 'col_id t =
  { order : 'col_id Order.t
  ; inject : 'col_id Order.Action.t -> unit Effect.t
  ; col_id_equal : 'col_id -> 'col_id -> bool
  }

let order t = t.order
let inject t = t.inject

let state ?(initial_order = Bonsai.return Order.default) ~equal:col_id_equal () graph =
  let equal = Order.equal col_id_equal in
  let%sub order, inject =
    Bonsai_extra.state_machine0_dynamic_model
      ~equal
      ~model:(`Given initial_order)
      ~apply_action:(fun _ -> Order.apply_action ~equal:col_id_equal)
      ()
      graph
  in
  let order = Bonsai.cutoff ~equal order in
  let%arr order = order
  and inject = inject in
  { order; inject; col_id_equal }
;;

module Header = struct
  let assoc_findi ~f list =
    match List.findi list ~f:(fun _i (key, _) -> f key) with
    | None -> None
    | Some (index, (_k, v)) -> Some (index, v)
  ;;

  module Expert = struct
    let default_click_handler
      ?(multisort_columns_when = `Shift_click)
      { order; inject; col_id_equal }
      ~column_id
      ~sortable
      f
      =
      let handle_click =
        Vdom.Attr.on_click (fun mouse_event ->
          let shift = Js_of_ocaml.Js.to_bool mouse_event##.shiftKey in
          let ctrl = Js_of_ocaml.Js.to_bool mouse_event##.ctrlKey in
          let should_multisort =
            match multisort_columns_when with
            | `Shift_click -> shift
            | `Ctrl_click -> ctrl
            | `Shift_or_ctrl_click -> shift || ctrl
          in
          if should_multisort
          then inject (Add_sort column_id)
          else inject (Set_sort column_id))
      in
      let (sort_state : Sort_state.t) =
        if not sortable
        then Not_sortable
        else (
          let col_state = assoc_findi ~f:(col_id_equal column_id) order in
          match col_state with
          | None -> Not_sorted
          | Some (index, dir) ->
            if List.length order = 1
            then Single_sort dir
            else Multi_sort { dir; index = index + 1 })
      in
      let header_node = f sort_state in
      Table_view.Header_label.wrap_clickable ~sortable ~handle_click header_node
    ;;
  end

  let with_icon = Table_view.Header_label.wrap_with_icon

  (* Not in `Table_view.ml`, because we don't add theming to legacy implementations. *)
  module Legacy = struct
    module Icons = struct
      (** White Diamond symbol (U+25C7) *)
      let neutral = "◇ "

      (** Diamond with Top Black Half symbol (U+2B18) *)
      let ascending = "⬘ "

      (** Diamond with Bottom Black Half symbol (U+2B19) *)
      let descending = "⬙ "
    end

    let wrap_with_icon (label : Vdom.Node.t) (sort_spec : Sort_state.t) =
      let get_icon = function
        | `None -> Icons.neutral
        | `Asc -> Icons.ascending
        | `Desc -> Icons.descending
      in
      let render ~dir = Vdom.Node.span [ Vdom.Node.text (get_icon dir); label ] in
      match sort_spec with
      | Not_sortable -> label
      | Not_sorted -> render ~dir:`None
      | Single_sort dir | Multi_sort { dir; _ } -> render ~dir
    ;;
  end
end
