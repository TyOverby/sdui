open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Floating_positioning_new
module Position = Position
module Alignment = Alignment
module Offset = Offset
module Anchor = Anchor
module Match_anchor_side = Match_anchor_side

type mouse_event = Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t

let focus_on_open_attr = function
  | false -> Vdom.Attr.empty
  | true -> Vdom_toplayer.For_bonsai_web_ui_toplayer.focus_popover_on_open
;;

module Close_on_click_outside = struct
  type t =
    | Yes
    | Yes_unless_target_is_popover
    | No
end

module Controls = struct
  open Js_of_ocaml

  type t =
    { open_ : unit Effect.t Bonsai.t
    ; close : unit Effect.t Bonsai.t
    ; is_open : bool Bonsai.t
    }

  let element_contains node other_node =
    Js.Unsafe.meth_call node "contains" [| Js.Unsafe.inject other_node |]
  ;;

  let element_inert (element : Dom_html.element Js.t) =
    element##hasAttribute (Js.string "inert") |> Js.to_bool
  ;;

  let event_target_inside_a_popover (ev : mouse_event) =
    Option.bind
      (ev##.target |> Js_of_ocaml.Js.Opt.to_option)
      ~f:Vdom_toplayer.For_bonsai_web_ui_toplayer.find_nearest_popover_ancestor
    |> Option.is_some
  ;;

  let on_evt_outside ~eff ~root_id (ev : mouse_event) =
    match Js_of_ocaml.Dom_html.getElementById_opt root_id with
    | None -> Effect.Ignore
    | Some root ->
      if element_contains root ev##.target || element_inert root
      then Effect.Ignore
      else eff ~click_target_was_another_popover:(event_target_inside_a_popover ev)
  ;;

  let on_esc_attrs ~eff ~root_id =
    (* We have to use [stopPropagation] so that we don't close parent popovers. *)
    let run_if_esc ~inner_eff ev =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
      | Escape ->
        (match Js_of_ocaml.Dom_html.getElementById_opt root_id with
         | None -> Effect.Ignore
         | Some root when element_inert root -> Effect.Ignore
         | Some _ -> inner_eff)
      | _ -> Effect.Ignore
    in
    [ Vdom.Attr.Global_listeners.keydown
        ~phase:Bubbling
        ~f:(run_if_esc ~inner_eff:(eff ~focus_inside:false))
    ; Vdom.Attr.on_keydown (fun ev ->
        if element_contains ev##.currentTarget ev##.target
        then
          run_if_esc
            ~inner_eff:(Effect.Many [ Effect.Stop_propagation; eff ~focus_inside:true ])
            ev
        else Effect.Ignore)
    ]
  ;;

  (* [event.target] for click events is where the click ended, not where it began.
     So if you mouse down inside of a popover, drag your mouse to outside of it,
     and release, that will register as a "click outside", and potentially close the
     popover.

     We could work around this by closing on mousedown, but this is not what users expect.

     Instead, if the mousedown immediately before a click was inside of the popover,
     the click will not close that popover. *)
  let monitor_mousedown ~root_id graph =
    let last_mousedown_was_inside, set_last_mousedown_was_inside =
      Bonsai.state `Outside graph
    in
    let monitor_mousedowns_attr =
      let%arr set_last_mousedown_was_inside = set_last_mousedown_was_inside
      and root_id = root_id in
      Vdom.Attr.Global_listeners.mousedown
        ~phase:Vdom.Attr.Global_listeners.Phase.Capture
        ~f:(fun (ev : mouse_event) ->
          match Js_of_ocaml.Dom_html.getElementById_opt root_id with
          | None -> Effect.Ignore
          | Some root ->
            set_last_mousedown_was_inside
              (if element_contains root ev##.target
               then `Inside_self
               else if event_target_inside_a_popover ev
               then `Inside_another_popover
               else `Outside))
    in
    monitor_mousedowns_attr, last_mousedown_was_inside
  ;;

  let listeners ~on_click_outside ~on_right_click_outside ~on_esc graph =
    let root_id = Bonsai.path_id graph in
    let monitor_mousedowns_attr, last_mousedown_was_inside =
      monitor_mousedown ~root_id graph
    in
    let%sub click, right_click =
      let%arr on_click_outside = on_click_outside
      and on_right_click_outside = on_right_click_outside
      and root_id = root_id
      and peek_last_mousedown = Bonsai.peek last_mousedown_was_inside graph in
      let build_click_listener ~kind ~f =
        match f with
        | None -> Vdom.Attr.empty
        | Some f ->
          let listener_f =
            match kind with
            | `Click -> Vdom.Attr.Global_listeners.click
            | `Right_click -> Vdom.Attr.Global_listeners.contextmenu
          in
          let close_effect ~click_target_was_another_popover =
            match kind with
            | `Right_click -> f ~click_target_was_another_popover
            | `Click ->
              (match%bind.Effect peek_last_mousedown with
               (* If the click "started" inside the popover, we disregard it because
                 clicking inside, then dragging outside and releasing shouldn't close. *)
               | Inactive | Active `Inside_self -> Effect.Ignore
               | Active `Inside_another_popover ->
                 f ~click_target_was_another_popover:true
               | Active `Outside -> f ~click_target_was_another_popover)
          in
          listener_f
            ~phase:Vdom.Attr.Global_listeners.Phase.Bubbling
            ~f:(on_evt_outside ~eff:close_effect ~root_id)
      in
      ( build_click_listener ~kind:`Click ~f:on_click_outside
      , build_click_listener ~kind:`Right_click ~f:on_right_click_outside )
    in
    let escape =
      match%sub on_esc with
      | None -> return []
      | Some eff ->
        let%arr eff = eff
        and root_id = root_id in
        on_esc_attrs ~eff ~root_id
    in
    let%arr root_id = root_id
    and monitor_mousedowns_attr = monitor_mousedowns_attr
    and click = click
    and right_click = right_click
    and escape = escape in
    Vdom.Attr.many
      ([ Vdom.Attr.id root_id; click; right_click; monitor_mousedowns_attr ] @ escape)
  ;;

  let control_attr
    ~close
    ?(close_on_click_outside = return Close_on_click_outside.Yes)
    ?(close_on_right_click_outside = return Close_on_click_outside.No)
    ?(close_on_esc = return true)
    graph
    =
    let build_on_click conf =
      match%sub conf with
      | Close_on_click_outside.No -> return None
      | Yes_unless_target_is_popover ->
        let%arr close = close in
        Some
          (fun ~click_target_was_another_popover ->
            if click_target_was_another_popover then Effect.Ignore else close)
      | Yes ->
        let%arr close = close in
        Some (fun ~click_target_was_another_popover:_ -> close)
    in
    let on_click_outside = build_on_click close_on_click_outside in
    let on_right_click_outside = build_on_click close_on_right_click_outside in
    let on_esc =
      match%sub close_on_esc with
      | false -> return None
      | true ->
        let%arr close = close in
        Some (fun ~focus_inside:_ -> close)
    in
    listeners ~on_click_outside ~on_right_click_outside ~on_esc graph
  ;;

  let create ?close_on_click_outside ?close_on_right_click_outside ?close_on_esc graph =
    let is_open, set_open = Bonsai.state false graph in
    let%sub open_, close =
      let%arr set_open = set_open in
      set_open true, set_open false
    in
    ( control_attr
        ~close
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    , { is_open; open_; close } )
  ;;

  module For_external_state = struct
    type t = Vdom.Attr.t

    let create = control_attr
  end
end

module Popover = struct
  module For_external_state = struct
    let from_theme ?(has_arrow = false) ?offset theme =
      let constants = View.constants theme in
      let offset =
        match offset, has_arrow with
        | Some offset, _ -> offset
        | None, false ->
          { Offset.main_axis = constants.toplayer.popover_default_offset_px
          ; cross_axis = 0.
          }
        | None, true ->
          { Offset.main_axis = constants.toplayer.popover_with_arrow_default_offset_px
          ; cross_axis = 0.
          }
      in
      let arrow =
        match has_arrow with
        | false -> None
        | true -> View.For_components.Toplayer.popover_arrow theme |> Some
      in
      View.For_components.Toplayer.popover_styles theme, offset, arrow
    ;;

    let transpose_join_opt v = Bonsai.transpose_opt v |> Bonsai.map ~f:Option.join

    let opt
      ?(extra_attrs = return [])
      ?(controls = return Vdom.Attr.empty)
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ?(focus_on_open = Bonsai.return false)
      ?has_arrow
      ~is_open
      ~content
      graph
      =
      match%sub is_open with
      | None -> return Vdom.Attr.empty
      | Some input ->
        let%arr theme = View.Theme.current graph
        and controls = controls
        and position = Bonsai.transpose_opt position
        and alignment = Bonsai.transpose_opt alignment
        and offset = Bonsai.transpose_opt offset
        and match_anchor_side_length = transpose_join_opt match_anchor_side_length
        and focus_on_open = focus_on_open
        and has_arrow = Bonsai.transpose_opt has_arrow
        and extra_attrs = extra_attrs
        and content = content input graph in
        let popover_style, offset, arrow = from_theme ?has_arrow ?offset theme in
        Vdom_toplayer.popover
          ~popover_attrs:
            (popover_style :: focus_on_open_attr focus_on_open :: controls :: extra_attrs)
          ?position
          ?alignment
          ~offset
          ?match_anchor_side_length
          ?arrow
          content
    ;;

    let bool
      ?extra_attrs
      ?controls
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ?focus_on_open
      ?has_arrow
      ~is_open
      ~content
      =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt
        ?extra_attrs
        ?controls
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ?focus_on_open
        ?has_arrow
        ~is_open
        ~content
    ;;

    let opt_virtual
      ?(extra_attrs = return [])
      ?(controls = return Vdom.Attr.empty)
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ?(focus_on_open = Bonsai.return false)
      ?has_arrow
      ~is_open
      ~content
      anchor
      graph
      =
      let node =
        match%sub is_open with
        | None -> return (Vdom.Node.none_deprecated [@alert "-deprecated"])
        | Some input ->
          let%arr theme = View.Theme.current graph
          and controls = controls
          and position = Bonsai.transpose_opt position
          and alignment = Bonsai.transpose_opt alignment
          and offset = Bonsai.transpose_opt offset
          and match_anchor_side_length = transpose_join_opt match_anchor_side_length
          and focus_on_open = focus_on_open
          and has_arrow = Bonsai.transpose_opt has_arrow
          and extra_attrs = extra_attrs
          and content = content input graph
          and anchor = anchor in
          let popover_style, offset, arrow = from_theme ?has_arrow ?offset theme in
          Vdom_toplayer.For_use_in_portals.popover_custom
            ~popover_attrs:
              (popover_style
               :: focus_on_open_attr focus_on_open
               :: controls
               :: extra_attrs)
            ?position
            ?alignment
            ~offset
            ?match_anchor_side_length
            ?arrow
            ~popover_content:content
            anchor
      in
      Portal.bonsai_driven node graph
    ;;

    let bool_virtual
      ?extra_attrs
      ?controls
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ?focus_on_open
      ?has_arrow
      ~is_open
      ~content
      =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt_virtual
        ?extra_attrs
        ?controls
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ?focus_on_open
        ?has_arrow
        ~is_open
        ~content
    ;;
  end

  let create
    ?(extra_attrs = return [])
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?focus_on_open
    ?has_arrow
    ~content
    graph
    =
    let control_attr, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs = extra_attrs
      and control_attr = control_attr in
      control_attr :: extra_attrs
    in
    ( For_external_state.bool
        ~extra_attrs
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ?focus_on_open
        ?has_arrow
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        graph
    , controls )
  ;;

  let create_virtual
    ?(extra_attrs = return [])
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?focus_on_open
    ?has_arrow
    ~content
    anchor
    graph
    =
    let control_attr, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs = extra_attrs
      and control_attr = control_attr in
      control_attr :: extra_attrs
    in
    let () =
      For_external_state.bool_virtual
        ~extra_attrs
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ?focus_on_open
        ?has_arrow
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        anchor
        graph
    in
    controls
  ;;
end

module Modal = struct
  module For_external_state = struct
    let opt
      ?(extra_attrs = return [])
      ?(controls = return Vdom.Attr.empty)
      ?lock_body_scroll
      ?(focus_on_open = Bonsai.return true)
      ~is_open
      ~content
      graph
      =
      let node =
        match%sub is_open with
        | None -> return (Vdom.Node.none_deprecated [@alert "-deprecated"])
        | Some input ->
          let%arr theme = View.Theme.current graph
          and lock_body_scroll = Bonsai.transpose_opt lock_body_scroll
          and focus_on_open = focus_on_open
          and extra_attrs = extra_attrs
          and controls = controls
          and content = content input graph in
          let modal_styles = View.For_components.Toplayer.modal_styles theme in
          Vdom_toplayer.For_use_in_portals.modal
            ~modal_attrs:
              (modal_styles :: focus_on_open_attr focus_on_open :: controls :: extra_attrs)
            ?lock_body_scroll
            content
      in
      Portal.bonsai_driven node graph
    ;;

    let bool ?extra_attrs ?controls ?lock_body_scroll ?focus_on_open ~is_open ~content =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt ?extra_attrs ?controls ?lock_body_scroll ?focus_on_open ~is_open ~content
    ;;
  end

  let create
    ?(extra_attrs = return [])
    ?(close_on_click_outside = return Close_on_click_outside.Yes_unless_target_is_popover)
    ?close_on_right_click_outside
    ?close_on_esc
    ?lock_body_scroll
    ?focus_on_open
    ~content
    graph
    =
    let control_attr, controls =
      Controls.create
        ~close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs = extra_attrs
      and control_attr = control_attr in
      control_attr :: extra_attrs
    in
    let () =
      For_external_state.bool
        ~extra_attrs
        ?lock_body_scroll
        ?focus_on_open
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        graph
    in
    controls
  ;;
end

module For_testing = Portal.For_testing
