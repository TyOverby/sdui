open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
open Bonsai_web_ui_toplayer
module Node_helpers = Virtual_dom_test_helpers.Node_helpers
module Var = Bonsai.Expert.Var

module Helper = struct
  type popover_inputs =
    { content : Node_helpers.t
    ; arrow : Node_helpers.t option
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    ; nested_anchored_popovers : anchored list
    }

  and anchored =
    { inputs : popover_inputs
    ; content_vdom : Vdom.Node.t
    ; arrow_vdom : Vdom.Node.t option
    }

  module Popover_inputs = struct
    type t = popover_inputs =
      { content : Node_helpers.t
      ; arrow : Node_helpers.t option
      ; position : Position.t
      ; alignment : Alignment.t
      ; offset : Offset.t
      ; match_anchor_side_length : Match_anchor_side.t option
      ; nested_anchored_popovers : anchored list
      }
  end

  module Anchored_popover = struct
    type t = anchored =
      { inputs : popover_inputs
      ; content_vdom : Vdom.Node.t
      ; arrow_vdom : Vdom.Node.t option
      }

    let inputs { inputs; _ } = inputs
    let content_vdom { content_vdom; _ } = content_vdom

    let best_effort_wrap_content_in_popover_vdom
      { inputs = { position; alignment; offset; match_anchor_side_length; _ }
      ; content_vdom
      ; arrow_vdom
      }
      =
      let fake_anchor =
        object%js (_self)
          val tagName = Js_of_ocaml.Js.string "fake_anchor_for_anchored_popover"
        end
      in
      Vdom_toplayer.For_testing_bonsai_web_ui_toplayer.wrap_anchored_popover
        ~position
        ~alignment
        ~offset
        ~match_anchor_side_length
        ~content:content_vdom
        ~popover_attrs:[ Vdom.Attr.class_ "currently-untestable" ]
        ~arrow:arrow_vdom
        ~anchor:(Js_of_ocaml.Js.Unsafe.coerce fake_anchor)
    ;;
  end

  module Virtual_popover = struct
    type t =
      { inputs : Popover_inputs.t
      ; popover_vdom : Vdom.Node.t
      }
  end

  module Modal = struct
    type t =
      { vdom : Vdom.Node.t
      ; content : Node_helpers.t
      ; lock_body_scroll : bool
      ; nested_anchored_popovers : anchored list
      }

    let has_attr ~attr_name = function
      | Node_helpers.Text _ | Widget -> false
      | Element { attributes; _ } ->
        List.Assoc.mem attributes ~equal:String.equal attr_name
    ;;

    let is_modal =
      has_attr ~attr_name:Vdom_toplayer.For_testing_bonsai_web_ui_toplayer.modal_attr_name
    ;;

    let lock_body_scroll =
      has_attr
        ~attr_name:
          Vdom_toplayer.For_testing_bonsai_web_ui_toplayer.lock_body_scroll_attr_name
    ;;
  end

  module Result = struct
    type t =
      { anchored_popovers : Anchored_popover.t list
      ; virtual_popovers : Virtual_popover.t list
      ; modals : Modal.t list
      }

    let wrap_app_vdom { anchored_popovers; virtual_popovers; modals } app_vdom =
      let rec to_vdom popover =
        let main_vdom, nested =
          match popover with
          | `Anchored_popover ({ Anchored_popover.inputs; _ } as anchored) ->
            ( Anchored_popover.best_effort_wrap_content_in_popover_vdom anchored
            , inputs.nested_anchored_popovers )
          | `Virtual_popover { Virtual_popover.inputs; popover_vdom } ->
            popover_vdom, inputs.nested_anchored_popovers
          | `Modal { Modal.vdom; nested_anchored_popovers; _ } ->
            vdom, nested_anchored_popovers
        in
        let nested_vdom = List.map nested ~f:(fun x -> to_vdom (`Anchored_popover x)) in
        {%html|
            <div class="popover-for-tests">
              %{main_vdom}
              <div class="nested-popovers-for-tests">*{nested_vdom}</div>
            </div>
          |}
      in
      let anchored_vdom =
        List.map anchored_popovers ~f:(fun x -> to_vdom (`Anchored_popover x))
      in
      let virtual_vdom =
        List.map virtual_popovers ~f:(fun x -> to_vdom (`Virtual_popover x))
      in
      let modal_vdom = List.map modals ~f:(fun x -> to_vdom (`Modal x)) in
      {%html|
          <html id="html-for-tests">
            <div id="app-root-for-tests">%{app_vdom}</div>
            <div id="anchored-popovers-for-tests">*{anchored_vdom}</div>
            <div id="virtual-popovers-for-tests">*{virtual_vdom}</div>
            <div id="modals-for-tests">*{modal_vdom}</div>
          </html>
        |}
    ;;
  end

  let rec extract_anchored_single_node node_helper =
    let hook_inputs =
      Node_helpers.get_hook_value_opt
        node_helper
        ~name:Vdom_toplayer.For_testing_popover_hook.hook_name
        ~type_id:Vdom_toplayer.For_testing_popover_hook.type_id
    in
    match hook_inputs with
    | Some [] ->
      failwith "Invalid state[anchored]: popover hook exists, but contains no popovers."
    | Some inputs ->
      List.map
        inputs
        ~f:
          (fun
            { content; arrow; position; alignment; offset; match_anchor_side_length; _ }
          ->
          let content_helper = Node_helpers.unsafe_convert_exn content in
          { content_vdom = content
          ; arrow_vdom = arrow
          ; inputs =
              { content = content_helper
              ; arrow = Option.map arrow ~f:Node_helpers.unsafe_convert_exn
              ; position
              ; alignment
              ; offset
              ; match_anchor_side_length
              ; nested_anchored_popovers = get_all_anchored content_helper
              }
          })
    | None -> []

  and get_all_anchored root_helper =
    let rec loop node_helpers acc =
      match node_helpers with
      | [] -> (* We could reverse, but order doesn't really matter. *) acc
      | Node_helpers.Widget :: tl | Text _ :: tl -> loop tl acc
      | (Element { children; _ } as curr) :: tl ->
        loop (children @ tl) (extract_anchored_single_node curr @ acc)
    in
    loop [ root_helper ] []
  ;;

  let extract_modal root_vdom root_helper =
    let content =
      match root_helper with
      | Node_helpers.Element
          { children = [ Element { children = [ content ]; _ }; _ ]; _ } -> content
      | Element _ ->
        (* This test has the added benefit of adding a check on the [Popover_dom]
                   changing unexpectedly. *)
        failwith
          "modal structure malformed! This test is likely out of date with the current \
           implementation of [Popover_dom]."
      | _ -> failwith "modal root must be an element"
    in
    Some
      { Modal.vdom = root_vdom
      ; content
      ; lock_body_scroll = Modal.lock_body_scroll root_helper
      ; nested_anchored_popovers = get_all_anchored content
      }
  ;;

  (* Virtual popovers being opened / closed is controlled by Bonsai computations, which
       are active / inactive independently of vdom. Therefore, we can always just pull them
       from the Var that backs portalling. *)
  let extract_virtual root_vdom root_helper =
    let hook_inputs =
      try
        Node_helpers.get_hook_value
          root_helper
          ~name:Floating_positioning_new.For_testing_position_me_hook.hook_name
          ~type_id:Floating_positioning_new.For_testing_position_me_hook.type_id
      with
      | e -> raise_s [%message "Virtual fail" (e : exn) (root_helper : Node_helpers.t)]
    in
    (* We could probably factor something out for structural tests of modals /
         popovers, but:
          - We expect modals not to have arrows
          - The DOM implementations of modals and popovers may diverse someday *)
    let content, arrow =
      match root_helper with
      | Element
          { children =
              [ Element { children = [ content ]; _ }
              ; Element { children = [ arrow ]; _ }
              ; _
              ]
          ; _
          } -> content, Some arrow
      | Element { children = [ Element { children = [ content ]; _ }; _ ]; _ } ->
        content, None
      | Element _ ->
        (* This test has the added benefit of adding a check on the [Popover_dom]
                 changing unexpectedly. *)
        failwith
          "popover structure malformed! This test is likely out of date with the current \
           implementation of [Popover_dom]."
      | _ -> failwith "popover root must be an element"
    in
    Some
      { Virtual_popover.popover_vdom = root_vdom
      ; inputs =
          { content
          ; arrow
          ; position = hook_inputs.position
          ; alignment = hook_inputs.alignment
          ; offset = hook_inputs.offset
          ; match_anchor_side_length = hook_inputs.match_anchor_side_length
          ; nested_anchored_popovers = get_all_anchored content
          }
      }
  ;;

  let extract_non_anchored = function
    | Vdom.Node.None -> None
    | vdom ->
      let helper = Node_helpers.unsafe_convert_exn vdom in
      (match Modal.is_modal helper with
       | true ->
         let%map.Option modal = extract_modal vdom helper in
         `Modal modal
       | false ->
         let%map.Option virtual_ = extract_virtual vdom helper in
         `Virtual_popover virtual_)
  ;;

  let get_toplayer_elements app_vdom =
    let anchored_popovers =
      let%arr app_vdom = app_vdom in
      let app_helper = Node_helpers.unsafe_convert_exn app_vdom in
      get_all_anchored app_helper
    in
    let virtual_and_modals =
      let%arr portals = Bonsai.Expert.Var.value For_testing.portals in
      Map.data portals
      |> List.map ~f:Vdom_toplayer.For_bonsai_web_ui_toplayer.Portal.For_testing.vdom
      |> List.map ~f:extract_non_anchored
      |> List.filter_opt
    in
    let virtual_popovers =
      let%arr virtual_and_modals = virtual_and_modals in
      List.filter_map virtual_and_modals ~f:(function
        | `Virtual_popover x -> Some x
        | `Modal _ -> None)
    in
    let modals =
      let%arr virtual_and_modals = virtual_and_modals in
      List.filter_map virtual_and_modals ~f:(function
        | `Modal x -> Some x
        | `Virtual_popover _ -> None)
    in
    let%arr anchored_popovers = anchored_popovers
    and virtual_popovers = virtual_popovers
    and modals = modals in
    { Result.anchored_popovers; virtual_popovers; modals }
  ;;

  let wrap_app_vdom app_vdom =
    let%arr app_vdom = app_vdom
    and result = get_toplayer_elements app_vdom in
    Result.wrap_app_vdom result app_vdom
  ;;
end

module Actions = struct
  type t =
    | Open
    | Close
    | Reset_models
end

module Popover_test_result = struct
  type t =
    | Open of
        { content_html : string
        ; arrow_html : string option
        ; position : Floating_positioning_new.Position.t
        ; alignment : Floating_positioning_new.Alignment.t
        ; offset : Floating_positioning_new.Offset.t
        ; match_anchor_side_length : Floating_positioning_new.Match_anchor_side.t option
        }
    | Closed
  [@@deriving sexp, equal]

  let conv
    { Helper.Popover_inputs.content
    ; arrow
    ; position
    ; alignment
    ; offset
    ; match_anchor_side_length
    ; nested_anchored_popovers = _
    }
    =
    Open
      { content_html = Node_helpers.to_string_html content
      ; arrow_html = Option.map ~f:Node_helpers.to_string_html arrow
      ; position
      ; alignment
      ; offset
      ; match_anchor_side_length
      }
  ;;
end

module Test_result = struct
  type t =
    { result : Helper.Result.t
    ; open_ : unit Effect.t
    ; close : unit Effect.t
    ; reset_models : unit Effect.t
    }
end

let test_component ?position ?alignment ?offset ?match_anchor_side_length ~content =
  Bonsai.with_model_resetter' ~f:(fun ~reset:reset_models graph ->
    let popover, { Controls.open_ = open_anchored; close = close_anchored; is_open = _ } =
      Popover.create ?position ?alignment ?offset ?match_anchor_side_length ~content graph
    in
    let { Controls.open_ = open_virtual; close = close_virtual; is_open = _ } =
      Popover.create_virtual
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~content
        (Bonsai.return (Anchor.of_coordinate ~x:0. ~y:0.))
        graph
    in
    let app_vdom =
      let%arr popover = popover in
      Vdom.Node.div ~attrs:[ popover ] []
    in
    let%arr result = Helper.get_toplayer_elements app_vdom
    and open_virtual = open_virtual
    and open_anchored = open_anchored
    and close_virtual = close_virtual
    and close_anchored = close_anchored
    and reset_models = reset_models in
    { Test_result.result
    ; open_ = Effect.Many [ open_virtual; open_anchored ]
    ; close = Effect.Many [ close_virtual; close_anchored ]
    ; reset_models
    })
;;

let%test_module "vdom output" =
  (module struct
    let toggle_button ~label open_ close is_open =
      let%arr open_ = open_
      and close = close
      and is_open = is_open in
      let on_click = if is_open then close else open_ in
      let formatted_id =
        "toggle-" ^ Capitalization.apply_to_words Kebab_case (String.split ~on:' ' label)
      in
      {%html|
          <button id=%{formatted_id} on_click=%{fun _ -> on_click}>
            Toggle popover %{formatted_id#String}
          </button>
        |}
    ;;

    let anchored_with_toggle ~label graph =
      let attr, { Controls.open_; close; is_open } =
        Popover.create ~content:(fun ~close:_ _ -> return {%html|%{label#String}|}) graph
      in
      attr, toggle_button ~label open_ close is_open
    ;;

    let virtual_with_toggle ~label graph =
      let { Controls.open_; close; is_open } =
        Popover.create_virtual
          ~content:(fun ~close:_ _ -> return {%html|%{label#String}|})
          (Anchor.of_coordinate ~x:0. ~y:0. |> return)
          graph
      in
      toggle_button ~label open_ close is_open
    ;;

    let create_nested_content
      ?(num_anchored = 1)
      ?(num_virtual = 1)
      ~prefix
      ()
      ~close:_
      graph
      =
      let anchored_attrs, anchored_togglers =
        List.init num_anchored ~f:(fun i ->
          anchored_with_toggle
            ~label:[%string "%{prefix}Nested Anchored %{i + 1#Int}"]
            graph)
        |> List.unzip
      in
      let virtual_togglers =
        List.init num_virtual ~f:(fun i ->
          virtual_with_toggle
            ~label:[%string "%{prefix}Nested Virtual %{i + 1#Int}"]
            graph)
      in
      let%arr anchored_attrs = Bonsai.all anchored_attrs
      and anchored_togglers = Bonsai.all anchored_togglers
      and virtual_togglers = Bonsai.all virtual_togglers in
      let id =
        [%string "%{prefix}-root"] |> String.filter ~f:(fun c -> not (Char.equal c ' '))
      in
      {%html|
          <div>
            <div id=%{id} *{anchored_attrs}></div>
            *{anchored_togglers} *{virtual_togglers}
          </div>
        |}
    ;;

    let test_popovers_and_virtual_popovers graph =
      let anchored1, toggle_anchored1 = anchored_with_toggle ~label:"Anchored 1" graph in
      let anchored2, toggle_anchored2 = anchored_with_toggle ~label:"Anchored 2" graph in
      let ( anchored3
          , { Controls.open_ = open_anchored3
            ; close = close_anchored3
            ; is_open = is_open_anchored3
            } )
        =
        Popover.create ~content:(create_nested_content ~prefix:"Anchored " ()) graph
      in
      let toggle_anchored3 =
        toggle_button ~label:"Anchored 3" open_anchored3 close_anchored3 is_open_anchored3
      in
      let toggle_virtual1 = virtual_with_toggle ~label:"Virtual 1" graph in
      let toggle_virtual2 = virtual_with_toggle ~label:"Virtual 2" graph in
      let { Controls.open_ = open_virtual3
          ; close = close_virtual3
          ; is_open = is_open_virtual3
          }
        =
        Popover.create_virtual
          ~content:(create_nested_content ~prefix:"Virtual " ())
          (Anchor.of_coordinate ~x:0. ~y:0. |> return)
          graph
      in
      let toggle_virtual3 =
        toggle_button ~label:"Virtual 3" open_virtual3 close_virtual3 is_open_virtual3
      in
      let app_root =
        let%arr anchored1 = anchored1
        and anchored2 = anchored2
        and anchored3 = anchored3
        and toggle_anchored1 = toggle_anchored1
        and toggle_anchored2 = toggle_anchored2
        and toggle_anchored3 = toggle_anchored3
        and toggle_virtual1 = toggle_virtual1
        and toggle_virtual2 = toggle_virtual2
        and toggle_virtual3 = toggle_virtual3 in
        {%html|
            <div>
              <div
                class="anchored-root"
                %{anchored1}
                %{anchored2}
                %{anchored3}
              ></div>
              %{toggle_anchored1} %{toggle_anchored2} %{toggle_anchored3} %{toggle_virtual1}
              %{toggle_virtual2} %{toggle_virtual3}
            </div>
          |}
      in
      Helper.wrap_app_vdom app_root
    ;;

    let%expect_test "Multiple and nested" =
      let handle =
        Handle.create (Result_spec.vdom Fn.id) test_popovers_and_virtual_popovers
      in
      Handle.show handle;
      [%expect
        {|
        <html id="html-for-tests">
          <div id="app-root-for-tests">
            <div>
              <div class="anchored-root"> </div>
              <button id="toggle-anchored-1" @on_click>  Toggle popover  toggle-anchored-1 </button>

              <button id="toggle-anchored-2" @on_click>  Toggle popover  toggle-anchored-2 </button>

              <button id="toggle-anchored-3" @on_click>  Toggle popover  toggle-anchored-3 </button>

              <button id="toggle-virtual-1" @on_click>  Toggle popover  toggle-virtual-1 </button>
              <button id="toggle-virtual-2" @on_click>  Toggle popover  toggle-virtual-2 </button>

              <button id="toggle-virtual-3" @on_click>  Toggle popover  toggle-virtual-3 </button>
            </div>
          </div>
          <div id="anchored-popovers-for-tests"> </div>
          <div id="virtual-popovers-for-tests"> </div>
          <div id="modals-for-tests"> </div>
        </html>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-anchored-1";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
            <div id="app-root-for-tests">
              <div>
        -|      <div class="anchored-root"> </div>
        +|      <div class="anchored-root"
        +|           vdom_toplayer_popover=(((content <vdom.node>)(popover_attrs(<vdom.attr> <vdom.attr> <vdom.attr> <vdom.attr>))(arrow())(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())))> </div>
                <button id="toggle-anchored-1" @on_click>  Toggle popover  toggle-anchored-1 </button>


              </div>
            </div>
        -|  <div id="anchored-popovers-for-tests"> </div>
        +|  <div id="anchored-popovers-for-tests">
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)"))
        +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|           vdom_toplayer_popover_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test"> Anchored 1 </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
        +|  </div>
            <div id="virtual-popovers-for-tests"> </div>
            <div id="modals-for-tests"> </div>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-anchored-3";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
              <div>
                <div class="anchored-root"
        -|           vdom_toplayer_popover=(((content <vdom.node>)(popover_attrs(<vdom.attr> <vdom.attr> <vdom.attr> <vdom.attr>))(arrow())(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())))> </div>
        +|           vdom_toplayer_popover=(((content <vdom.node>)(popover_attrs(<vdom.attr> <vdom.attr> <vdom.attr> <vdom.attr>))(arrow())(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length()))((content <vdom.node>)(popover_attrs(<vdom.attr> <vdom.attr> <vdom.attr> <vdom.attr>))(arrow())(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())))> </div>
                <button id="toggle-anchored-1" @on_click>  Toggle popover  toggle-anchored-1 </button>


                <div class="nested-popovers-for-tests"> </div>
              </div>
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)"))
        +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|           vdom_toplayer_popover_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test">
        +|          <div>
        +|            <div id="Anchored-root"> </div>
        +|            <button id="toggle-anchored-nested-anchored-1" @on_click>  Toggle popover  toggle-anchored-nested-anchored-1 </button>
        +|
        +|            <button id="toggle-anchored-nested-virtual-1" @on_click>  Toggle popover  toggle-anchored-nested-virtual-1 </button>
        +|          </div>
        +|        </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
            </div>
            <div id="virtual-popovers-for-tests"> </div>
        |}];
      Handle.click_on
        ~get_vdom:Fn.id
        handle
        ~selector:"#toggle-anchored-nested-anchored-1";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
                  <div class="ppx_css_anonymous_class_hash_replaced_in_test">
                    <div>
        -|            <div id="Anchored-root"> </div>
        +|            <div id="Anchored-root"
        +|                 vdom_toplayer_popover=(((content <vdom.node>)(popover_attrs(<vdom.attr> <vdom.attr> <vdom.attr> <vdom.attr>))(arrow())(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())))> </div>
                      <button id="toggle-anchored-nested-anchored-1" @on_click>  Toggle popover  toggle-anchored-nested-anchored-1 </button>


                  <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
                </div>
        -|      <div class="nested-popovers-for-tests"> </div>
        +|      <div class="nested-popovers-for-tests">
        +|        <div class="popover-for-tests">
        +|          <div popover="manual"
        +|               tabindex="-1"
        +|               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|               class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
        +|               custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)"))
        +|               floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|               vdom_toplayer_popover_inertness=()
        +|               vdom_toplayer_restore_focus_on_close=()>
        +|            <div class="ppx_css_anonymous_class_hash_replaced_in_test"> Anchored Nested Anchored 1 </div>
        +|            <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|          </div>
        +|          <div class="nested-popovers-for-tests"> </div>
        +|        </div>
        +|      </div>
              </div>
            </div>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-1";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
              </div>
            </div>
        -|  <div id="virtual-popovers-for-tests"> </div>
        +|  <div id="virtual-popovers-for-tests">
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           id="bonsai_path_replaced_in_test"
        +|           class="floating_hash_replaced_in_test popover_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test toplayer_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)")(--ppx_css_anonymous_var_1_hash_replaced_in_test white)(--ppx_css_anonymous_var_2_hash_replaced_in_test black)(--ppx_css_anonymous_var_3_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_4_hash_replaced_in_test grey))
        +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|           global-click-listener=((bubbling <fun>))
        +|           global-keydown-listener=((bubbling <fun>))
        +|           global-mousedown-listener=((capture <fun>))
        +|           vdom_toplayer_popover_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()
        +|           @on_keydown>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test"> Virtual 1 </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
        +|  </div>
            <div id="modals-for-tests"> </div>
          </html>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-3";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
                <div class="nested-popovers-for-tests"> </div>
              </div>
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           id="bonsai_path_replaced_in_test"
        +|           class="floating_hash_replaced_in_test popover_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test toplayer_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)")(--ppx_css_anonymous_var_1_hash_replaced_in_test white)(--ppx_css_anonymous_var_2_hash_replaced_in_test black)(--ppx_css_anonymous_var_3_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_4_hash_replaced_in_test grey))
        +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|           global-click-listener=((bubbling <fun>))
        +|           global-keydown-listener=((bubbling <fun>))
        +|           global-mousedown-listener=((capture <fun>))
        +|           vdom_toplayer_popover_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()
        +|           @on_keydown>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test">
        +|          <div>
        +|            <div id="Virtual-root"> </div>
        +|            <button id="toggle-virtual-nested-anchored-1" @on_click>  Toggle popover  toggle-virtual-nested-anchored-1 </button>
        +|
        +|            <button id="toggle-virtual-nested-virtual-1" @on_click>  Toggle popover  toggle-virtual-nested-virtual-1 </button>
        +|          </div>
        +|        </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
            </div>
            <div id="modals-for-tests"> </div>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-nested-virtual-1";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
                <div class="nested-popovers-for-tests"> </div>
              </div>
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           id="bonsai_path_replaced_in_test"
        +|           class="floating_hash_replaced_in_test popover_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test toplayer_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)")(--ppx_css_anonymous_var_1_hash_replaced_in_test white)(--ppx_css_anonymous_var_2_hash_replaced_in_test black)(--ppx_css_anonymous_var_3_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_4_hash_replaced_in_test grey))
        +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|           global-click-listener=((bubbling <fun>))
        +|           global-keydown-listener=((bubbling <fun>))
        +|           global-mousedown-listener=((capture <fun>))
        +|           vdom_toplayer_popover_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()
        +|           @on_keydown>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test"> Virtual Nested Virtual 1 </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
              <div class="popover-for-tests">
                <div popover="manual"
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-nested-anchored-1";
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-3";
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-1";
      Handle.recompute_view_until_stable handle
    ;;

    let modal_with_toggle ~label graph =
      let { Controls.open_; close; is_open } =
        Modal.create ~content:(fun ~close:_ _ -> return {%html|%{label#String}|}) graph
      in
      toggle_button ~label open_ close is_open
    ;;

    let test_modals graph =
      let toggle_modal1 = modal_with_toggle ~label:"Modal 1" graph in
      let toggle_modal2 = modal_with_toggle ~label:"Modal 2" graph in
      let { Controls.open_ = open_modal3; close = close_modal3; is_open = is_open_modal3 }
        =
        Modal.create ~content:(create_nested_content ~prefix:"Modal " ()) graph
      in
      let toggle_modal3 =
        toggle_button ~label:"Modal 3" open_modal3 close_modal3 is_open_modal3
      in
      let app_root =
        let%arr toggle_modal1 = toggle_modal1
        and toggle_modal2 = toggle_modal2
        and toggle_modal3 = toggle_modal3 in
        {%html|<div>%{toggle_modal1} %{toggle_modal2} %{toggle_modal3}</div>|}
      in
      Helper.wrap_app_vdom app_root
    ;;

    let%expect_test "Multiple and nested" =
      let handle = Handle.create (Result_spec.vdom Fn.id) test_modals in
      Handle.show handle;
      [%expect
        {|
        <html id="html-for-tests">
          <div id="app-root-for-tests">
            <div>
              <button id="toggle-modal-1" @on_click>  Toggle popover  toggle-modal-1 </button>

              <button id="toggle-modal-2" @on_click>  Toggle popover  toggle-modal-2 </button>

              <button id="toggle-modal-3" @on_click>  Toggle popover  toggle-modal-3 </button>
            </div>
          </div>
          <div id="anchored-popovers-for-tests"> </div>
          <div id="virtual-popovers-for-tests"> </div>
          <div id="modals-for-tests"> </div>
        </html>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-1";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
            <div id="anchored-popovers-for-tests"> </div>
            <div id="virtual-popovers-for-tests"> </div>
        -|  <div id="modals-for-tests"> </div>
        +|  <div id="modals-for-tests">
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           data-testing-modal=""
        +|           id="bonsai_path_replaced_in_test"
        +|           class="floating_hash_replaced_in_test modal_hash_replaced_in_test popover_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test toplayer_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)")(--ppx_css_anonymous_var_1_hash_replaced_in_test white)(--ppx_css_anonymous_var_2_hash_replaced_in_test black)(--ppx_css_anonymous_var_3_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_4_hash_replaced_in_test grey))
        +|           global-click-listener=((bubbling <fun>))
        +|           global-keydown-listener=((bubbling <fun>))
        +|           global-mousedown-listener=((capture <fun>))
        +|           vdom_toplayer_modal_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()
        +|           vdom_toplayer_show_on_mount=()
        +|           @on_keydown
        +|           @on_toggle>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test"> Modal 1 </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
        +|  </div>
          </html>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-3";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
                <div class="nested-popovers-for-tests"> </div>
              </div>
        +|    <div class="popover-for-tests">
        +|      <div popover="manual"
        +|           tabindex="-1"
        +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|           data-testing-modal=""
        +|           id="bonsai_path_replaced_in_test"
        +|           class="floating_hash_replaced_in_test modal_hash_replaced_in_test popover_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test toplayer_hash_replaced_in_test"
        +|           custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)")(--ppx_css_anonymous_var_1_hash_replaced_in_test white)(--ppx_css_anonymous_var_2_hash_replaced_in_test black)(--ppx_css_anonymous_var_3_hash_replaced_in_test 1px)(--ppx_css_anonymous_var_4_hash_replaced_in_test grey))
        +|           global-click-listener=((bubbling <fun>))
        +|           global-keydown-listener=((bubbling <fun>))
        +|           global-mousedown-listener=((capture <fun>))
        +|           vdom_toplayer_modal_inertness=()
        +|           vdom_toplayer_restore_focus_on_close=()
        +|           vdom_toplayer_show_on_mount=()
        +|           @on_keydown
        +|           @on_toggle>
        +|        <div class="ppx_css_anonymous_class_hash_replaced_in_test">
        +|          <div>
        +|            <div id="Modal-root"> </div>
        +|            <button id="toggle-modal-nested-anchored-1" @on_click>  Toggle popover  toggle-modal-nested-anchored-1 </button>
        +|
        +|            <button id="toggle-modal-nested-virtual-1" @on_click>  Toggle popover  toggle-modal-nested-virtual-1 </button>
        +|          </div>
        +|        </div>
        +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|      </div>
        +|      <div class="nested-popovers-for-tests"> </div>
        +|    </div>
            </div>
          </html>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-nested-anchored-1";
      Handle.recompute_view_until_stable handle;
      Handle.show_diff ~diff_context:2 handle;
      [%expect
        {|
                  <div class="ppx_css_anonymous_class_hash_replaced_in_test">
                    <div>
        -|            <div id="Modal-root"> </div>
        +|            <div id="Modal-root"
        +|                 vdom_toplayer_popover=(((content <vdom.node>)(popover_attrs(<vdom.attr> <vdom.attr> <vdom.attr> <vdom.attr>))(arrow())(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())))> </div>
                      <button id="toggle-modal-nested-anchored-1" @on_click>  Toggle popover  toggle-modal-nested-anchored-1 </button>


                  <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
                </div>
        -|      <div class="nested-popovers-for-tests"> </div>
        +|      <div class="nested-popovers-for-tests">
        +|        <div class="popover-for-tests">
        +|          <div popover="manual"
        +|               tabindex="-1"
        +|               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
        +|               class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
        +|               custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_anonymous_var_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_anonymous_var_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_anonymous_var_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_anonymous_var_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_anonymous_var_6_hash_replaced_in_test"var(--floatingMaxHeight)"))
        +|               floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(strategy Fixed)(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
        +|               vdom_toplayer_popover_inertness=()
        +|               vdom_toplayer_restore_focus_on_close=()>
        +|            <div class="ppx_css_anonymous_class_hash_replaced_in_test"> Modal Nested Anchored 1 </div>
        +|            <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
        +|          </div>
        +|          <div class="nested-popovers-for-tests"> </div>
        +|        </div>
        +|      </div>
              </div>
            </div>
        |}];
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-nested-anchored-1";
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-3";
      Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-1";
      Handle.recompute_view_until_stable handle
    ;;

    module Modal_test_result = struct
      type t =
        { app_root : Vdom.Node.t
        ; modal_desc : Sexp.t
        }

      let sexp_of_t { modal_desc; _ } = modal_desc
      let get_vdom { app_root; _ } = app_root
    end

    let%expect_test "modals pass through lock_body_scroll" =
      let lock_body_scroll = Var.create false in
      let component graph =
        let { Controls.open_; close; is_open } =
          Modal.create
            ~lock_body_scroll:(Var.value lock_body_scroll)
            ~content:(fun ~close:_ _ -> return {%html|Hi|})
            graph
        in
        let toggle =
          let%arr open_ = open_
          and close = close
          and is_open = is_open in
          if is_open then close else open_
        in
        let app_root =
          let%arr toggle = toggle in
          {%html|<button id="toggle-button" on_click=%{fun _ -> toggle}>Toggle</button>|}
        in
        let result = Helper.get_toplayer_elements app_root in
        let modal_desc =
          match%arr result with
          | { modals = []; _ } -> [%sexp "No Modals"]
          | { modals = [ { lock_body_scroll; _ } ]; _ } ->
            [%message "One modal" (lock_body_scroll : bool)]
          | _ -> [%sexp "More than one modal"]
        in
        let%arr modal_desc = modal_desc
        and app_root = app_root in
        { Modal_test_result.modal_desc; app_root }
      in
      let handle =
        Handle.create (Result_spec.sexp (module Modal_test_result)) component
      in
      Handle.show handle;
      [%expect {| "No Modals" |}];
      Handle.click_on
        ~get_vdom:Modal_test_result.get_vdom
        handle
        ~selector:"#toggle-button";
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| ("One modal" (lock_body_scroll false)) |}];
      Var.set lock_body_scroll true;
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| ("One modal" (lock_body_scroll true)) |}];
      Handle.click_on
        ~get_vdom:Modal_test_result.get_vdom
        handle
        ~selector:"#toggle-button";
      Handle.recompute_view_until_stable handle;
      Handle.show handle;
      [%expect {| "No Modals" |}]
    ;;
  end)
;;

(* Test that virtual and anchored are consistent, and that inputs are propogated to the
   hooks correctly. *)
module _ = struct
  module Result_spec = struct
    type t = Test_result.t
    type incoming = Actions.t

    let view
      { Test_result.result = { Helper.Result.anchored_popovers; virtual_popovers; _ }; _ }
      =
      match anchored_popovers, virtual_popovers with
      | [], _ :: _ -> failwith "Virtual popover is open, but anchored is not."
      | _ :: _, [] -> failwith "Anchored popover is open, but virtual is not."
      | _ :: _ :: _, _ ->
        failwith "More than one anchored popover, but expected only one."
      | _, _ :: _ :: _ -> failwith "More than one virtual popover, but expected only one."
      | [], [] -> [%sexp ([ Closed ] : Popover_test_result.t list)] |> Sexp.to_string_hum
      | [ anchored ], [ virtual_ ] ->
        let anchored_sexp =
          [%sexp
            ([ Popover_test_result.conv (Helper.Anchored_popover.inputs anchored) ]
             : Popover_test_result.t list)]
        in
        let virtual_sexp =
          [%sexp
            ([ Popover_test_result.conv virtual_.inputs ] : Popover_test_result.t list)]
        in
        (match Sexp.equal anchored_sexp virtual_sexp with
         | true -> anchored_sexp |> Sexp.to_string_hum
         | false ->
           "Error: popovers produced different result: \n"
           ^ Expect_test_patdiff.patdiff_s anchored_sexp virtual_sexp)
    ;;

    let incoming { Test_result.open_; close; reset_models; _ } = function
      | Actions.Open -> open_
      | Close -> close
      | Reset_models -> reset_models
    ;;
  end

  let get_vdom
    ~for_
    { Test_result.result = { anchored_popovers; virtual_popovers; _ }; _ }
    =
    match for_ with
    | `Virtual ->
      virtual_popovers
      |> List.hd
      |> Option.value_map
           ~f:(fun { popover_vdom; _ } -> popover_vdom)
           ~default:Vdom.Node.None
    | `Anchored ->
      anchored_popovers
      |> List.hd
      |> Option.value_map ~f:Helper.Anchored_popover.content_vdom ~default:Vdom.Node.None
  ;;

  let%expect_test "External open and closing" =
    let test_component =
      test_component ~content:(fun ~close:_ _ ->
        Bonsai.return (View.text "Popover content!"))
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
        (position Auto) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
        (match_anchor_side_length ())))
      |}];
    Handle.do_actions handle [ Close ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}]
  ;;

  let%expect_test "Popover changing directions and alignments" =
    let position_var = Var.create Floating_positioning_new.Position.Top in
    let alignment_var = Var.create Floating_positioning_new.Alignment.Center in
    let offset_var = Var.create Floating_positioning_new.Offset.zero in
    let match_anchor_side_length_var = Var.create None in
    let test_component =
      test_component
        ~position:(Var.value position_var)
        ~alignment:(Var.value alignment_var)
        ~offset:(Var.value offset_var)
        ~match_anchor_side_length:(Var.value match_anchor_side_length_var)
        ~content:(fun ~close:_ _ -> Bonsai.return (View.text "Popover content!"))
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
        (position Top) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
        (match_anchor_side_length ())))
      |}];
    Var.set position_var Left;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Top) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
      +|  (position Left) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
          (match_anchor_side_length ())))
      |}];
    Var.set position_var Top;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Left) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
      +|  (position Top) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
          (match_anchor_side_length ())))
      |}];
    Var.set position_var Bottom;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Top) (alignment Center) (offset ((main_axis 0) (cross_axis 0)))
      -|  (match_anchor_side_length ())))
      +|  (position Bottom) (alignment Center)
      +|  (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Var.set alignment_var Start;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Bottom) (alignment Center)
      -|  (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      +|  (position Bottom) (alignment Start) (offset ((main_axis 0) (cross_axis 0)))
      +|  (match_anchor_side_length ())))
      |}];
    Var.set alignment_var End;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Bottom) (alignment Start) (offset ((main_axis 0) (cross_axis 0)))
      +|  (position Bottom) (alignment End) (offset ((main_axis 0) (cross_axis 0)))
          (match_anchor_side_length ())))
      |}];
    Var.set offset_var { main_axis = 3.; cross_axis = 2. };
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Bottom) (alignment End) (offset ((main_axis 0) (cross_axis 0)))
      +|  (position Bottom) (alignment End) (offset ((main_axis 3) (cross_axis 2)))
          (match_anchor_side_length ())))
      |}];
    Var.set offset_var { main_axis = -3.; cross_axis = 16. };
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
        ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (position Bottom) (alignment End) (offset ((main_axis 3) (cross_axis 2)))
      +|  (position Bottom) (alignment End) (offset ((main_axis -3) (cross_axis 16)))
          (match_anchor_side_length ())))
      |}];
    Var.set match_anchor_side_length_var (Some Grow_to_match);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (position Bottom) (alignment End) (offset ((main_axis -3) (cross_axis 16)))
      -|  (match_anchor_side_length ())))
      +|  (match_anchor_side_length (Grow_to_match))))
      |}];
    Var.set match_anchor_side_length_var (Some Match_exactly);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (position Bottom) (alignment End) (offset ((main_axis -3) (cross_axis 16)))
      -|  (match_anchor_side_length (Grow_to_match))))
      +|  (match_anchor_side_length (Match_exactly))))
      |}];
    Var.set match_anchor_side_length_var (Some Shrink_to_match);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (position Bottom) (alignment End) (offset ((main_axis -3) (cross_axis 16)))
      -|  (match_anchor_side_length (Match_exactly))))
      +|  (match_anchor_side_length (Shrink_to_match))))
      |}];
    Var.set match_anchor_side_length_var None;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (position Bottom) (alignment End) (offset ((main_axis -3) (cross_axis 16)))
      -|  (match_anchor_side_length (Shrink_to_match))))
      +|  (match_anchor_side_length ())))
      |}]
  ;;

  let%expect_test "Closing via the internal button on click" =
    let test_component =
      test_component ~content:(fun ~close _ ->
        let%arr close = close in
        View.vbox
          [ View.text "Popover!"
          ; Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.on_click (fun _ -> close); Vdom.Attr.class_ "close_button" ]
              [ View.text "Close" ]
          ])
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<div style={ display: flex; flex-direction: column; }>\
         \n  <span> Popover! </span>\
         \n  <button class=\"close_button\" @on_click>\
         \n    <span> Close </span>\
         \n  </button>\
         \n</div>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Handle.click_on handle ~selector:".close_button" ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".close_button" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}]
  ;;

  let%expect_test "Closing doesn't affect internal state; resetting closes the popover \
                   and clears its contents."
    =
    let test_component =
      test_component ~content:(fun ~close:_ graph ->
        let count, set_count = Bonsai.state 0 graph in
        let%arr count = count
        and set_count = set_count in
        Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.on_click (fun _ -> set_count (count + 1))
            ; Vdom.Attr.class_ "increment"
            ]
          [ View.text (string_of_int count) ])
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 0 </span>\
         \n</button>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 1 </span>\
         \n</button>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Handle.do_actions handle [ Close ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 1 </span>\
         \n</button>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Handle.do_actions handle [ Reset_models ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 0 </span>\
         \n</button>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}]
  ;;

  let%expect_test "Resetting the insides of a popover doesn't close it" =
    let test_component =
      test_component ~content:(fun ~close:_ graph ->
        Bonsai.with_model_resetter'
          ~f:(fun ~reset graph ->
            let count, set_count = Bonsai.state 0 graph in
            let%arr count = count
            and set_count = set_count
            and reset = reset in
            Vdom.Node.div
              [ Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.on_click (fun _ -> set_count (count + 1))
                    ; Vdom.Attr.class_ "increment"
                    ]
                  [ View.text (string_of_int count) ]
              ; Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.on_click (fun _ -> reset)
                    ; Vdom.Attr.class_ "reset_contents"
                    ]
                  [ Vdom.Node.text "reset" ]
              ])
          graph)
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<div>\
         \n  <button class=\"increment\" @on_click>\
         \n    <span> 0 </span>\
         \n  </button>\
         \n  <button class=\"reset_contents\" @on_click> reset </button>\
         \n</div>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<div>\
         \n  <button class=\"increment\" @on_click>\
         \n    <span> 1 </span>\
         \n  </button>\
         \n  <button class=\"reset_contents\" @on_click> reset </button>\
         \n</div>")
        (arrow_html ()) (position Auto) (alignment Center)
        (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}];
    Handle.click_on
      handle
      ~selector:".reset_contents"
      ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".reset_contents" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff handle;
    [%expect
      {|
        ((Open
          (content_html
            "<div>\
           \n  <button class=\"increment\" @on_click>\
      -|   \n    <span> 1 </span>\
      +|   \n    <span> 0 </span>\
           \n  </button>\
           \n  <button class=\"reset_contents\" @on_click> reset </button>\
           \n</div>")
          (arrow_html ()) (position Auto) (alignment Center)
          (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ())))
      |}]
  ;;
end

include Helper
