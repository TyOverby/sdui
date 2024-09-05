open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Popover = Bonsai_web_ui_popover

module Open_close = struct
  type t =
    | Open
    | Close
    | Toggle
end

let get_vdom (vdom, _) = vdom

module Popover_result_spec = struct
  type t = Vdom.Node.t * Popover.Result.t
  type incoming = Open_close.t

  let view (vdom, popover) =
    let { Popover.Result.wrap = _; open_ = _; close = _; toggle = _; is_open } =
      popover
    in
    let vdom_string =
      vdom
      |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
      |> Virtual_dom_test_helpers.Node_helpers.to_string_html
    in
    [%string
      {|
is_open: %{is_open#Bool}
-----------------
%{vdom_string}|}]
  ;;

  let incoming (_, { Popover.Result.open_; close; wrap = _; toggle; is_open = _ })
    = function
    | Open_close.Open -> open_
    | Close -> close
    | Toggle -> toggle
  ;;
end

let%expect_test "External open and closing" =
  let popover graph =
    let%sub ({ Popover.Result.wrap; _ } as popover) =
      Popover.component
        ~close_when_clicked_outside:(Bonsai.return false)
        ~direction:(Bonsai.return Popover.Direction.Right)
        ~alignment:(Bonsai.return Popover.Alignment.Center)
        ~popover:(fun ~close:_ _graph -> Bonsai.return (View.text "Popover content!"))
        ()
        graph
    in
    let%arr wrap = wrap
    and popover = popover in
    wrap (View.text "Popover base!"), popover
  in
  let handle = Handle.create (module Popover_result_spec) popover in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <span> Popover base! </span>
    </span>
    |}];
  Handle.do_actions handle [ Open ];
  Handle.show handle;
  [%expect
    {|
    is_open: true
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
      <span> Popover base! </span>
      <div id="bonsai_path_replaced_in_test"
           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <span> Popover content! </span>
      </div>
    </span>
    |}];
  Handle.do_actions handle [ Close ];
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <span> Popover base! </span>
    </span>
    |}];
  Handle.do_actions handle [ Toggle ];
  Handle.show handle;
  [%expect
    {|
    is_open: true
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
      <span> Popover base! </span>
      <div id="bonsai_path_replaced_in_test"
           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <span> Popover content! </span>
      </div>
    </span>
    |}];
  Handle.do_actions handle [ Toggle ];
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <span> Popover base! </span>
    </span>
    |}]
;;

let%expect_test "Popover changing directions" =
  let direction_var = Bonsai.Expert.Var.create Popover.Direction.Right in
  let popover graph =
    let%sub ({ Popover.Result.wrap; _ } as popover) =
      Popover.component
        ~close_when_clicked_outside:(Bonsai.return false)
        ~direction:(Bonsai.Expert.Var.value direction_var)
        ~alignment:(Bonsai.return Popover.Alignment.Center)
        ~popover:(fun ~close:_ _graph -> Bonsai.return (View.text "Popover content!"))
        ()
        graph
    in
    let%arr wrap = wrap
    and popover = popover in
    wrap (View.text "Popover entry!"), popover
  in
  let handle = Handle.create (module Popover_result_spec) popover in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <span> Popover entry! </span>
    </span>
    |}];
  Handle.do_actions handle [ Open ];
  Handle.show handle;
  [%expect
    {|
    is_open: true
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
      <span> Popover entry! </span>
      <div id="bonsai_path_replaced_in_test"
           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <span> Popover content! </span>
      </div>
    </span>
    |}];
  Bonsai.Expert.Var.set direction_var Left;
  Handle.show_diff ~diff_context:0 handle;
  [%expect
    {|
    -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    +|<span class="left_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    |}];
  Bonsai.Expert.Var.set direction_var Up;
  Handle.show_diff ~diff_context:0 handle;
  [%expect
    {|
    -|<span class="left_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    +|<span class="tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test top_hash_replaced_in_test">
    |}];
  Bonsai.Expert.Var.set direction_var Down;
  Handle.show_diff ~diff_context:0 handle;
  [%expect
    {|
    -|<span class="tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test top_hash_replaced_in_test">
    +|<span class="bottom_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    |}]
;;

let button ~on_click s =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.id s; Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text s ]
;;

let%expect_test "Opening and closing from within popover base" =
  let direction_var = Bonsai.Expert.Var.create Popover.Direction.Right in
  let popover graph =
    let popover =
      Popover.component
        ~close_when_clicked_outside:(Bonsai.return false)
        ~direction:(Bonsai.Expert.Var.value direction_var)
        ~alignment:(Bonsai.return Popover.Alignment.Center)
        ~popover:(fun ~close:_ _graph -> Bonsai.return (View.text "Popover content!"))
        ()
        graph
    in
    let%arr ({ Popover.Result.wrap; open_; close; toggle = _; is_open = _ } as popover) =
      popover
    in
    ( wrap
        (Vdom.Node.div [ button ~on_click:open_ "open"; button ~on_click:close "close" ])
    , popover )
  in
  let handle = Handle.create (module Popover_result_spec) popover in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <div>
        <button id="open" @on_click> open </button>
        <button id="close" @on_click> close </button>
      </div>
    </span>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"#open";
  Handle.show handle;
  [%expect
    {|
    is_open: true
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
      <div>
        <button id="open" @on_click> open </button>
        <button id="close" @on_click> close </button>
      </div>
      <div id="bonsai_path_replaced_in_test"
           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <span> Popover content! </span>
      </div>
    </span>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"#close";
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <div>
        <button id="open" @on_click> open </button>
        <button id="close" @on_click> close </button>
      </div>
    </span>
    |}]
;;

let%expect_test "Opening from base and closing from dialog" =
  let direction_var = Bonsai.Expert.Var.create Popover.Direction.Right in
  let popover graph =
    let popover =
      Popover.component
        ~close_when_clicked_outside:(Bonsai.return false)
        ~direction:(Bonsai.Expert.Var.value direction_var)
        ~alignment:(Bonsai.return Popover.Alignment.Center)
        ~popover:(fun ~close _graph ->
          let%arr close = close in
          Vdom.Node.button
            ~attrs:[ Vdom.Attr.id "close"; Vdom.Attr.on_click (fun _ -> close) ]
            [ Vdom.Node.text "close" ])
        ()
        graph
    in
    let%arr ({ Popover.Result.wrap; open_; close = _; toggle = _; is_open = _ } as popover)
      =
      popover
    in
    ( wrap
        (Vdom.Node.button
           ~attrs:[ Vdom.Attr.id "open"; Vdom.Attr.on_click (fun _ -> open_) ]
           [ Vdom.Node.text "open" ])
    , popover )
  in
  let handle = Handle.create (module Popover_result_spec) popover in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <button id="open" @on_click> open </button>
    </span>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"#open";
  Handle.show handle;
  [%expect
    {|
    is_open: true
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
      <button id="open" @on_click> open </button>
      <div id="bonsai_path_replaced_in_test"
           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))>
        <button id="close" @on_click> close </button>
      </div>
    </span>
    |}];
  Handle.click_on handle ~get_vdom ~selector:"#close";
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <button id="open" @on_click> open </button>
    </span>
    |}]
;;

let%expect_test "Opening from returned effect and closing by clicking outside." =
  let id = "test-popover" in
  let click_fake_hook_action handle ~hook_name ~hook_id =
    let fake_event =
      object%js
        val target = Js_of_ocaml.Js.null
      end
    in
    Handle.trigger_hook_via
      handle
      ~get_vdom
      ~selector:[%string "[data-test=%{id}]"]
      ~name:hook_name
      hook_id
      ~f:(fun input event ->
        (Vdom.Attr.Global_listeners.For_testing.combine_capture_and_bubbling input) event)
      (Js_of_ocaml.Js.Unsafe.coerce fake_event)
  in
  let hook_triggers =
    (* NOTE: This test tests that both left clicking and right clicking somewhere outside
       of the element has the same behavior. *)
    [ click_fake_hook_action
        ~hook_name:"global-click-listener"
        ~hook_id:Vdom.Attr.Global_listeners.For_testing.click_type_id
    ; click_fake_hook_action
        ~hook_name:"global-contextmenu-listener"
        ~hook_id:Vdom.Attr.Global_listeners.For_testing.contextmenu_type_id
    ; (fun handle ->
        let fake_event =
          object%js
            val key = Js_of_ocaml.Js.string "Escape"
            val code = Js_of_ocaml.Js.string "Escape"
            val location = 0
            val keyCode = 27
          end
        in
        Handle.trigger_hook_via
          handle
          ~get_vdom
          ~selector:[%string "[data-test=%{id}]"]
          ~name:"global-keydown-listener"
          Vdom.Attr.Global_listeners.For_testing.keydown_type_id
          ~f:(fun input event ->
            (Vdom.Attr.Global_listeners.For_testing.combine_capture_and_bubbling input)
              event)
          (Js_of_ocaml.Js.Unsafe.coerce fake_event))
    ]
  in
  List.iter hook_triggers ~f:(fun trigger_hook ->
    let popover graph =
      let popover =
        Popover.component
          ~popover_extra_attr:(Bonsai.return (Vdom.Attr.create "data-test" id))
          ~close_when_clicked_outside:(Bonsai.return true)
            (* NOTE: [close_when_clicked_outside] is set to true. *)
          ~direction:(Bonsai.return Popover.Direction.Right)
          ~alignment:(Bonsai.return Popover.Alignment.Center)
          ~popover:(fun ~close:_ _graph -> Bonsai.return (View.text "Popover content!"))
          ()
          graph
      in
      let%arr ({ Popover.Result.wrap; _ } as popover) = popover in
      wrap (View.text "Popover base!"), popover
    in
    let handle = Handle.create (module Popover_result_spec) popover in
    Handle.show handle;
    [%expect
      {|
      is_open: false
      -----------------
      <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
        <span> Popover base! </span>
      </span>
      |}];
    Handle.do_actions handle [ Open ];
    Handle.show handle;
    [%expect
      {|
      is_open: true
      -----------------
      <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
        <span> Popover base! </span>
        <div id="bonsai_path_replaced_in_test"
             data-test="test-popover"
             class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
             custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
             global-click-listener=((capture <fun>))
             global-contextmenu-listener=((capture <fun>))
             global-keydown-listener=((capture <fun>))>
          <span> Popover content! </span>
        </div>
      </span>
      |}];
    trigger_hook handle;
    Handle.show handle;
    [%expect
      {|
      is_open: false
      -----------------
      <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
        <span> Popover base! </span>
      </span>
      |}])
;;

let%expect_test "Nested popover" =
  let popover graph =
    let popover =
      Popover.component
        ~close_when_clicked_outside:(Bonsai.return true)
        ~direction:(Bonsai.return Popover.Direction.Right)
        ~alignment:(Bonsai.return Popover.Alignment.Center)
        ~popover:(fun ~close:close_popover1 graph ->
          let%sub { Popover.Result.wrap; open_; close = _; toggle = _; is_open = _ } =
            Popover.component
              ~close_when_clicked_outside:(Bonsai.return true)
              ~direction:(Bonsai.return Popover.Direction.Right)
              ~alignment:(Bonsai.return Popover.Alignment.Center)
              ~popover:(fun ~close _graph ->
                let%arr close = close in
                button ~on_click:close "close-popover2")
              ()
              graph
          in
          let%arr wrap = wrap
          and open_ = open_
          and close_popover1 = close_popover1 in
          wrap
            (Vdom.Node.div
               [ button ~on_click:open_ "open-popover2"
               ; button ~on_click:close_popover1 "close-popover1"
               ]))
        ()
        graph
    in
    let%arr ({ Popover.Result.wrap; open_; _ } as popover) = popover in
    wrap (button ~on_click:open_ "open-popover1"), popover
  in
  let handle = Handle.create (module Popover_result_spec) popover in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <button id="open-popover1" @on_click> open-popover1 </button>
    </span>
    |}];
  (* Opens first popover. *)
  Handle.click_on ~get_vdom ~selector:"#open-popover1" handle;
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: false
    +|is_open: true
      -----------------
    -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
    +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    -|  <button id="open-popover1" @on_click> open-popover1 </button>
    +|  <button id="open-popover1" @on_click> open-popover1 </button>
    +|  <div id="bonsai_path_replaced_in_test"
    +|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    +|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    +|       global-click-listener=((capture <fun>))
    +|       global-contextmenu-listener=((capture <fun>))
    +|       global-keydown-listener=((capture <fun>))>
    +|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
    +|      <div>
    +|        <button id="open-popover2" @on_click> open-popover2 </button>
    +|        <button id="close-popover1" @on_click> close-popover1 </button>
    +|      </div>
    +|    </span>
    +|  </div>
      </span>
    |}];
  (* Opens second popover that's inside the first popover. *)
  Handle.click_on ~get_vdom ~selector:"#open-popover2" handle;
  Handle.show_diff handle;
  [%expect
    {|
      is_open: true
      -----------------
      <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
        <button id="open-popover1" @on_click> open-popover1 </button>
        <div id="bonsai_path_replaced_in_test"
             class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
             custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
             global-click-listener=((capture <fun>))
             global-contextmenu-listener=((capture <fun>))
             global-keydown-listener=((capture <fun>))>
    -|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
    +|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
            <div>
              <button id="open-popover2" @on_click> open-popover2 </button>
              <button id="close-popover1" @on_click> close-popover1 </button>
            </div>
    +|      <div id="bonsai_path_replaced_in_test"
    +|           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    +|           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    +|           global-click-listener=((capture <fun>))
    +|           global-contextmenu-listener=((capture <fun>))
    +|           global-keydown-listener=((capture <fun>))>
    +|        <button id="close-popover2" @on_click> close-popover2 </button>
    +|      </div>
          </span>
        </div>
      </span>
    |}];
  (* When the first popover is closed, the second popover is also closed. *)
  Handle.click_on ~get_vdom ~selector:"#close-popover1" handle;
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: true
    +|is_open: false
      -----------------
    -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
        <button id="open-popover1" @on_click> open-popover1 </button>
    -|  <div id="bonsai_path_replaced_in_test"
    -|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    -|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    -|       global-click-listener=((capture <fun>))
    -|       global-contextmenu-listener=((capture <fun>))
    -|       global-keydown-listener=((capture <fun>))>
    -|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    -|      <div>
    -|        <button id="open-popover2" @on_click> open-popover2 </button>
    -|        <button id="close-popover1" @on_click> close-popover1 </button>
    -|      </div>
    -|      <div id="bonsai_path_replaced_in_test"
    -|           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    -|           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    -|           global-click-listener=((capture <fun>))
    -|           global-contextmenu-listener=((capture <fun>))
    -|           global-keydown-listener=((capture <fun>))>
    -|        <button id="close-popover2" @on_click> close-popover2 </button>
    -|      </div>
    -|    </span>
    -|  </div>
      </span>
    |}];
  (* When the first popover is re-opened, the second popover remains open like before. *)
  Handle.click_on ~get_vdom ~selector:"#open-popover1" handle;
  Handle.show_diff handle;
  [%expect
    {|
    -|is_open: false
    +|is_open: true
      -----------------
    -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
    +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
        <button id="open-popover1" @on_click> open-popover1 </button>
    +|  <div id="bonsai_path_replaced_in_test"
    +|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    +|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    +|       global-click-listener=((capture <fun>))
    +|       global-contextmenu-listener=((capture <fun>))
    +|       global-keydown-listener=((capture <fun>))>
    +|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    +|      <div>
    +|        <button id="open-popover2" @on_click> open-popover2 </button>
    +|        <button id="close-popover1" @on_click> close-popover1 </button>
    +|      </div>
    +|      <div id="bonsai_path_replaced_in_test"
    +|           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    +|           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    +|           global-click-listener=((capture <fun>))
    +|           global-contextmenu-listener=((capture <fun>))
    +|           global-keydown-listener=((capture <fun>))>
    +|        <button id="close-popover2" @on_click> close-popover2 </button>
    +|      </div>
    +|    </span>
    +|  </div>
      </span>
    |}];
  Handle.click_on ~get_vdom ~selector:"#close-popover2" handle;
  (* Closing the second popover results in the first one staying open. *)
  Handle.show_diff handle;
  [%expect
    {|
      is_open: true
      -----------------
      <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
        <button id="open-popover1" @on_click> open-popover1 </button>
        <div id="bonsai_path_replaced_in_test"
             class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
             custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
             global-click-listener=((capture <fun>))
             global-contextmenu-listener=((capture <fun>))
             global-keydown-listener=((capture <fun>))>
    -|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
    +|    <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
            <div>
              <button id="open-popover2" @on_click> open-popover2 </button>
              <button id="close-popover1" @on_click> close-popover1 </button>
            </div>
    -|      <div id="bonsai_path_replaced_in_test"
    -|           class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
    -|           custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
    -|           global-click-listener=((capture <fun>))
    -|           global-contextmenu-listener=((capture <fun>))
    -|           global-keydown-listener=((capture <fun>))>
    -|        <button id="close-popover2" @on_click> close-popover2 </button>
    -|      </div>
          </span>
        </div>
      </span>
    |}]
;;

let%test_module "interactions with [with_model_resetter]" =
  (module struct
    module Popover_result_spec = struct
      type t = Vdom.Node.t * Popover.Result.t * unit Effect.t

      type incoming =
        | Open
        | Close
        | Reset
      [@@deriving variants]

      let get_vdom (vdom, _, _) = vdom

      let view (vdom, _, _) =
        vdom
        |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
        |> Virtual_dom_test_helpers.Node_helpers.to_string_html
             ?filter_printed_attributes:None
             ~censor_paths:true
             ~censor_hash:true
      ;;

      let incoming
        (_, { Popover.Result.open_; close; wrap = _; toggle = _; is_open = _ }, reset)
        = function
        | Open -> open_
        | Close -> close
        | Reset -> reset
      ;;
    end

    let popover graph =
      let state, resetter =
        Bonsai.with_model_resetter
          ~f:(fun graph ->
            let popover =
              Popover.component
                ~close_when_clicked_outside:(Bonsai.return true)
                ~direction:(Bonsai.return Popover.Direction.Right)
                ~alignment:(Bonsai.return Popover.Alignment.Center)
                ~popover:(fun ~close:_ graph ->
                  let x, reset =
                    Bonsai.with_model_resetter
                      ~f:(fun _graph -> Bonsai.return (View.text "Popover!"))
                      graph
                  in
                  let%arr x = x
                  and reset = reset in
                  Vdom.Node.div [ x; button ~on_click:reset "reset-popover" ])
                ()
                graph
            in
            let x, reset =
              Bonsai.with_model_resetter
                ~f:(fun _graph -> Bonsai.return (View.text "Popover base!"))
                graph
            in
            let%arr ({ Popover.Result.wrap; _ } as popover) = popover
            and x = x
            and reset = reset in
            wrap (Vdom.Node.div [ x; button ~on_click:reset "reset-base" ]), popover)
          graph
      in
      let%sub vdom, popover = state in
      let%arr vdom = vdom
      and popover = popover
      and resetter = resetter in
      vdom, popover, resetter
    ;;

    let%expect_test "resetting entire computation" =
      let handle = Handle.create (module Popover_result_spec) popover in
      Handle.show handle;
      [%expect
        {|
        <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
          <div>
            <span> Popover base! </span>
            <button id="reset-base" @on_click> reset-base </button>
          </div>
        </span>
        |}];
      Handle.do_actions handle [ Open ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
        +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
            <div>
              <span> Popover base! </span>
              <button id="reset-base" @on_click> reset-base </button>
            </div>
        +|  <div id="bonsai_path_replaced_in_test"
        +|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
        +|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
        +|       global-click-listener=((capture <fun>))
        +|       global-contextmenu-listener=((capture <fun>))
        +|       global-keydown-listener=((capture <fun>))>
        +|    <div>
        +|      <span> Popover! </span>
        +|      <button id="reset-popover" @on_click> reset-popover </button>
        +|    </div>
        +|  </div>
          </span>
        |}];
      Handle.do_actions handle [ Reset ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
        +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
            <div>
              <span> Popover base! </span>
              <button id="reset-base" @on_click> reset-base </button>
            </div>
        -|  <div id="bonsai_path_replaced_in_test"
        -|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
        -|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
        -|       global-click-listener=((capture <fun>))
        -|       global-contextmenu-listener=((capture <fun>))
        -|       global-keydown-listener=((capture <fun>))>
        -|    <div>
        -|      <span> Popover! </span>
        -|      <button id="reset-popover" @on_click> reset-popover </button>
        -|    </div>
        -|  </div>
          </span>
        |}]
    ;;

    let%expect_test "resetting popover" =
      let handle = Handle.create (module Popover_result_spec) popover in
      Handle.show handle;
      [%expect
        {|
        <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
          <div>
            <span> Popover base! </span>
            <button id="reset-base" @on_click> reset-base </button>
          </div>
        </span>
        |}];
      Handle.do_actions handle [ Open ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
        +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
            <div>
              <span> Popover base! </span>
              <button id="reset-base" @on_click> reset-base </button>
            </div>
        +|  <div id="bonsai_path_replaced_in_test"
        +|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
        +|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
        +|       global-click-listener=((capture <fun>))
        +|       global-contextmenu-listener=((capture <fun>))
        +|       global-keydown-listener=((capture <fun>))>
        +|    <div>
        +|      <span> Popover! </span>
        +|      <button id="reset-popover" @on_click> reset-popover </button>
        +|    </div>
        +|  </div>
          </span>
        |}];
      Handle.click_on
        ~get_vdom:Popover_result_spec.get_vdom
        ~selector:"#reset-popover"
        handle;
      Handle.show_diff handle
    ;;

    let%expect_test "resetting returned output" =
      let handle = Handle.create (module Popover_result_spec) popover in
      Handle.show handle;
      [%expect
        {|
        <span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
          <div>
            <span> Popover base! </span>
            <button id="reset-base" @on_click> reset-base </button>
          </div>
        </span>
        |}];
      Handle.do_actions handle [ Open ];
      Handle.show_diff handle;
      [%expect
        {|
        -|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
        +|<span class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test tooltip_open_hash_replaced_in_test">
            <div>
              <span> Popover base! </span>
              <button id="reset-base" @on_click> reset-base </button>
            </div>
        +|  <div id="bonsai_path_replaced_in_test"
        +|       class="default_tooltip_styles_hash_replaced_in_test tooltip_hash_replaced_in_test"
        +|       custom-css-vars=((--fg_hash_replaced_in_test black)(--border_hash_replaced_in_test grey)(--bg_hash_replaced_in_test white))
        +|       global-click-listener=((capture <fun>))
        +|       global-contextmenu-listener=((capture <fun>))
        +|       global-keydown-listener=((capture <fun>))>
        +|    <div>
        +|      <span> Popover! </span>
        +|      <button id="reset-popover" @on_click> reset-popover </button>
        +|    </div>
        +|  </div>
          </span>
        |}];
      Handle.click_on
        ~get_vdom:Popover_result_spec.get_vdom
        ~selector:"#reset-base"
        handle;
      Handle.show_diff handle
    ;;
  end)
;;

let%expect_test "popover with an extra base attr" =
  let popover graph =
    let%sub ({ Popover.Result.wrap; _ } as popover) =
      Popover.component
        ~close_when_clicked_outside:(Bonsai.return false)
        ~base_extra_attr:
          (Bonsai.return (Vdom.Attr.create "data-test" "I am attached as a base attr."))
        ~direction:(Bonsai.return Popover.Direction.Right)
        ~alignment:(Bonsai.return Popover.Alignment.Center)
        ~popover:(fun ~close:_ _graph -> Bonsai.return (View.text "Popover content!"))
        ()
        graph
    in
    let%arr wrap = wrap
    and popover = popover in
    wrap (View.text "Popover base!"), popover
  in
  let handle = Handle.create (module Popover_result_spec) popover in
  Handle.show handle;
  [%expect
    {|
    is_open: false
    -----------------
    <span data-test="I am attached as a base attr."
          class="right_hash_replaced_in_test tooltip_container_hash_replaced_in_test">
      <span> Popover base! </span>
    </span>
    |}]
;;
