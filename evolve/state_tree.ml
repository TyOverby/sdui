open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Snips = Shared.Snips
module Form = Bonsai_web_ui_form.With_manual_view
module Feather = Feather_icon
module Lease_pool = Sd_chain.Lease_pool
module P = Sd.Parameters.Individual

let ul_styles = {%css| list-style-type:none; padding-left:20px; user-select:none; |}
let li_styles = {%css| margin:0; padding:0; |}

let component
  ~state
  ~current_id
  ~inject
  ~seen
  ~set_current_id
  ~override_on_click
  (local_ graph)
  =
  let%arr state
  and current_id
  and inject
  and theme = View.Theme.current graph
  and seen
  and set_current_id
  and override_on_click in
  let tree_structure = Image_tree.Model.tree_structure state in
  let set_on_click id = Vdom.Attr.on_click (fun _ -> set_current_id id) in
  let label_attr =
    {%css| 
        display: inline-flex; 
        align-items: flex-end; 
        cursor: pointer; 
        line-height: 1em; 
        padding: 3px 5px; 
        border:2px solid transparent;
        border-radius: 5px;
        |}
  in
  let maybe_highlight ~state ~id =
    let selected_attrs =
      if [%equal: Image_tree.Unique_id.t option] (Some id) current_id
      then {%css| border-color: #1ba1f2; |}
      else Vdom.Attr.empty
    in
    let seen_attrs =
      if not (Set.mem seen id)
      then (
        match state with
        | Image_tree.Stage.State.Finished _ | Error _ -> {%css| background: #1bf2372e; |}
        | _ -> Vdom.Attr.empty)
      else Vdom.Attr.empty
    in
    Vdom.Attr.many [ selected_attrs; seen_attrs ]
  in
  let remove_button_attrs ~id =
    [ {%css| 
        cursor:pointer; 
        opacity: 0.6;

        &:hover {
          opacity: 1;
          border: 1px solid currentColor;
          border-radius: 3px;
        }
      |}
    ; Vdom.Attr.on_click (fun _ ->
        print_s
          [%message (state.parents : Image_tree.Unique_id.t Image_tree.Unique_id.Map.t)];
        Effect.Many
          [ inject (Image_tree.Action.Remove { id; from_kbd = false })
          ; (if [%equal: Image_tree.Unique_id.t option] (Some id) current_id
             then
               Option.value_map
                 (Map.find state.parents id)
                 ~f:set_current_id
                 ~default:Effect.Ignore
             else Effect.Ignore)
          ; Effect.Stop_propagation
          ])
    ]
  in
  let rec loop
    ~only_child
    ~deindented
    { Image_tree.Model.Tree_structure.id; stage = { desc; state }; children }
    =
    let icon =
      match state with
      | Initial -> Feather.File_text
      | Enqueued -> Feather.Upload
      | In_progress -> Feather.Loader
      | Finished _ -> Feather.Image
      | Error _ -> Feather.Alert_triangle
    in
    let children =
      match children with
      | [] -> Vdom.Node.none
      | [ child ] when only_child -> loop ~only_child:true ~deindented:true child
      | [ _ ] ->
        Vdom.Node.ul
          ~attrs:[ ul_styles ]
          (List.map children ~f:(fun c ->
             Vdom.Node.li [ loop ~only_child:true ~deindented:false c ]))
      | children ->
        Vdom.Node.ul
          ~attrs:[ ul_styles ]
          (List.map children ~f:(fun c ->
             Vdom.Node.li [ loop ~only_child:false ~deindented:false c ]))
    in
    let on_click, highlight =
      match override_on_click, state with
      | Some f, Finished { image; _ } ->
        Vdom.Attr.on_click (fun _ -> f image), {%css| background: yellow; color: black|}
      | _ -> set_on_click id, {%css||}
    in
    Vdom.Node.li
      ~attrs:[ li_styles; Vdom.Attr.id (Image_tree.Unique_id.to_dom_id id) ]
      [ Vdom.Node.span
          ~attrs:[ on_click; maybe_highlight ~state ~id; highlight; label_attr ]
          [ (if deindented
             then
               Feather.svg
                 ~extra_attrs:[ {%css| margin-right: 0.5em; |} ]
                 ~size:(`Em 1)
                 Corner_down_right
             else Vdom.Node.none)
          ; Feather.svg ~extra_attrs:[ {%css| margin-right: 0.5em; |} ] ~size:(`Em 1) icon
          ; Vdom.Node.text (Image_tree.Stage.Kind.to_string desc)
          ; Feather.svg
              ~extra_attrs:({%css| margin-left: 0.5em; |} :: remove_button_attrs ~id)
              ~size:(`Em 1)
              X
          ]
      ; children
      ]
  in
  Vdom.Node.div
    [ Vdom.Node.ul
        ~attrs:[ ul_styles ]
        (List.map tree_structure ~f:(fun child ->
           Vdom.Node.li [ loop ~only_child:false ~deindented:false child ]))
    ; View.button
        theme
        "New Prompt"
        ~attrs:[ {%css| width: 100%; text-align: center; |} ]
        ~on_click:(inject Image_tree.Action.Add_root)
    ]
;;
