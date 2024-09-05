open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(** This control is similar to the typeahead control, differing in the fact that it
    doesn't aim to complete your input. Think of this control as a multi-select that
    you're free to add random values to. *)

let input ~placeholder:placeholder_ ~value:value_ ~extra_attr ~id:id_ ~on_input:on_input_ =
  Vdom.Node.input
    ~attrs:
      [ Vdom.Attr.(
          extra_attr
          @ type_ "text"
          @ create "list" id_
          @ placeholder placeholder_
          (* Both Attr.value and Attr.string_property value must be set. The former only affects
             initial control state while the latter affects the control state whilst the form is
             being used. *)
          @ value value_
          @ value_prop value_
          @ on_change (fun _ input -> on_input_ input))
      ]
    ()
;;

let pills ~selected_options ~on_set_change ~inject_selected_options =
  let pill option =
    let remove_option _ =
      let selected_options = Set.remove selected_options option in
      Effect.Many
        [ on_set_change selected_options; inject_selected_options selected_options ]
    in
    Vdom.Node.span
      ~attrs:
        [ Vdom.Attr.(
            tabindex 0
            @ create "data-value" option
            @ on_click remove_option
            @ on_keyup (fun ev ->
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
              | Space | Enter | NumpadEnter | Backspace | Delete -> remove_option ev
              | _ -> Effect.Ignore))
        ]
      [ Vdom.Node.text (option ^ " ×") ]
  in
  if Set.is_empty selected_options
  then Vdom.Node.none_deprecated [@alert "-deprecated"]
  else
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "bonsai-web-ui-freeform-multiselect-pills" ]
      (Set.to_list selected_options |> List.map ~f:pill)
;;

let input
  ~placeholder
  ~extra_attr
  ~split
  ~id
  ~selected_options
  ~inject_selected_options
  ~on_set_change
  (* This state is held internally to force the typeahead to clear the text contents
     of the input field when an option is selected. *)
    graph
  =
  let select, inject_select =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t] graph
  in
  let%arr select = select
  and inject_select = inject_select
  and selected_options = selected_options
  and inject_selected_options = inject_selected_options
  and extra_attr = extra_attr
  and id = id
  and on_set_change = on_set_change in
  let on_input user_input =
    let maybe_changed_options =
      split user_input
      |> String.Set.of_list
      |> Set.filter ~f:(fun item -> not (String.strip item |> String.is_empty))
      |> Set.union selected_options
    in
    let ui_events =
      if Set.equal maybe_changed_options selected_options
      then [ inject_select user_input ]
      else [ inject_select ""; on_set_change maybe_changed_options ]
    in
    Effect.Many (inject_selected_options maybe_changed_options :: ui_events)
  in
  input ~extra_attr ~value:select ~placeholder ~id ~on_input
;;

let create
  ?(extra_attr = Bonsai.return Vdom.Attr.empty)
  ?(placeholder = "")
  ?(on_set_change = Bonsai.return (const Effect.Ignore))
  ?(split = List.return)
  ()
  graph
  =
  let selected_options, inject_selected_options =
    Bonsai.state
      String.Set.empty
      ~sexp_of_model:[%sexp_of: String.Set.t]
      ~equal:[%equal: String.Set.t]
      graph
  in
  let id = Bonsai.path_id graph in
  let input =
    input
      ~placeholder
      ~extra_attr
      ~id
      ~on_set_change
      ~split
      ~selected_options
      ~inject_selected_options
      graph
  in
  let%arr selected_options = selected_options
  and inject_selected_options = inject_selected_options
  and input = input
  and on_set_change = on_set_change in
  let pills = pills ~selected_options ~on_set_change ~inject_selected_options in
  selected_options, Vdom.Node.div [ input; pills ], inject_selected_options
;;
