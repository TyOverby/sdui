open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
  .app {
    overflow-anchor:none;
  }

  .textbox {
    width:100% !important;
    padding: 0.3em 0.2em 0 0.2em !important;
  }

  .disabled {
    opacity:0.6;
  }

  .chosen {
    text-size: 0.8em;
    opacity: 0.8;
    font-weight: 200;
  }

  .button-box {
    margin:0.5em;
    gap:5px;
  }

  .button-box > * {
    cursor: pointer;
  }

  .choice-button {
  }

  .choice-button {
    background: var(--bg);
    color: var(--fg);
    border: 1px solid var(--border);
    border-radius: 3px;
    font-size: 0.8em;
    line-height: 1.3em;
    cursor: pointer;
    text-align: left;
  }

  .chosen,
  .character {
    line-height: 1.3em;
    text-align: justify;
    margin-left: auto;
    margin-right: auto;
    max-width: 1000px;
  }

  .character p {
  }

  .choices_container {
    max-width: 1500px;
    margin-left: auto;
    margin-right: auto;
    width: fit-content;
  }

  .choices_container > div {
    max-width: 300px;
  }

  .character-tag {
    position: relative;
    margin: 0;
    margin-top: 1em;
    font-size: 0.8em;
    color: #2196F3;
    font-weight: bold;
    text-transform: lowercase;
  }

  .character-tag + p {
    margin-top: 0;
  }

  p:has(+ .character-tag){
    margin-bottom: 0;
  }

|}]

module Kind = struct
  type t =
    | System
    | Splice
    | Prefix
    | Instruction
    | Response
    | Character of string
    | Choice
end

module Out = struct
  type text =
    { text : string
    ; tokens : int option
    ; mute : unit Effect.t
    }

  let _f { tokens; _ } =
    let _ = Kind.Response in
    tokens
  ;;

  type t =
    | System of string
    | Splice of string
    | Prefix of string
    | Instruction of text
    | Response of text
    | Response_continue of text
    | Muted
end

let generic_editable ~label =
  let%sub state, inject =
    Bonsai.state_machine0
      ~default_model:""
      ~apply_action:(fun _ctx model -> function
        | `Append new_text -> model ^ new_text
        | `Clear -> ""
        | `Set s -> s)
      ()
  in
  let%sub state = Bonsai.pure String.lstrip state in
  let%sub tokens =
    let%sub string_stable =
      Bonsai_extra.value_stability
        ~equal:[%equal: string]
        ~time_to_stable:(Time_ns.Span.of_sec 1.0)
        state
    in
    let%sub debounced =
      match%arr string_stable with
      | Unstable { previously_stable = Some previously_stable; _ } -> previously_stable
      | Unstable { previously_stable = None; _ } -> ""
      | Stable s -> s
    in
    match%sub debounced with
    | "" -> Bonsai.const None
    | debounced ->
      Bonsai.Edge.Poll.effect_on_change
        ~equal_input:[%equal: string]
        ~effect:
          (Value.return (fun s ->
             Prompt.make ~context:s ~history:[] `Continue |> Prompt.count_tokens))
        Bonsai.Edge.Poll.Starting.empty
        debounced
  in
  let%sub visible_state = Bonsai.state true in
  let%sub theme = View.Theme.current in
  let%arr theme = theme
  and state = state
  and tokens = tokens
  and inject = inject
  and enabled, set_enabled = visible_state in
  let tokens =
    match tokens with
    | Some (Ok tokens) -> Some tokens
    | None | Some (Error _) -> None
  in
  let view ?(extra_buttons = []) () =
    let textarea =
      Shared.Raw_textarea.textarea
        theme
        ~attrs:[ Style.textbox ]
        ~label:(Some label)
        ~value:state
        ~on_change:(fun s -> inject (`Set s))
        ~on_blur:Effect.Ignore
    in
    let visible_icon = if enabled then Feather_icon.Eye else Eye_off in
    View.vbox
      [ View.hbox
          ~attrs:[ (if not enabled then Style.disabled else Vdom.Attr.empty) ]
          [ textarea
          ; View.vbox
              ~attrs:[ Style.button_box ]
              (Feather_icon.svg
                 visible_icon
                 ~size:(`Px 20)
                 ~extra_attrs:[ Vdom.Attr.on_click (fun _ -> set_enabled (not enabled)) ]
               :: extra_buttons)
          ]
      ]
  in
  let state = { Out.text = state; tokens; mute = set_enabled false } in
  state, inject, view, enabled
;;

let mirror_to_localstorage ~unique_id getter setter =
  let store =
    Persistent_var.create
      (module struct
        type t = string option [@@deriving equal, sexp]
      end)
      `Local_storage
      ~default:None
      ~unique_id
  in
  let store_value, store_set = Persistent_var.value store, Persistent_var.effect store in
  let store_set = Value.return (fun s -> store_set (Some s)) in
  let getter = Value.map getter ~f:Option.some in
  Bonsai_extra.mirror'
    ~equal:String.equal
    ~store_set
    ~store_value
    ~interactive_set:setter
    ~interactive_value:getter
    ()
;;

let mirror_editable_to_local_storage ~unique_id edit_box =
  let%sub state, inject, _, _ = return edit_box in
  mirror_to_localstorage
    ~unique_id
    (state >>| fun { Out.text; _ } -> text)
    (inject >>| fun inject s -> inject (`Set s))
;;

let system =
  let%sub edit_box = generic_editable ~label:"system" in
  let%sub () = mirror_editable_to_local_storage ~unique_id:"system" edit_box in
  let%arr state, _inject, view, enabled = edit_box in
  let value = if not enabled then Out.Muted else System state.text in
  { Helpers.view = view (); value }
;;

let splice =
  let%sub edit_box = generic_editable ~label:"splice" in
  let%sub () = mirror_editable_to_local_storage ~unique_id:"splice" edit_box in
  let%arr state, _inject, view, enabled = edit_box in
  let value = if not enabled then Out.Muted else Splice state.text in
  { Helpers.view = view (); value }
;;

let prefix =
  let%sub edit_box = generic_editable ~label:"prefix" in
  let%sub () = mirror_editable_to_local_storage ~unique_id:"prefix" edit_box in
  let%arr state, _inject, view, enabled = edit_box in
  let value = if not enabled then Out.Muted else Prefix state.text in
  { Helpers.view = view (); value }
;;

let instruction =
  let%sub edit_box = generic_editable ~label:"instruction" in
  let%sub () = mirror_editable_to_local_storage ~unique_id:"instruction" edit_box in
  let%arr state, _inject, view, enabled = edit_box in
  let value = if not enabled then Out.Muted else Instruction state in
  { Helpers.view = view (); value }
;;

let rec dispatch_response
  ?length_penalty
  ?early_stopping
  ?max_new_tokens
  ?(system_substitutions = [])
  ~recent_history
  ~get_all
  ~my_path
  ~inject
  ~inject_finished
  kind
  =
  let%bind.Effect all_results = get_all
  and my_path = my_path in
  let up_to_me =
    Map.subrange all_results ~lower_bound:Unbounded ~upper_bound:(Excl my_path)
  in
  let system, next_to_mute, history =
    Map.fold
      up_to_me
      ~init:("", None, [])
      ~f:(fun ~key:_ ~data (system, next_to_mute, history) ->
        match (data : Out.t) with
        | System s -> s, next_to_mute, history
        | Instruction s ->
          let next_to_mute = Option.first_some next_to_mute (Some s.mute) in
          system, next_to_mute, (`Instruction, s.text) :: history
        | Response s ->
          let next_to_mute = Option.first_some next_to_mute (Some s.mute) in
          system, next_to_mute, (`Response, s.text) :: history
        | Response_continue s ->
          let next_to_mute = Option.first_some next_to_mute (Some s.mute) in
          system, next_to_mute, (`Continue, s.text) :: history
        | Splice _ | Prefix _ | Muted -> system, next_to_mute, history)
  in
  let system =
    system_substitutions
    |> List.fold ~init:system ~f:(fun acc (k, v) ->
      String.Search_pattern.(replace_all (create ("{{" ^ k ^ "}}")) ~in_:acc ~with_:v))
  in
  let history = List.rev history @ recent_history in
  let query =
    Prompt.make
      ?length_penalty
      ?early_stopping
      ?max_new_tokens
      ~context:system
      ~history
      kind
  in
  match%bind.Effect Prompt.count_tokens query with
  | Ok tokens ->
    if tokens < 3500 - 256
    then (
      match%bind.Effect Prompt.send ~on_response:(fun s -> inject (`Append s)) query with
      | Ok () -> inject_finished
      | Error e -> inject (`Append (Error.to_string_hum e)))
    else (
      match next_to_mute with
      | None -> inject (`Append "ERROR: couldn't find something to mute")
      | Some mute ->
        let%bind.Effect () = mute in
        dispatch_response
          ?length_penalty
          ?early_stopping
          ?max_new_tokens
          ~get_all
          ~my_path:(Effect.return my_path)
          ~recent_history
          ~inject
          ~inject_finished
          `Instruction)
  | Error e -> inject (`Append (Error.to_string_hum e))
;;

let regenerate_button ~inject ~generate =
  Feather_icon.svg
    Rotate_cw
    ~size:(`Px 20)
    ~extra_attrs:[ Vdom.Attr.on_click (fun _ -> Effect.Many [ inject `Clear; generate ]) ]
;;

let response ~get_all ~my_path =
  let%sub edit_box = generic_editable ~label:"response" in
  let%arr state, inject, view, enabled = edit_box
  and get_all = get_all
  and my_path = my_path in
  let view =
    let generate =
      dispatch_response
        ~get_all
        ~my_path
        ~inject
        ~inject_finished:Effect.Ignore
        ~recent_history:[]
        `Instruction
    in
    let extra_buttons = [ regenerate_button ~inject ~generate ] in
    view ~extra_buttons ()
  in
  let value = if not enabled then Out.Muted else Response state in
  { Helpers.view; value }
;;

let numeric_remove = Re.Pcre.re "[0-9]+\\." |> Re.compile

let strip_line line =
  line
  |> String.strip
  |> String.chop_prefix_if_exists ~prefix:"- "
  |> String.chop_suffix_if_exists ~suffix:"."
  |> String.chop_suffix_if_exists ~suffix:","
  |> Re.replace_string numeric_remove ~by:""
;;

let choice ~get_all ~my_path ~choice_picked =
  let%sub edit_box = generic_editable ~label:"choice" in
  let%sub _done_with_choices, set_done_with_choices = Bonsai.state false in
  let%sub dispatch =
    let%arr _, inject, _, _ = edit_box
    and get_all = get_all
    and my_path = my_path
    and set_done_with_choices = set_done_with_choices in
    let%bind.Effect all = get_all in
    let splice_text =
      Map.data all
      |> List.find_map ~f:(function
        | Out.Splice text -> Some text
        | _ -> None)
      |> Option.value ~default:""
    in
    let all =
      let first_response = ref true in
      let system_prompt = ref "" in
      Map.filter_map all ~f:(function
        | System _ -> Some (Out.System splice_text)
        | Instruction text ->
          system_prompt := text.text;
          None
        | Response text | Response_continue text ->
          if !first_response
          then (
            first_response := false;
            let text = { text with text = !system_prompt ^ "\n\n" ^ text.text } in
            Some (Instruction text))
          else Some (Response_continue text)
        | _ -> None)
    in
    dispatch_response
      ~length_penalty:1.5
      ~early_stopping:true
      ~max_new_tokens:100
      ~get_all:(Effect.return all)
      ~my_path
      ~inject
      ~inject_finished:(set_done_with_choices true)
      ~recent_history:
        [ `Instruction, splice_text
          (*; `Response, "here are some actions that characters could take:\n -"*)
        ]
      `Response
  in
  let%sub theme = View.Theme.current in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate:dispatch () in
  let%sub selected, set_selected = Bonsai.state_opt () in
  match%sub selected with
  | Some text ->
    let%sub muted, set_muted = Bonsai.state false in
    let%arr text = text
    and muted = muted
    and set_muted = set_muted in
    let view =
      Vdom.Node.p
        ~attrs:[ Style.chosen; (if muted then Style.disabled else Vdom.Attr.empty) ]
        [ Vdom.Node.text text ]
    in
    let value =
      if not muted
      then Out.Instruction { text; tokens = None; mute = set_muted true }
      else Out.Muted
    in
    { Helpers.view; value }
  | None ->
    let%arr state, inject, _view, _enabled = edit_box
    and theme = theme
    and dispatch = dispatch
    and set_selected = set_selected
    and choice_picked = choice_picked in
    let view =
      let options =
        let groups =
          state.text
          |> String.split_lines
          |> List.folding_map ~init:None ~f:(fun current_char line ->
            let line = strip_line line in
            let out =
              let%bind.Option character, action =
                match String.lsplit2 line ~on:':', current_char with
                | Some (character, action), _ -> Some (Some character, strip_line action)
                | None, Some character -> Some (Some character, line)
                | None, None -> Some (None, line)
              in
              let character, action =
                Option.map ~f:String.strip character, String.strip action
              in
              match action with
              | "" -> Some (character, None)
              | action ->
                let view =
                  Vdom.Node.button
                    ~attrs:
                      [ Style.choice_button
                      ; Vdom.Attr.on_click (fun _ ->
                          Effect.Many
                            [ set_selected
                                (Some
                                   (match character with
                                    | Some character -> character ^ " " ^ action
                                    | None -> action))
                            ; choice_picked character
                            ])
                      ; Style.Variables.set_all
                          ~fg:
                            ((View.extreme_colors theme).foreground
                             |> Css_gen.Color.to_string_css)
                          ~bg:
                            ((View.extreme_colors theme).background
                             |> Css_gen.Color.to_string_css)
                          ~border:
                            (View.extreme_primary_border_color theme
                             |> Css_gen.Color.to_string_css)
                      ]
                    [ Vdom.Node.text action ]
                in
                Some (character, Some view)
            in
            match out with
            | None -> current_char, None
            | Some (Some char, _) -> Some char, out
            | Some (None, _) -> current_char, out)
          |> List.filter_opt
          |> Helpers.alist_group ~equal:[%equal: string option]
          |> List.filter ~f:(function k, _ ->
            (match Option.map ~f:String.lowercase k with
             | Some ("all" | "both" | "note") -> false
             | Some s
               when String.Caseless.substr_index s ~pattern:"both" |> Option.is_some ->
               false
             | None -> true
             | _ -> true))
        in
        groups
        |> List.map ~f:(function person, group ->
          let person = Option.value person ~default:"" in
          let rows = Vdom.Node.span [ Vdom.Node.text person ] :: List.filter_opt group in
          View.vbox rows ~cross_axis_alignment:Start ~gap:(`Em_float 0.5))
        |> List.append [ regenerate_button ~inject ~generate:dispatch ]
        |> View.hbox ~attrs:[ Style.choices_container ] ~gap:(`Em_float 0.5)
      in
      options
    in
    { Helpers.view; value = Out.Muted }
;;

let character character ~get_all ~my_path ~character_finished =
  let%sub edit_box = generic_editable ~label:"character" in
  let%sub dispatch =
    let%arr _, inject, _, _ = edit_box
    and get_all = get_all
    and my_path = my_path
    and character = character
    and character_finished = character_finished in
    let%bind.Effect all = get_all in
    let prefix_text =
      Map.data all
      |> List.find_map ~f:(function
        | Out.Prefix "" -> Some (character ^ ":")
        | Out.Prefix text -> Some text
        | _ -> None)
      |> Option.value ~default:(character ^ ":")
    in
    let%bind.Effect () = inject (`Append (prefix_text ^ "\n")) in
    dispatch_response
      ~system_substitutions:[ "char", character ]
      ~length_penalty:1.2
      ~early_stopping:true
      ~max_new_tokens:256
      ~get_all:(Effect.return all)
      ~my_path
      ~inject
      ~inject_finished:character_finished
      ~recent_history:[ `Response, prefix_text ]
      `Continue
  in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate:dispatch () in
  let%arr state, _inject, _view, enabled = edit_box in
  let view =
    state.text
    |> String.split_lines
    |> List.filter_map ~f:(fun text ->
      if String.length text = 0
      then None
      else
        Some
          (if String.length text < 20 && String.is_suffix text ~suffix:":"
           then
             Vdom.Node.p
               ~attrs:[ Style.character_tag ]
               [ Vdom.Node.text (String.chop_suffix_if_exists ~suffix:":" text) ]
           else Vdom.Node.p [ Vdom.Node.text text ]))
    |> Vdom.Node.section
         ~attrs:
           [ Style.character; (if not enabled then Style.disabled else Vdom.Attr.empty) ]
  in
  let state = { state with text = String.strip state.text } in
  { Helpers.view; value = Out.Response state }
;;

let component kind ~get_all ~my_path ~character_finished ~choice_picked =
  let%sub r =
    match%sub kind with
    | Kind.System -> system
    | Splice -> splice
    | Prefix -> prefix
    | Instruction -> instruction
    | Response -> response ~get_all ~my_path
    | Character c -> character c ~get_all ~my_path ~character_finished
    | Choice -> choice ~get_all ~my_path ~choice_picked
  in
  let%arr r = r in
  r, Helpers.Path.Map.empty
;;

let component =
  let%sub chats, inject =
    let add_one ctx kind =
      `Append kind
      |> Bonsai.Apply_action_context.inject ctx
      |> Bonsai.Apply_action_context.schedule_event ctx
    in
    Bonsai.state_machine0
      ~default_model:
        (Bignum.Map.of_alist_exn
           [ Bignum.of_int 0, Kind.System
           ; Bignum.of_int 1, Splice
           ; Bignum.of_int 2, Prefix
           ; Bignum.of_int 3, Instruction
           ])
      ~apply_action:(fun ctx model -> function
        | `Append kind ->
          let key =
            match Map.max_elt model with
            | None -> Bignum.of_int 0
            | Some (k, _) -> Bignum.(k + one)
          in
          Map.set model ~key ~data:kind
        | `Finished ->
          add_one ctx Choice;
          model
        | `Chose name ->
          add_one ctx (Character name);
          model)
      ()
  in
  let%sub character_finished =
    let%arr inject = inject in
    inject `Finished
  in
  let%sub choice_picked =
    let%arr inject = inject in
    function
    | Some name -> inject (`Chose name)
    | None -> inject (`Chose "")
  in
  let%sub components =
    Helpers.tree (fun ~get_all ->
      Helpers.multi_merge
        (module Bignum)
        chats
        ~f:(fun ~key:_ ~my_path ~data ->
          component data ~get_all ~my_path ~character_finished ~choice_picked))
  in
  let%sub views =
    Bonsai.assoc
      (module Bignum)
      components
      ~f:(fun _ data ->
        let%sub { view; _ } = return data in
        return view)
  in
  let%arr views = views
  and inject = inject in
  Vdom.Node.div
    ~attrs:[ Style.app ]
    [ Vdom_node_with_map_children.make ~tag:"div" views
    ; Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              Effect.Many [ inject (`Append (Kind.Character "Lana")) ])
          ]
        [ Vdom.Node.text "foo" ]
    ]
;;
