open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Extendy = Bonsai_web_ui_extendy
module Selectable_style = Vdom_input_widgets.Selectable_style
module Form = Form_manual

module type Model = sig
  type t [@@deriving sexp_of]
end

module type Stringable_model = sig
  type t

  include Model with type t := t
  include Stringable with type t := t
end

let form_expert_create ~value ~view ~set = { Form.value; view; set }

let sexp_to_pretty_string sexp_of_t t =
  t
  |> sexp_of_t
  |> Sexp.to_string_mach
  |> String.lowercase
  |> String.map ~f:(function
    | '(' | ')' | '-' | '_' -> ' '
    | o -> o)
;;

module Style =
  [%css
  stylesheet
    {|
      .invalid_text_box {
        outline: none;
        border: 2px solid red;
        border-radius: 2px;
      }
      |}]

module Basic_stateful = struct
  let make state ~view graph =
    let state, set_state = state graph in
    let%arr view = view
    and state = state
    and set_state = set_state in
    let view = view ~state ~set_state in
    form_expert_create ~value:(Ok state) ~view ~set:set_state
  ;;

  let make_themed state ~view graph =
    let state, set_state = state graph in
    let theme = View.Theme.current graph in
    let%arr view = view
    and theme = theme
    and state = state
    and set_state = set_state in
    let view = view ~state ~set_state ~theme in
    form_expert_create ~value:(Ok state) ~view ~set:set_state
  ;;
end

let optional_to_required =
  Form.project'
    ~parse:(function
      | Some a -> Ok a
      | None -> Error (Error.of_string "a value is required"))
    ~unparse:(fun a -> Some a)
;;

module Non_interactive = struct
  let constant view value _graph =
    let%arr view = view
    and value = value in
    let view = view in
    form_expert_create ~view ~value ~set:(fun _ -> Effect.Ignore)
  ;;
end

let string_underlying
  ~(f :
      View.Theme.t
      -> ?attrs:Vdom.Attr.t list
      -> ?placeholder:string
      -> ?key:string
      -> ?allow_updates_when_focused:[ `Always | `Never ]
      -> disabled:bool
      -> value:string
      -> set_value:(string -> unit Ui_effect.t)
      -> unit
      -> Vdom.Node.t)
  ?(allow_updates_when_focused = `Always)
  ?(extra_attrs = Bonsai.return [])
  ?placeholder
  ()
  graph
  =
  let view =
    let path = Bonsai.path_id graph in
    let%arr extra_attrs = extra_attrs
    and path = path
    and placeholder = Bonsai.transpose_opt placeholder in
    fun ~state ~set_state ~theme ->
      f
        theme
        ?placeholder
        ~key:path
        ~allow_updates_when_focused
        ~disabled:false
        ~attrs:extra_attrs
        ~value:state
        ~set_value:set_state
        ()
  in
  Basic_stateful.make_themed
    (Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t])
    ~view
    graph
;;

module Textbox = struct
  let string ?extra_attrs ?placeholder ?(allow_updates_when_focused = `Always) () =
    string_underlying
      ~f:View.Form_inputs.textbox
      ?extra_attrs
      ?placeholder
      ~allow_updates_when_focused
      ()
  ;;

  let int ?extra_attrs ?placeholder ?(allow_updates_when_focused = `Always) here graph =
    let parse input =
      match Int.of_string input with
      | exception _ -> Or_error.error_string "Expected an integer"
      | x -> Or_error.return x
    in
    let unparse = Int.to_string_hum in
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused here graph
    in
    Form.project' form ~parse ~unparse
  ;;

  let float ?extra_attrs ?placeholder ?(allow_updates_when_focused = `Always) () graph =
    let parse input =
      match Float.of_string input with
      | exception _ -> Or_error.error_string "Expected a floating point number"
      | x -> Or_error.return x
    in
    let unparse = Float.to_string_hum in
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project' form ~parse ~unparse
  ;;

  let sexpable
    (type t)
    ?extra_attrs
    ?placeholder
    ?(allow_updates_when_focused = `Always)
    (module M : Sexpable with type t = t)
    graph
    =
    let parse_exn s = s |> Sexp.of_string |> M.t_of_sexp in
    let unparse t = t |> M.sexp_of_t |> Sexp.to_string_hum in
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn ~unparse
  ;;

  let stringable
    (type t)
    ?extra_attrs
    ?placeholder
    ?(allow_updates_when_focused = `Always)
    (module M : Stringable with type t = t)
    graph
    =
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn:M.of_string ~unparse:M.to_string
  ;;
end

module Password = struct
  let string ?extra_attrs ?placeholder ?(allow_updates_when_focused = `Always) () =
    string_underlying
      ~f:View.Form_inputs.password
      ?extra_attrs
      ?placeholder
      ~allow_updates_when_focused
      ()
  ;;

  let stringable
    (type t)
    ?extra_attrs
    ?placeholder
    ?(allow_updates_when_focused = `Always)
    (module M : Stringable with type t = t)
    graph
    =
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn:M.of_string ~unparse:M.to_string
  ;;
end

module Textarea = struct
  let string
    ?(extra_attrs = Bonsai.return [])
    ?placeholder
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let view =
      let%arr extra_attrs = extra_attrs
      and placeholder = Bonsai.transpose_opt placeholder in
      fun ~state ~set_state ~theme ->
        View.Form_inputs.textarea
          theme
          ?placeholder
          ~allow_updates_when_focused
          ~disabled:false
          ~attrs:extra_attrs
          ~value:state
          ~set_value:set_state
          ()
    in
    Basic_stateful.make_themed
      (Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t])
      ~view
      graph
  ;;

  let int ?extra_attrs ?placeholder ?(allow_updates_when_focused = `Always) () graph =
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string_hum
  ;;

  let float ?extra_attrs ?placeholder ?(allow_updates_when_focused = `Always) () graph =
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn:Float.of_string ~unparse:Float.to_string_hum
  ;;

  let sexpable
    (type t)
    ?extra_attrs
    ?placeholder
    ?(allow_updates_when_focused = `Always)
    (module M : Sexpable with type t = t)
    graph
    =
    let parse_exn s = s |> Sexp.of_string |> M.t_of_sexp in
    let unparse t = t |> M.sexp_of_t |> Sexp.to_string_hum in
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn ~unparse
  ;;

  let stringable
    (type t)
    ?extra_attrs
    ?placeholder
    ?(allow_updates_when_focused = `Always)
    (module M : Stringable with type t = t)
    graph
    =
    let%map.Bonsai form =
      string ?extra_attrs ?placeholder ~allow_updates_when_focused () graph
    in
    Form.project form ~parse_exn:M.of_string ~unparse:M.to_string
  ;;
end

module Checkbox = struct
  let make_input ?key ~extra_attrs ~state ~set_state () =
    Vdom.Node.input
      ?key
      ~attrs:
        [ Vdom.Attr.many
            ([ Vdom.Attr.style (Css_gen.margin_left (`Px 0))
             ; Vdom.Attr.type_ "checkbox"
             ; Vdom.Attr.on_click (fun evt ->
                 (* try to get the actual state of the checkbox, but if
                    that doesn't work, assume that clicking on the
                    element toggled the state. *)
                 let checked =
                   let open Option.Let_syntax in
                   let open Js_of_ocaml in
                   let%bind target = evt##.target |> Js.Opt.to_option in
                   let%bind coerced =
                     Dom_html.CoerceTo.input target |> Js.Opt.to_option
                   in
                   return (Js.to_bool coerced##.checked)
                 in
                 match checked with
                 | Some bool -> set_state bool
                 | None -> set_state (not state))
             ; Vdom.Attr.bool_property "checked" state
             ]
             @ extra_attrs)
        ]
      ()
  ;;

  let checkbox ?(extra_attrs = Bonsai.return []) default_model graph =
    let view =
      let path = Bonsai.path_id graph in
      let%arr extra_attrs = extra_attrs
      and path = path in
      fun ~state ~set_state -> make_input ~key:path ~extra_attrs ~state ~set_state ()
    in
    Basic_stateful.make
      (Bonsai.state
         default_model
         ~sexp_of_model:[%sexp_of: Bool.t]
         ~equal:[%equal: Bool.t])
      ~view
      graph
  ;;

  let bool ?extra_attrs ~default () = checkbox ?extra_attrs default

  let set
    (type a cmp)
    ?(style = Bonsai.return Selectable_style.Native)
    ?(extra_container_attrs = Bonsai.return [])
    ?(extra_checkbox_attrs = Bonsai.return (fun ~checked:_ -> []))
    ?to_string
    ?(layout = `Vertical)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    values
    : Bonsai.graph -> ((a, cmp) Set.t, Vdom.Node.t) Form.t Bonsai.t
    =
    fun graph ->
    let to_string =
      Option.value to_string ~default:(sexp_to_pretty_string [%sexp_of: M.t])
    in
    let module M = struct
      include M
      include Comparable.Make_plain_using_comparator (M)

      let to_string = to_string
    end
    in
    let view =
      let%map values = values
      and style = style
      and extra_container_attrs = extra_container_attrs
      and extra_checkbox_attrs = extra_checkbox_attrs in
      fun ~state ~set_state ->
        Vdom_input_widgets.Checklist.of_values
          ~layout
          ~extra_container_attrs
          ~extra_checkbox_attrs
          (module M)
          values
          ~style
          ~is_checked:(Set.mem state)
          ~on_toggle:(fun item ->
            set_state
            @@ if Set.mem state item then Set.remove state item else Set.add state item)
    in
    Basic_stateful.make
      (Bonsai.state
         M.Set.empty
         ~sexp_of_model:[%sexp_of: M.Set.t]
         ~equal:[%equal: M.Set.t])
      ~view
      graph
  ;;

  module Private = struct
    let make_input = make_input
  end
end

module Toggle = struct
  (* Most of this css is from https://www.w3schools.com/howto/howto_css_switch.asp *)

  module Style =
    [%css
    stylesheet
      {|
        .toggle {
          position: relative;
          display: inline-block;
          width: 40px;
          height: 22px;
        }

        .invisible {
          opacity: 0;
          width: 0;
          height: 0;
        }

        .slider {
          position: absolute;
          cursor: pointer;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background-color: #ccc; /* change */
          transition: 0.2s;
          border-radius: 34px;
        }

        .slider::before {
          position: absolute;
          content: "";
          height: 18px;
          width: 18px;
          left: 2px;
          bottom: 2px;
          border-radius: 50%;
          background-color: white;
          transition: 0.2s;
        }

        .invisible:checked + .slider {
          background-color: #2196f3;
        }

        .invisible:focused + .slider {
          box-shadow: 0 0 1px #2196f3;
        }

        .invisible:checked + .slider::before {
          transform: translateX(18px);
        }
        |}]

  let bool ?(extra_attr = Bonsai.return Vdom.Attr.empty) ~default () graph =
    let view =
      let path = Bonsai.path_id graph in
      let%arr extra_attr = extra_attr
      and path = path in
      fun ~state ~set_state ->
        let checkbox =
          Checkbox.make_input
            ~key:path
            ~extra_attrs:[ Vdom.Attr.many [ Style.invisible; extra_attr ] ]
            ~state
            ~set_state
            ()
        in
        let slider = Vdom.Node.span ~attrs:[ Style.slider ] [] in
        Vdom.Node.label ~attrs:[ Style.toggle ] [ checkbox; slider ]
    in
    Basic_stateful.make
      (Bonsai.state default ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t])
      ~view
      graph
  ;;
end

module Dropdown = struct
  module Opt = struct
    type 'a t =
      | Uninitialized
      | Explicitly_none
      | Set of 'a
    [@@deriving equal, sexp]

    let to_option = function
      | Uninitialized | Explicitly_none -> None
      | Set element -> Some element
    ;;

    let value ~default = function
      | Uninitialized | Explicitly_none -> default
      | Set element -> element
    ;;
  end

  let make_input
    (type a)
    ?to_string
    ?placeholder
    ?key
    (module E : Model with type t = a)
    ~equal
    ~include_empty
    ~default_value
    ~(state : a Opt.t)
    ~(set_state : a Opt.t -> unit Effect.t)
    ~extra_attrs
    ~extra_option_attrs
    ~all
    =
    let module E = struct
      include E

      let to_string =
        Option.value to_string ~default:(sexp_to_pretty_string [%sexp_of: t])
      ;;

      let equal = equal
    end
    in
    let maker ~extra_attrs options =
      match include_empty, default_value with
      | true, _ | false, None ->
        let selected =
          match state with
          | Uninitialized -> default_value
          | Explicitly_none -> None
          | Set v -> Some v
        in
        Vdom_input_widgets.Dropdown.of_values_opt
          ?key
          ?placeholder
          ~selected
          ~on_change:(function
            | None -> set_state Explicitly_none
            | Some v -> set_state (Set v))
          ~extra_attrs
          ~extra_option_attrs
          options
      | false, Some default ->
        Vdom_input_widgets.Dropdown.of_values
          ?key
          ~selected:(Opt.value state ~default)
          ~on_change:(fun a -> set_state (Set a))
          ~extra_attrs
          ~extra_option_attrs
          options
    in
    maker (module E) all ~extra_attrs
  ;;

  let impl
    (type t)
    ?to_string
    ?placeholder
    ?(extra_attrs = Bonsai.return [])
    ?(extra_option_attrs = Bonsai.return (Fn.const []))
    (module E : Model with type t = t)
    ~equal
    all
    ~include_empty
    ~init
    graph
    =
    let module E = struct
      include E

      let equal = equal
    end
    in
    let module E_opt = struct
      type t = E.t Opt.t [@@deriving sexp_of, equal]
    end
    in
    let default_value =
      match init with
      | `Empty -> Bonsai.return None
      | `First_item -> all >>| List.hd
      | `This item -> item >>| Option.some
      | `Const item -> Bonsai.return (Some item)
    in
    let state, set_state =
      Bonsai.state
        Uninitialized
        ~sexp_of_model:[%sexp_of: E_opt.t]
        ~equal:[%equal: E_opt.t]
        graph
    in
    let path = Bonsai.path_id graph in
    let%arr state = state
    and set_state = set_state
    and all = all
    and extra_attrs = extra_attrs
    and extra_option_attrs = extra_option_attrs
    and default_value = default_value
    and path = path in
    let view =
      make_input
        ?to_string
        ?placeholder
        (module E)
        ~equal
        ~include_empty
        ~default_value
        ~state
        ~set_state
        ~extra_attrs
        ~extra_option_attrs
        ~all
        ~key:path
    in
    let value =
      match state, default_value with
      | Uninitialized, Some default_value -> Some default_value
      | _ -> Opt.to_option state
    in
    form_expert_create ~value:(Ok value) ~view ~set:(function
      | None -> set_state Explicitly_none
      | Some v -> set_state (Set v))
  ;;

  let list_opt
    (type t)
    ?(init = `Empty)
    ?extra_attrs
    ?extra_option_attrs
    ?to_string
    ?placeholder
    (module E : Model with type t = t)
    ~equal
    all
    =
    impl
      ?to_string
      ?placeholder
      ?extra_attrs
      ?extra_option_attrs
      (module E)
      ~equal
      all
      ~include_empty:true
      ~init
  ;;

  let enumerable_opt
    (type t)
    ?(init = `Empty)
    ?extra_attrs
    ?extra_option_attrs
    ?to_string
    (module E : Bonsai.Enum with type t = t)
    =
    impl
      ?extra_attrs
      ?extra_option_attrs
      ?to_string
      (module E)
      ~equal:E.equal
      (Bonsai.return E.all)
      ~include_empty:true
      ~init
  ;;

  let include_empty_from_init = function
    | `Empty -> true
    | `First_item | `This _ | `Const -> false
  ;;

  let list
    ?(init = `First_item)
    ?extra_attrs
    ?extra_option_attrs
    ?to_string
    m
    ~equal
    all
    graph
    =
    let%map.Bonsai form =
      impl
        ?to_string
        ?extra_attrs
        ?extra_option_attrs
        m
        ~equal
        all
        ~init
        ~include_empty:(include_empty_from_init init)
        graph
    in
    optional_to_required form
  ;;

  let enumerable
    (type t)
    ?(init = `First_item)
    ?extra_attrs
    ?extra_option_attrs
    ?to_string
    (module E : Bonsai.Enum with type t = t)
    graph
    =
    let%map.Bonsai form =
      impl
        ?extra_attrs
        ?extra_option_attrs
        ?to_string
        (module E)
        ~equal:E.equal
        (Bonsai.return E.all)
        ~init
        ~include_empty:(include_empty_from_init init)
        graph
    in
    optional_to_required form
  ;;

  module Private = struct
    module Opt = Opt

    let make_input = make_input
  end
end

module Typeahead = struct
  let single_opt
    (type a)
    ?(extra_attrs = Bonsai.return [])
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    (module M : Model with type t = a)
    ~equal
    ~all_options
    graph
    =
    let%sub { selected = value; view; set_selected = set; _ } =
      Bonsai_web_ui_typeahead.Typeahead.create
        ?placeholder
        ?to_string
        ?to_option_description
        ?handle_unknown_option
        (module M)
        ~equal
        ~all_options
        ~extra_attrs
        graph
    in
    let%arr value = value
    and view = view
    and set = set in
    form_expert_create ~value:(Ok value) ~view ~set
  ;;

  let single
    ?extra_attrs
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    m
    ~equal
    ~all_options
    graph
    =
    let%map.Bonsai form =
      single_opt
        ?extra_attrs
        ?placeholder
        ?to_string
        ?to_option_description
        ?handle_unknown_option
        m
        ~equal
        ~all_options
        graph
    in
    optional_to_required form
  ;;

  let set
    ?(extra_attrs = Bonsai.return [])
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    ?split
    m
    ~all_options
    graph
    =
    let%sub { selected = value; view; set_selected = set; _ } =
      Bonsai_web_ui_typeahead.Typeahead.create_multi
        ?placeholder
        ?to_string
        ?to_option_description
        ?handle_unknown_option
        ?split
        m
        ~extra_attrs
        ~all_options
        graph
    in
    let%arr value = value
    and view = view
    and set = set in
    form_expert_create ~value:(Ok value) ~view ~set
  ;;

  let list
    (type a cmp)
    ?extra_attrs
    ?placeholder
    ?to_string
    ?to_option_description
    ?handle_unknown_option
    ?split
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    ~all_options
    graph
    =
    let%map.Bonsai form =
      set
        ?extra_attrs
        ?placeholder
        ?to_string
        ?to_option_description
        ?handle_unknown_option
        ?split
        (module M)
        ~all_options
        graph
    in
    Form.project form ~parse_exn:Set.to_list ~unparse:(Set.of_list (module M))
  ;;
end

module Date_time = struct
  module Span_unit = struct
    type t =
      | Milliseconds
      | Seconds
      | Minutes
      | Hours
    [@@deriving equal, sexp, compare, enumerate]

    let to_string = function
      | Milliseconds -> "ms"
      | Seconds -> "s"
      | Minutes -> "m"
      | Hours -> "h"
    ;;

    let of_float = function
      | Milliseconds -> Time_ns.Span.of_ms
      | Seconds -> Time_ns.Span.of_sec
      | Minutes -> Time_ns.Span.of_min
      | Hours -> Time_ns.Span.of_hr
    ;;

    let to_float = function
      | Milliseconds -> Time_ns.Span.to_ms
      | Seconds -> Time_ns.Span.to_sec
      | Minutes -> Time_ns.Span.to_min
      | Hours -> Time_ns.Span.to_hr
    ;;
  end

  let date_opt
    ?(extra_attrs = Bonsai.return [])
    ?default
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~state ~set_state ->
        Vdom_input_widgets.Entry.date
          ~allow_updates_when_focused
          ~extra_attrs
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make
      (Bonsai.state_opt
         ?default_model:default
         ~sexp_of_model:[%sexp_of: Date.t]
         ~equal:[%equal: Date.t])
      ~view
      graph
  ;;

  let date ?extra_attrs ?default ?(allow_updates_when_focused = `Always) () graph =
    let%map.Bonsai form =
      date_opt ?extra_attrs ?default ~allow_updates_when_focused () graph
    in
    optional_to_required form
  ;;

  let time_opt
    ?(extra_attrs = Bonsai.return [])
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let view =
      let%arr extra_attrs = extra_attrs in
      fun ~state ~set_state ->
        Vdom_input_widgets.Entry.time
          ~allow_updates_when_focused
          ~extra_attrs
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make
      (Bonsai.state_opt
         ~sexp_of_model:[%sexp_of: Time_ns.Ofday.t]
         ~equal:[%equal: Time_ns.Ofday.t])
      ~view
      graph
  ;;

  let time ?extra_attrs ?(allow_updates_when_focused = `Always) () graph =
    let%map.Bonsai form = time_opt ?extra_attrs ~allow_updates_when_focused () graph in
    optional_to_required form
  ;;

  let time_span_opt
    ?(extra_unit_attrs = Bonsai.return [])
    ?(extra_amount_attrs = Bonsai.return [])
    ?(default_unit = Span_unit.Seconds)
    ?(default = None)
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let unit, set_unit =
      Bonsai.state
        default_unit
        ~sexp_of_model:[%sexp_of: Span_unit.t]
        ~equal:[%equal: Span_unit.t]
        graph
    in
    let unit_view =
      let%arr unit = unit
      and set_unit = set_unit
      and extra_unit_attrs = extra_unit_attrs in
      Dropdown.Private.make_input
        ~to_string:Span_unit.to_string
        (module Span_unit)
        ~equal:[%equal: Span_unit.t]
        ~include_empty:false
        ~default_value:(Some default_unit)
        ~state:(Set unit)
        ~set_state:(function
          | Uninitialized | Explicitly_none ->
            (* I think these cases can't happen, because both [include_empty:false] and
               [state:(Set unit)] are passed. *)
            Effect.Ignore
          | Set unit -> set_unit unit)
        ~extra_attrs:extra_unit_attrs
        ~extra_option_attrs:(Fn.const [])
        ~all:Span_unit.all
    in
    let amount, set_amount =
      let default_amount =
        let%map.Option default = default in
        Span_unit.to_float default_unit default
      in
      Bonsai.state
        default_amount
        ~sexp_of_model:[%sexp_of: Float.t option]
        ~equal:[%equal: Float.t option]
        graph
    in
    let amount_view =
      let%arr amount = amount
      and set_amount = set_amount
      and extra_amount_attrs = extra_amount_attrs in
      Vdom_input_widgets.Entry.number
        (module Vdom_input_widgets.Decimal)
        ~extra_attrs:extra_amount_attrs
        ~value:amount
        ~step:1.0
        ~on_input:set_amount
        ~allow_updates_when_focused
    in
    let value =
      let%arr amount = amount
      and unit = unit in
      Option.map amount ~f:(Span_unit.of_float unit)
    in
    let view =
      let%arr unit_view = unit_view
      and amount_view = amount_view in
      Vdom.Node.div [ amount_view; unit_view ]
    in
    let set =
      let%arr set_unit = set_unit
      and set_amount = set_amount in
      function
      | None -> Effect.Many [ set_unit default_unit; set_amount None ]
      | Some time_span ->
        let unit =
          if Time_ns.Span.( < ) time_span (Time_ns.Span.of_min 1.)
          then Span_unit.Seconds
          else if Time_ns.Span.( < ) time_span (Time_ns.Span.of_hr 1.)
          then Minutes
          else Hours
        in
        Effect.Many
          [ set_unit unit; set_amount (Some (Span_unit.to_float unit time_span)) ]
    in
    let%arr value = value
    and view = view
    and set = set in
    form_expert_create ~value:(Ok value) ~view ~set
  ;;

  let time_span
    ?extra_unit_attrs
    ?extra_amount_attrs
    ?default_unit
    ?default
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let%map.Bonsai form =
      time_span_opt
        ?extra_unit_attrs
        ?extra_amount_attrs
        ?default_unit
        ~default
        ~allow_updates_when_focused
        ()
        graph
    in
    optional_to_required form
  ;;

  let datetime_local_opt
    ?(extra_attrs = Bonsai.return [])
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~state ~set_state ->
        Vdom_input_widgets.Entry.datetime_local
          ~extra_attrs
          ~value:state
          ~on_input:set_state
          ~allow_updates_when_focused
          ()
    in
    let module Time_ns = struct
      include Time_ns.Stable.Alternate_sexp.V1

      let equal = Time_ns.equal
    end
    in
    Basic_stateful.make
      (Bonsai.state_opt ~sexp_of_model:[%sexp_of: Time_ns.t] ~equal:[%equal: Time_ns.t])
      ~view
      graph
  ;;

  let datetime_local ?extra_attrs ?(allow_updates_when_focused = `Always) () graph =
    let%map.Bonsai form =
      datetime_local_opt ?extra_attrs ~allow_updates_when_focused () graph
    in
    optional_to_required form
  ;;

  module Range = struct
    module Input_element = struct
      type 'a t =
        extra_attrs:Vdom.Attr.t list
        -> value:'a option
        -> on_input:('a option -> unit Effect.t)
        -> Vdom.Node.t
    end

    let make_opt_range
      (type a)
      ?(allow_equal = false)
      ?(extra_attr = Bonsai.return Vdom.Attr.empty)
      ~kind_name
      (module M : Model with type t = a)
      ~equal
      (module C : Comparisons.S with type t = a)
      (view : a Input_element.t)
      graph
      =
      let module M = struct
        include M

        let equal = equal
      end
      in
      let module M_opt = struct
        type t = M.t option [@@deriving sexp_of, equal]
      end
      in
      let bounds_error =
        lazy
          (Or_error.error_string
             (if allow_equal
              then
                [%string
                  "Start %{kind_name} must be before or the same as the end %{kind_name}."]
              else
                [%string
                  "Start %{kind_name} must be strictly before the end %{kind_name}."]))
      in
      let lower_id = Bonsai.path_id graph in
      let upper_id = Bonsai.path_id graph in
      let lower, set_lower =
        Bonsai.state
          None
          ~sexp_of_model:[%sexp_of: M_opt.t]
          ~equal:[%equal: M_opt.t]
          graph
      in
      let upper, set_upper =
        Bonsai.state
          None
          ~sexp_of_model:[%sexp_of: M_opt.t]
          ~equal:[%equal: M_opt.t]
          graph
      in
      let%arr lower = lower
      and set_lower = set_lower
      and upper = upper
      and set_upper = set_upper
      and lower_id = lower_id
      and upper_id = upper_id
      and extra_attr = extra_attr in
      let value =
        match lower, upper with
        | Some lower, Some upper ->
          (match C.compare lower upper with
           | 0 -> if allow_equal then Ok (Some lower, Some upper) else force bounds_error
           | x when x < 0 -> Ok (Some lower, Some upper)
           | x when x > 0 -> force bounds_error
           | _ -> assert false)
        | t -> Ok t
      in
      let view =
        let lower_view =
          view
            ~extra_attrs:[ Vdom.Attr.id lower_id; extra_attr ]
            ~value:lower
            ~on_input:set_lower
        in
        let upper_view =
          view
            ~extra_attrs:[ Vdom.Attr.id upper_id; extra_attr ]
            ~value:upper
            ~on_input:set_upper
        in
        Vdom.Node.div [ lower_view; Vdom.Node.text " - "; upper_view ]
      in
      let set (lower_val, upper_val) =
        let pairwise_set = Effect.Many [ set_lower lower_val; set_upper upper_val ] in
        match lower_val, upper_val with
        | None, _ | _, None -> pairwise_set
        | Some lower_val, Some upper_val ->
          (match C.compare lower_val upper_val with
           | 0 -> if allow_equal then pairwise_set else Effect.Ignore
           | x when x < 0 -> pairwise_set
           | x when x > 0 -> Effect.Ignore
           | _ -> Effect.Ignore)
      in
      form_expert_create ~view ~value ~set
    ;;

    let of_opt_range form =
      Form.project'
        form
        ~parse:(fun (lower, upper) ->
          match lower, upper with
          | Some lower, Some upper -> Ok (lower, upper)
          | None, None -> Error (Error.of_string "Values are required for this range")
          | None, Some _ ->
            Error (Error.of_string "A value is required for the start of this range")
          | Some _, None ->
            Error (Error.of_string "A value is required for the end of this range"))
        ~unparse:(fun (lower, upper) -> Some lower, Some upper)
    ;;

    let date_opt ?extra_attr ?allow_equal ?(allow_updates_when_focused = `Always) () =
      make_opt_range
        ~kind_name:"date"
        ?extra_attr
        ?allow_equal
        (module Date)
        ~equal:[%equal: Date.t]
        (module Date)
        (fun ~extra_attrs ->
          Vdom_input_widgets.Entry.date ~allow_updates_when_focused ~extra_attrs ())
    ;;

    let date ?extra_attr ?allow_equal ?(allow_updates_when_focused = `Always) () graph =
      let%map.Bonsai form =
        date_opt ?extra_attr ?allow_equal ~allow_updates_when_focused () graph
      in
      of_opt_range form
    ;;

    let time_opt ?extra_attr ?allow_equal ?(allow_updates_when_focused = `Always) () =
      make_opt_range
        ~kind_name:"time"
        ?extra_attr
        ?allow_equal
        (module Time_ns.Ofday)
        ~equal:[%equal: Time_ns.Ofday.t]
        (module Time_ns.Ofday)
        (fun ~extra_attrs ->
          Vdom_input_widgets.Entry.time ~allow_updates_when_focused ~extra_attrs ())
    ;;

    let time ?extra_attr ?allow_equal ?(allow_updates_when_focused = `Always) () graph =
      let%map.Bonsai form =
        time_opt ?extra_attr ?allow_equal ~allow_updates_when_focused () graph
      in
      of_opt_range form
    ;;

    let datetime_local_opt
      ?extra_attr
      ?allow_equal
      ?(allow_updates_when_focused = `Always)
      ()
      =
      make_opt_range
        ~kind_name:"time"
        ?extra_attr
        ?allow_equal
        (module Time_ns.Alternate_sexp)
        ~equal:[%equal: Time_ns.Alternate_sexp.t]
        (module Time_ns.Alternate_sexp)
        (fun ~extra_attrs ->
          Vdom_input_widgets.Entry.datetime_local
            ~allow_updates_when_focused
            ~extra_attrs
            ())
    ;;

    let datetime_local
      ?extra_attr
      ?allow_equal
      ?(allow_updates_when_focused = `Always)
      ()
      graph
      =
      let%map.Bonsai form =
        datetime_local_opt ?extra_attr ?allow_equal ~allow_updates_when_focused () graph
      in
      of_opt_range form
    ;;
  end
end

module Multiselect = struct
  let set
    (type a cmp)
    ?(extra_attrs = Bonsai.return [])
    ?to_string
    ?default_selection_status
    ?(allow_updates_when_focused = `Always)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    input_list
    graph
    =
    let module Item = struct
      include M
      include Comparable.Make_plain_using_comparator (M)

      let to_string =
        Option.value to_string ~default:(sexp_to_pretty_string [%sexp_of: t])
      ;;
    end
    in
    let module Single_factor = Bonsai_web_ui_multi_select.Make (Item) in
    let input_set = input_list >>| Item.Set.of_list in
    let extra_row_attrs ~is_focused =
      if is_focused
      then
        `RGBA (Css_gen.Color.RGBA.create () ~r:0 ~g:0 ~b:0 ~a:(Percent.of_mult 0.3))
        |> Css_gen.background_color
        |> Vdom.Attr.style
      else Vdom.Attr.empty
    in
    let path = Bonsai.path_id graph in
    let view_config =
      let%arr path = path in
      { Single_factor.View_config.header =
          Vdom.Node.none_deprecated [@alert "-deprecated"]
      ; extra_row_attrs = Some extra_row_attrs
      ; autofocus_search_box = false
      ; search_box_id = Some path
      ; allow_updates_when_focused
      }
    in
    let single_factor_result =
      Single_factor.bonsai ?default_selection_status ~view_config input_set graph
    in
    let%arr { Single_factor.Result.view; inject; selected_items; key_handler; _ } =
      single_factor_result
    and extra_attrs = extra_attrs in
    let set_state set =
      inject
        (Single_factor.Action.Set_all_selection_statuses
           (Map.of_key_set
              set
              ~f:(Fn.const Bonsai_web_ui_multi_select.Selection_status.Selected)))
    in
    let on_keydown =
      Vdom.Attr.on_keydown
        (Vdom_keyboard.Keyboard_event_handler.handle_or_ignore_event key_handler)
    in
    let view =
      Vdom.Node.div ~attrs:[ Vdom.Attr.many (on_keydown :: extra_attrs) ] [ view ]
    in
    form_expert_create ~value:(Ok selected_items) ~view ~set:set_state
  ;;

  let list
    (type a cmp)
    ?extra_attrs
    ?to_string
    ?default_selection_status
    ?(allow_updates_when_focused = `Always)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    input_list
    graph
    =
    let%map.Bonsai form =
      set
        ?extra_attrs
        ?to_string
        ?default_selection_status
        ~allow_updates_when_focused
        (module M)
        input_list
        graph
    in
    Form.project form ~parse_exn:Set.to_list ~unparse:(Set.of_list (module M))
  ;;
end

let list_rev_map2 a b ~f =
  let rec loop a b acc =
    match a, b with
    | x :: xs, y :: ys -> loop xs ys (f x y :: acc)
    | [], [] -> Ok acc
    | _ -> Error `Unequal_lengths
  in
  loop a b []
;;

module Multiple = struct
  let stringable_list
    (type a)
    ?(extra_input_attr = Bonsai.return Vdom.Attr.empty)
    ?(extra_pill_container_attr = Bonsai.return Vdom.Attr.empty)
    ?(extra_pill_attr = Bonsai.return Vdom.Attr.empty)
    ?(placeholder = Bonsai.return "")
    (module M : Stringable_model with type t = a)
    ~equal
    graph
    =
    let module M = struct
      include M

      let equal = equal
    end
    in
    let module M_list = struct
      type t = M.t list [@@deriving equal, sexp_of]
    end
    in
    let invalid, inject_invalid =
      Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t] graph
    in
    let selected_options, inject_selected_options =
      Bonsai.state [] ~sexp_of_model:[%sexp_of: M_list.t] ~equal:[%equal: M_list.t] graph
    in
    let state, set_state =
      Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t] graph
    in
    let pills =
      Bonsai_web_ui_common_components.Pills.of_list
        ~extra_container_attr:extra_pill_container_attr
        ~extra_pill_attr
        ~to_string:(Bonsai.return M.to_string)
        ~inject_selected_options
        selected_options
        graph
    in
    let%arr invalid = invalid
    and inject_invalid = inject_invalid
    and selected_options = selected_options
    and inject_selected_options = inject_selected_options
    and state = state
    and set_state = set_state
    and pills = pills
    and extra_input_attr = extra_input_attr
    and placeholder_ = placeholder in
    let handle_keydown event =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
      | Enter ->
        (match Option.try_with (fun () -> M.of_string state) with
         | None -> Effect.Many [ Effect.Prevent_default; inject_invalid true ]
         | Some value ->
           Effect.Many
             [ Effect.Prevent_default
             ; inject_selected_options (value :: selected_options)
             ; set_state ""
             ])
      | _ -> Effect.Ignore
    in
    let invalid_attr = if invalid then Style.invalid_text_box else Vdom.Attr.empty in
    let input =
      Vdom_input_widgets.Entry.text
        ~allow_updates_when_focused:`Always
        ~placeholder:placeholder_
        ~extra_attrs:
          [ Vdom.Attr.(extra_input_attr @ invalid_attr @ on_keydown handle_keydown) ]
        ~on_input:(fun input ->
          Effect.Many [ inject_invalid false; set_state (Option.value ~default:"" input) ])
        ~value:(Some state)
        ()
    in
    let view = Vdom.Node.div [ input; pills ] in
    form_expert_create ~value:(Ok selected_options) ~view ~set:inject_selected_options
  ;;

  type ('a, 'view) item =
    { form : ('a, 'view) Form.t
    ; remove : unit Effect.t
    }

  type ('a, 'view) t =
    { items : ('a, 'view) item list
    ; add_element : unit Effect.t
    }

  type ('a, 'view) nonempty_t =
    { hd : ('a, 'view) Form.t
    ; tl : ('a, 'view) item list
    ; add_element : unit Effect.t
    }

  module Seqnum_for_list = Bonsai_extra.Id_gen (Int63) ()

  let list (type a view) (t : Bonsai.graph -> (a, view) Form.t Bonsai.t)
    : Bonsai.graph -> (a list, (a, view) t) Form.t Bonsai.t
    =
    fun graph ->
    let%sub form, _, _, _, _, _ =
      Bonsai.wrap
        graph
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~default_model:()
        ~apply_action:
          (fun
            context
            (_, forms, set_length, bonk, get_next_seqnum, most_recent_seqnum)
            ()
            (list_of_values, my_seqnum)
          ->
          if not (Seqnum_for_list.equal my_seqnum most_recent_seqnum)
          then
            (* if the lists aren't the same length and the seqnums aren't the same, it's
               because another setter happened after this one, so we shouldn't do anything 
               here, and let the next setter do its thing. *)
            ()
          else (
            let setters_applied =
              list_rev_map2 (Map.data forms) list_of_values ~f:(fun form value ->
                Form.set form value)
            in
            match setters_applied with
            | Ok setters_applied ->
              Bonsai.Apply_action_context.schedule_event
                context
                (Ui_effect.Many setters_applied)
            | Error `Unequal_lengths ->
              (* If the lists aren't the same size, then another call to [set] modified the 
               length.  Because the seqnum for the action matches the current seqnum, we
               know we're the last in the sequence, so we can update the length _again_
               and try the whole transaction again. *)
              Bonsai.Apply_action_context.schedule_event
                context
                (let%bind.Effect new_seqnum = get_next_seqnum in
                 Ui_effect.Many
                   [ set_length (List.length list_of_values)
                   ; bonk
                       (Bonsai.Apply_action_context.inject
                          context
                          (list_of_values, new_seqnum))
                   ])))
        ~f:(fun (_ : unit Bonsai.t) inject_outer graph ->
          let extendy = Extendy.component t graph in
          let bonk = Bonsai_extra.bonk graph in
          let get_next_seqnum, most_recent_seqnum =
            Seqnum_for_list.component' ~reset:`Bump graph
          in
          let%arr { Extendy.contents; append; set_length; remove } = extendy
          and inject_outer = inject_outer
          and bonk = bonk
          and get_next_seqnum = get_next_seqnum
          and most_recent_seqnum = most_recent_seqnum in
          let view =
            let items =
              contents
              |> Map.to_alist
              |> List.map ~f:(fun (key, form) ->
                let remove = remove key in
                { form; remove })
            in
            let add_element = append in
            { items; add_element }
          in
          let value =
            contents |> Map.data |> List.map ~f:Form.value |> Or_error.combine_errors
          in
          let set (list : a list) =
            Effect.lazy_
              (lazy
                (let%bind.Effect my_seqnum = get_next_seqnum in
                 match Map.length contents = List.length list with
                 | false ->
                   Vdom.Effect.Many
                     [ set_length (List.length list)
                     ; bonk (inject_outer (list, my_seqnum))
                     ]
                 | true -> inject_outer (list, my_seqnum)))
          in
          ( form_expert_create ~value ~view ~set
          , contents
          , set_length
          , bonk
          , get_next_seqnum
          , most_recent_seqnum ))
    in
    form
  ;;

  let nonempty_list (type a view) (t : Bonsai.graph -> (a, view) Form.t Bonsai.t)
    : Bonsai.graph -> (a Nonempty_list.t, (a, view) nonempty_t) Form.t Bonsai.t
    =
    fun graph ->
    let hd = t graph in
    let tl = list t graph in
    let%arr hd = hd
    and tl = tl in
    Form.both hd tl
    |> Form.project
         ~parse_exn:(fun (hd, tl) -> Nonempty_list.create hd tl)
         ~unparse:(fun (hd :: tl) -> hd, tl)
    |> Form.map_view ~f:(fun (_, { items; add_element }) ->
      { hd; tl = items; add_element })
  ;;

  let set
    (type a cmp view)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    (form : Bonsai.graph -> (a, view) Form.t Bonsai.t)
    : Bonsai.graph -> ((a, cmp) Set.t, (a, view) t) Form.t Bonsai.t
    =
    fun graph ->
    let%map.Bonsai form = list form graph in
    Form.project form ~parse_exn:(Set.of_list (module M)) ~unparse:Set.to_list
  ;;

  let map
    (type a cmp)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    ~key
    ~data
    graph
    =
    let both graph =
      let key_form = key graph in
      let value_form = data graph in
      Bonsai.map2 key_form value_form ~f:Form.both
    in
    let%map.Bonsai form = list both graph in
    Form.project form ~parse_exn:(Map.of_alist_exn (module M)) ~unparse:Map.to_alist
  ;;
end

let validate_range ?min ?max value =
  let ( < ) = Float.( < ) in
  let ( > ) = Float.( > ) in
  let too_small =
    match min with
    | None -> Ok ()
    | Some min ->
      if value < min
      then
        Or_error.error_s
          [%message (value : float) "lower than allowed threshold" (min : float)]
      else Ok ()
  in
  let too_large =
    match max with
    | None -> Ok ()
    | Some max ->
      if value > max
      then
        Or_error.error_s
          [%message (value : float) "higher than allowed threshold" (max : float)]
      else Ok ()
  in
  Or_error.all_unit [ too_small; too_large ]
;;

module Number = struct
  let float
    ?(extra_attrs = Bonsai.return [])
    ?min
    ?max
    ?default
    ~step
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let optional_unvalidated =
      let view =
        let%arr extra_attrs = extra_attrs in
        fun ~state ~set_state ~theme ->
          View.Form_inputs.number
            theme
            ~attrs:extra_attrs
            ?min
            ?max
            ~disabled:false
            ~step
            ~allow_updates_when_focused
            ~value:state
            ~set_value:set_state
            ()
      in
      Basic_stateful.make_themed (Bonsai.state_opt ?default_model:default) ~view graph
    in
    let%arr optional_unvalidated = optional_unvalidated in
    Form.project'
      optional_unvalidated
      ~parse:(function
        | Some value -> Ok value
        | None -> Or_error.error_s [%message "value not specified"])
      ~unparse:Option.return
    |> Form.validate ~f:(validate_range ?min ?max)
  ;;

  let int
    ?extra_attrs
    ?min
    ?max
    ?default
    ~step
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let int x = Option.map x ~f:Int.to_float in
    let float =
      float
        ?extra_attrs
        ?min:(int min)
        ?max:(int max)
        ?default:(int default)
        ~step:(Int.to_float step)
        ~allow_updates_when_focused
        ()
        graph
    in
    let%arr float = float in
    Form.project float ~parse_exn:Int.of_float ~unparse:Int.to_float
  ;;
end

module Range = struct
  (* The default values of [min]/[max]/[default] match the browser spec. *)
  let float
    ?(extra_attrs = Bonsai.return [])
    ?(min = 0.)
    ?(max = 100.)
    ?left_label
    ?right_label
    ?(default = (min +. max) /. 2.)
    ~step
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let unvalidated =
      let view =
        let%arr extra_attrs = extra_attrs in
        fun ~state ~set_state ~theme ->
          let input =
            View.Form_inputs.range
              theme
              ~attrs:extra_attrs
              ~min
              ~max
              ~disabled:false
              ~allow_updates_when_focused
              ~step
              ~value:state
              ~set_value:set_state
              ()
          in
          match List.filter_opt [ left_label; Some input; right_label ] with
          | [ x ] -> x
          | elements ->
            Vdom.Node.span ~attrs:[ Vdom.Attr.style (Css_gen.flex_container ()) ] elements
      in
      Basic_stateful.make_themed (Bonsai.state default) ~view graph
    in
    let%arr unvalidated = unvalidated in
    Form.validate unvalidated ~f:(validate_range ~min ~max)
  ;;

  let int
    ?extra_attrs
    ?min
    ?max
    ?left_label
    ?right_label
    ?default
    ~step
    ?(allow_updates_when_focused = `Always)
    ()
    graph
    =
    let int x = Option.map x ~f:Int.to_float in
    let float =
      float
        ?extra_attrs
        ?min:(int min)
        ?max:(int max)
        ?left_label
        ?right_label
        ?default:(int default)
        ~allow_updates_when_focused
        ~step:(Int.to_float step)
        ()
        graph
    in
    let%arr float = float in
    Form.project float ~parse_exn:Int.of_float ~unparse:Int.to_float
  ;;
end

module Radio_buttons = struct
  let list
    (type t)
    ?(style = Bonsai.return Selectable_style.Native)
    ?(extra_container_attrs = Bonsai.return [])
    ?(extra_button_attrs = Bonsai.return (fun ~checked:_ -> []))
    ?init
    ?to_string
    (module E : Model with type t = t)
    ~equal
    ~layout
    all
    graph
    =
    let module E = struct
      include E

      let equal = equal

      let to_string (item : E.t) =
        match to_string with
        | Some f -> f item
        | None -> sexp_to_pretty_string E.sexp_of_t item
      ;;
    end
    in
    let path = Bonsai.path_id graph in
    let view =
      let%map all = all
      and style = style
      and extra_container_attrs = extra_container_attrs
      and extra_button_attrs = extra_button_attrs
      and path = path in
      fun ~state ~set_state ->
        let node_fun =
          match layout with
          | `Vertical -> Vdom_input_widgets.Radio_buttons.of_values
          | `Horizontal -> Vdom_input_widgets.Radio_buttons.of_values_horizontal
        in
        node_fun
          ~extra_container_attrs
          ~extra_button_attrs
          ~style
          (module E)
          ~on_click:(fun value -> set_state (Some value))
          ~selected:state
          ~name:path
          all
    in
    let%map.Bonsai form =
      Basic_stateful.make
        (Bonsai.state_opt
           ?default_model:init
           ~sexp_of_model:[%sexp_of: E.t]
           ~equal:E.equal)
        ~view
        graph
    in
    optional_to_required form
  ;;

  let enumerable
    (type t)
    ?style
    ?extra_container_attrs
    ?extra_button_attrs
    ?init
    ?to_string
    (module E : Bonsai.Enum with type t = t)
    ~layout
    =
    list
      ?style
      ?extra_container_attrs
      ?extra_button_attrs
      ?init
      ?to_string
      (module E)
      ~equal:E.equal
      ~layout
      (Bonsai.return E.all)
  ;;
end

module Color_picker = struct
  let hex ?(extra_attr = Bonsai.return Vdom.Attr.empty) () graph =
    let view =
      let%map extra_attr = extra_attr in
      fun ~state ~set_state ->
        Vdom_input_widgets.Entry.color_picker
          ~extra_attr
          ~value:state
          ~on_input:set_state
          ()
    in
    Basic_stateful.make
      (Bonsai.state
         (`Hex "#000000")
         ~sexp_of_model:[%sexp_of: [ `Hex of string ]]
         ~equal:[%equal: [ `Hex of string ]])
      ~view
      graph
  ;;
end

module File_select = struct
  module File = struct
    type t = Bonsai_web_ui_file.t [@@deriving sexp_of]

    let equal = phys_equal
  end

  (** For security reasons, the browser restricts what values can be set into an input
      with type "file": you are only allowed to clear these inputs. Therefore, the
      following functions print a warning message and do nothing if anything other than
      clearing is attempted with [Form.set]. *)

  let single_opt ?(extra_attrs = Bonsai.return []) ?accept () graph =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~state ~set_state ->
        (* [value_prop] is a trick that enforces the browser input is cleared when an
           empty value is set into the form: if we ever change from a [Some] to a [None],
           then we also set value to "", which clears the form. Otherwise, we let the form
           be uncontrolled while a file is selected. *)
        let value_prop =
          if Option.is_none state then Vdom.Attr.value_prop "" else Vdom.Attr.empty
        in
        Vdom_input_widgets.File_select.single
          ?accept
          ~extra_attrs:(extra_attrs @ [ value_prop ])
          ~on_input:(fun file ->
            set_state (Option.map file ~f:Bonsai_web_ui_file_from_web_file.create))
          ()
    in
    let form =
      Basic_stateful.make
        (Bonsai.state_opt ~sexp_of_model:[%sexp_of: File.t] ~equal:[%equal: File.t])
        ~view
        graph
    in
    let%arr form = form in
    { form with
      set =
        (function
          | None -> Form.set form None
          | Some file ->
            Effect.print_s
              [%message
                "WARNING: Attempted to set the value of a file select to a value other \
                 than [None]. This is prohibited by the browser and therefore ignored."
                  ~filename:(Bonsai_web_ui_file.filename file : Filename.t)])
    }
  ;;

  let single ?(extra_attrs = Bonsai.return []) ?accept () graph =
    Bonsai.map (single_opt ~extra_attrs ?accept () graph) ~f:optional_to_required
  ;;

  let multiple ?(extra_attrs = Bonsai.return []) ?accept () graph =
    let view =
      let%map extra_attrs = extra_attrs in
      fun ~state ~set_state ->
        (* [value_prop] is a trick that enforces the browser input is cleared when an
           empty value is set into the form: if we ever change from a non-empty map to an
           empty one, then we also set value to "", which clears the form. Otherwise, we
           let the form be uncontrolled while a file is selected.*)
        let value_prop =
          if Map.is_empty state then Vdom.Attr.value_prop "" else Vdom.Attr.empty
        in
        Vdom_input_widgets.File_select.list
          ?accept
          ~extra_attrs:(extra_attrs @ [ value_prop ])
          ~on_input:(fun files ->
            let files =
              List.map files ~f:(fun file ->
                let file = Bonsai_web_ui_file_from_web_file.create file in
                Bonsai_web_ui_file.filename file, file)
              |> Filename.Map.of_alist_exn
            in
            set_state files)
          ()
    in
    let form =
      Basic_stateful.make
        (Bonsai.state
           Filename.Map.empty
           ~sexp_of_model:[%sexp_of: File.t Filename.Map.t]
           ~equal:[%equal: File.t Filename.Map.t])
        ~view
        graph
    in
    let%arr form = form in
    { form with
      set =
        (fun files ->
          match Map.is_empty files with
          | true -> Form.set form files
          | false ->
            Effect.print_s
              [%message
                "WARNING: Attempted to set the value of a file select to a value other \
                 than [Map.empty]. This is prohibited by the browser and therefore \
                 ignored."
                  (files : File.t Filename.Map.t)])
    }
  ;;
end

module Freeform_multiselect = struct
  let set ?(extra_attr = Bonsai.return Vdom.Attr.empty) ?placeholder ?split () graph =
    let freeform_multiselect =
      Bonsai_web_ui_freeform_multiselect.Freeform_multiselect.create
        ?placeholder
        ?split
        ~extra_attr
        ()
        graph
    in
    let%arr value, view, set = freeform_multiselect in
    form_expert_create ~value:(Ok value) ~view ~set
  ;;

  let list ?extra_attr ?placeholder ?split () graph =
    let%map.Bonsai form = set ?extra_attr ?placeholder ?split () graph in
    Form.project form ~parse_exn:Set.to_list ~unparse:String.Set.of_list
  ;;
end

module Rank = struct
  let list
    key
    ?enable_debug_overlay
    ?extra_item_attrs
    ?left
    ?right
    ?empty_list_placeholder
    ?default_item_height
    render
    graph
    =
    let%map.Bonsai value, view, inject =
      Bonsai_web_ui_reorderable_list.with_inject
        key
        ?enable_debug_overlay
        ?extra_item_attrs
        ?left
        ?right
        ?empty_list_placeholder
        ?default_item_height
        (fun ~index:_ ~source key graph ->
          let%map.Bonsai view = render ~source key graph in
          (), view)
        graph
    in
    form_expert_create
      ~value:(Ok (List.map ~f:fst value))
      ~view
      ~set:(fun items -> inject [ Overwrite items ])
  ;;
end

module Query_box = struct
  let create_opt
    (type k cmp)
    (module Key : Bonsai.Comparator with type t = k and type comparator_witness = cmp)
    ?initial_query
    ?max_visible_items
    ?suggestion_list_kind
    ?selected_item_attr
    ?extra_list_container_attr
    ?(extra_input_attr = Bonsai.return Vdom.Attr.empty)
    ?(extra_attr = Bonsai.return Vdom.Attr.empty)
    ~selection_to_string
    ~f
    ()
    graph
    =
    let last_selected_value, set_last_selected_value =
      let module M = struct
        type t = Key.t [@@deriving sexp_of]

        let equal a b = Key.comparator.compare a b = 0
      end
      in
      Bonsai.state_opt graph ~sexp_of_model:[%sexp_of: M.t] ~equal:[%equal: M.t]
    in
    let extra_input_attr =
      let%arr extra_input_attr = extra_input_attr
      and last_selected_value = last_selected_value
      and selection_to_string = selection_to_string in
      match last_selected_value with
      | None -> extra_input_attr
      | Some last_selected_value ->
        let placeholder = selection_to_string last_selected_value in
        Vdom.Attr.combine extra_input_attr (Vdom.Attr.placeholder placeholder)
    in
    let%sub { query; view; _ } =
      Bonsai_web_ui_query_box.create
        (module Key)
        ?initial_query
        ?max_visible_items
        ?suggestion_list_kind
        ?selected_item_attr
        ?extra_list_container_attr
        ~extra_input_attr
        ~extra_attr
        ~f
        ~on_select:
          (let%map set_last_selected_value = set_last_selected_value in
           fun key -> set_last_selected_value (Some key))
        ()
        graph
    in
    let%arr last_selected_value = last_selected_value
    and set_last_selected_value = set_last_selected_value
    and query = query
    and view = view in
    (* It's important that we make the value [None] if the textbox has text in
       it so that people don't get the impression that the textbox represents
       the current form value. *)
    let value = if String.is_empty query then last_selected_value else None in
    form_expert_create ~value:(Ok value) ~view ~set:set_last_selected_value
  ;;

  let create
    key
    ?initial_query
    ?max_visible_items
    ?suggestion_list_kind
    ?selected_item_attr
    ?extra_list_container_attr
    ?extra_input_attr
    ?extra_attr
    ~selection_to_string
    ~f
    ()
    graph
    =
    Bonsai.map
      (create_opt
         key
         ?initial_query
         ?max_visible_items
         ?suggestion_list_kind
         ?selected_item_attr
         ?extra_list_container_attr
         ?extra_input_attr
         ?extra_attr
         ~selection_to_string
         ~f
         ()
         graph)
      ~f:optional_to_required
  ;;

  module Query_box_styles =
    [%css
    stylesheet
      {|
        .list_container {
          background: white;
          border: solid 2px black;
          min-width: 150px;
          outline: none;
          border-bottom-right-radius: 3px;
          border-bottom-left-radius: 3px;
          user-select: none;
        }

        .selected_item {
          background-color: rgb(222, 222, 222);
        }

        .item {
          padding: 5px;
        }

        .description {
          color: #555555;
        }

        .input::placeholder {
          color: black;
        }

        .input:focus::placeholder {
          color: #555555;
        }

        .input:not(:focus):not(:placeholder-shown) {
          color: red;
        }

        .selected_item .description {
          color: black;
        }

        .matched-part {
          font-weight: bold;
        }
        |}]

  let optional_computation opt _graph =
    match opt with
    | None -> Bonsai.return None
    | Some value ->
      let%arr value = value in
      Some value
  ;;

  let optional_computation_value_map x ~default ~f _graph =
    match x with
    | None -> Bonsai.return default
    | Some x ->
      let%arr x = x in
      f x
  ;;

  let underlying_query_box_component
    (type a cmp)
    ?(extra_input_attr = Bonsai.return Vdom.Attr.empty)
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    ~extra_attr
    ~(to_string : (a -> string) Bonsai.t)
    ~selected_item_attr
    ~extra_list_container_attr
    ~all_options
    ~handle_unknown_option
    ~to_option_description
    graph
    =
    let extra_input_attr =
      let%arr extra_input_attr = extra_input_attr in
      Vdom.Attr.many [ Query_box_styles.input; extra_input_attr ]
    in
    create_opt
      (module M)
      ~extra_attr
      ~extra_input_attr
      ~selected_item_attr
      ~extra_list_container_attr
      ~selection_to_string:to_string
      ~f:(fun query graph ->
        let render_item ~to_string ~key ~to_option_description =
          let main_item = Vdom.Node.span [ Vdom.Node.text (to_string key) ] in
          Option.value_map
            to_option_description
            ~default:main_item
            ~f:(fun to_option_description ->
              Bonsai_web.View.vbox
                ~attrs:[ Query_box_styles.item ]
                [ main_item
                ; Vdom.Node.span
                    ~attrs:[ Query_box_styles.description ]
                    [ Vdom.Node.text (to_option_description key) ]
                ])
        in
        let result_without_unknown_option =
          Bonsai.Incr.compute
            (Bonsai.map4
               to_string
               to_option_description
               query
               all_options
               ~f:(fun a b c d -> a, b, c, d))
            ~f:(fun incr ->
              let%pattern_bind.Incr to_string, to_option_description, query, all_options =
                incr
              in
              let%bind.Incr query = query
              and to_string = to_string
              and to_option_description = to_option_description in
              Incr_map.filter_mapi all_options ~f:(fun ~key ~data:() ->
                match
                  Fuzzy_match.is_match
                    ~char_equal:Char.Caseless.equal
                    ~pattern:query
                    (to_string key)
                with
                | false -> None
                | true -> Some (render_item ~to_string ~key ~to_option_description)))
            graph
        in
        let%arr result_without_unknown_option = result_without_unknown_option
        and handle_unknown_option = handle_unknown_option
        and query = query
        and to_string = to_string
        and to_option_description = to_option_description in
        match handle_unknown_option with
        | None -> result_without_unknown_option
        | Some f ->
          (match f query with
           | None -> result_without_unknown_option
           | Some unknown_option ->
             (match Map.mem result_without_unknown_option unknown_option with
              | true -> result_without_unknown_option
              | false ->
                let item =
                  render_item ~to_string ~key:unknown_option ~to_option_description
                in
                Map.set result_without_unknown_option ~key:unknown_option ~data:item)))
      ()
      graph
  ;;

  let single_opt
    (type a cmp)
    ?extra_attrs
    ?extra_input_attr
    ?to_string
    ?to_option_description
    ?selected_item_attr
    ?extra_list_container_attr
    ?handle_unknown_option
    (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
    ~all_options
    : Bonsai.graph -> (a option, Vdom.Node.t) Form.t Bonsai.t
    =
    fun graph ->
    let extra_attr =
      optional_computation_value_map
        extra_attrs
        ~default:Vdom.Attr.empty
        ~f:Vdom.Attr.many
        graph
    in
    let to_string =
      optional_computation_value_map
        ~default:(Fn.compose Sexp.to_string_hum M.sexp_of_t)
        ~f:Fn.id
        to_string
        graph
    in
    let to_option_description = optional_computation to_option_description graph in
    let handle_unknown_option = optional_computation handle_unknown_option graph in
    let selected_item_attr =
      optional_computation_value_map
        selected_item_attr
        ~default:Query_box_styles.selected_item
        ~f:Fn.id
        graph
    in
    let extra_list_container_attr =
      optional_computation_value_map
        extra_list_container_attr
        ~default:Query_box_styles.list_container
        ~f:Fn.id
        graph
    in
    let all_options =
      let%arr all_options = all_options in
      List.fold
        all_options
        ~init:(Map.empty (module M))
        ~f:(fun acc a -> Map.set acc ~key:a ~data:())
    in
    underlying_query_box_component
      ?extra_input_attr
      (module M)
      ~extra_attr
      ~to_string
      ~selected_item_attr
      ~extra_list_container_attr
      ~all_options
      ~handle_unknown_option
      ~to_option_description
      graph
  ;;

  let single
    ?extra_attrs
    ?extra_input_attr
    ?to_string
    ?to_option_description
    ?selected_item_attr
    ?extra_list_container_attr
    ?handle_unknown_option
    m
    ~all_options
    graph
    =
    let%map.Bonsai form =
      single_opt
        ?extra_attrs
        ?extra_input_attr
        ?to_string
        ?to_option_description
        ?selected_item_attr
        ?extra_list_container_attr
        ?handle_unknown_option
        m
        ~all_options
        graph
    in
    optional_to_required form
  ;;
end

module Private = struct
  let sexp_to_pretty_string = sexp_to_pretty_string
end
