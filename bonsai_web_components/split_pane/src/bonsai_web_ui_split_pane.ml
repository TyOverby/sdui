open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
module Size_tracker = Bonsai_web_ui_element_size_hooks.Size_tracker

(* General implementation overview:

   There are 4 divs -- the container and its 3 children: panel a, separator, panel b

   The container is a flexbox. The width/height of the first panel is set by this library
   and the second is allowed to grow/shrink depending on the reaming space.

   There is a state machine (State.t/Action.t/[state_machine]) which keeps track of

   - the (px) size of the first panel
   - the container dimensions
   - whether there's a drag going on (and if so, what was the initial mouse position
     relative to the start of the separator)

   Resizing is done by having a mouse down listener on the separator. While a resize is in
   progress, an Vdom.Attr global mouse move/up hook is used (which essentially manages an
   event listener on window) to keep track of the mouse position, with the position being
   continuously updated each time the event fires.

   The bonsai size tracker (resize observer wrapper) is used to keep track of the parent
   container dimensions; it fires a state machine event whenever it updates (but not more
   than once a frame thanks to the size tracker).

   {2 Dynamism}

   In order to get the interactive parameter setting in the demo working, the parameters
   are taken as a Value.t. Whenever they get changed an on_change edge trigger emits an
   action asking for a size recalculation/constraint calculation. However, for the most
   part the logic is written in a way that is independent of parameters; recalculating is
   really just about making sure the (possibly new) constraints continue to hold.

   This is why the variable names in the state machine is named in a way that's agnostic
   of things like exact separator size or direction:

   - start/end is used instead of top/bottom and left/right
   - size is used instead of 'width'/'height' *)

module Split_dir = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp, sexp_grammar, equal]
end

module Size = struct
  type t =
    | Percent of Percent.t
    | Px of float
  [@@deriving sexp, sexp_grammar, equal, variants]
end

module Panel = struct
  type t =
    | First
    | Second
  [@@deriving sexp, sexp_grammar, equal]

  let get_panel_px t ~first_panel_px ~separator_size_px ~container_size_px =
    match t with
    | First -> first_panel_px
    | Second -> container_size_px -. Float.of_int separator_size_px -. first_panel_px
  ;;
end

module Panel_and_size = struct
  type t =
    { panel : Panel.t
    ; size : Size.t
    }
  [@@deriving sexp, sexp_grammar, equal, fields ~getters]

  let create panel size = { panel; size }
  let percent panel percent = { panel; size = Percent percent }
  let px panel px = { panel; size = Px px }

  let to_first_panel_px t ~separator_size_px ~container_size_px =
    let sep = separator_size_px |> Float.of_int in
    let size_to_use =
      match t.panel with
      | First -> t.size
      | Second ->
        (match t.size with
         | Percent x -> Size.Percent Percent.(one_hundred_percent - x)
         | Px x -> Px (container_size_px -. sep -. x))
    in
    match size_to_use with
    | Px x -> x
    | Percent x -> (Percent.to_mult x *. container_size_px) -. (sep /. 2.)
  ;;
end

module Constraint = struct
  module Direction = struct
    type t =
      | Min
      | Max
    [@@deriving sexp, sexp_grammar, equal]
  end

  type t =
    { direction : Direction.t
    ; panel : Panel.t
    ; size : Size.t
    }
  [@@deriving sexp, sexp_grammar, equal]

  let of_panel_and_size direction panel_and_size =
    { direction
    ; panel = Panel_and_size.panel panel_and_size
    ; size = Panel_and_size.size panel_and_size
    }
  ;;

  let max_percent ~panel percent =
    of_panel_and_size Max (Panel_and_size.percent panel percent)
  ;;

  let min_percent ~panel percent =
    of_panel_and_size Min (Panel_and_size.percent panel percent)
  ;;

  let max_px ~panel px = of_panel_and_size Max (Panel_and_size.px panel px)
  let min_px ~panel px = of_panel_and_size Min (Panel_and_size.px panel px)
  let to_panel_and_size t = Panel_and_size.create t.panel t.size

  let to_first_panel_size_relative t ~separator_size_px ~container_size_px =
    let direction_relative_to_first_panel =
      match t.direction, t.panel with
      | Min, First -> Direction.Min
      | Max, First -> Max
      | Min, Second -> Max
      | Max, Second -> Min
    in
    let px =
      to_panel_and_size t
      |> Panel_and_size.to_first_panel_px ~separator_size_px ~container_size_px
    in
    direction_relative_to_first_panel, px
  ;;
end

let constraints_to_bounds ~constraints ~separator_size_px ~container_size_px =
  let container_max = container_size_px -. Float.of_int separator_size_px in
  let min', max' =
    List.fold
      constraints
      ~init:(0., container_max)
      ~f:(fun (prev_min, prev_max) constraint_ ->
        match
          Constraint.to_first_panel_size_relative
            constraint_
            ~separator_size_px
            ~container_size_px
        with
        | Min, x -> Float.max x prev_min, prev_max
        | Max, x -> prev_min, Float.min x prev_max)
  in
  (* Clamp the constraints back down to the container *)
  Float.min min' container_max, Float.max max' 0.
;;

module On_container_resize = struct
  type t =
    | Keep_proportion
    | Try_to_keep_panel_size of Panel.t
  [@@deriving sexp, sexp_grammar, equal]

  let to_new_size t ~first_panel_px ~old_container_size_px ~separator_size_px
    : Panel_and_size.t
    =
    match t with
    | Keep_proportion ->
      let percent =
        Percent.of_mult
          ((first_panel_px +. (Float.of_int separator_size_px /. 2.))
           /. old_container_size_px)
      in
      Panel_and_size.percent First percent
    | Try_to_keep_panel_size panel ->
      Panel_and_size.create
        panel
        (Px
           (Panel.get_panel_px
              panel
              ~first_panel_px
              ~separator_size_px
              ~container_size_px:old_container_size_px))
  ;;
end

module Style =
  [%css
  stylesheet
    {|
      :root {
        --separator-color: #eee;
      }

      .horizontal,
      .vertical {
        display: flex;
        width: 100%;
        height: 100%;
        max-width: 100%;
        max-height: 100%;
        overflow: hidden;
        box-sizing: border-box;
      }

      .horizontal {
        flex-direction: row;
      }

      .vertical {
        flex-direction: column;
      }

      .separator {
        flex-shrink: none;
        background-color: var(--separator-color);
        background-repeat: no-repeat;
        background-position: 50%;
      }

      .vertical .separator {
        background-image: url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAFAQMAAABo7865AAAABlBMVEVHcEzMzMzyAv2sAAAAAXRSTlMAQObYZgAAABBJREFUeF5jOAMEEAIEEFwAn3kMwcB6I2AAAAAASUVORK5CYII=");
        cursor: row-resize;
      }

      .horizontal .separator {
        background-image: url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAeCAYAAADkftS9AAAAIklEQVQoU2M4c+bMfxAGAgYYmwGrIIiDjrELjpo5aiZeMwF+yNnOs5KSvgAAAABJRU5ErkJggg==");
        cursor: col-resize;
      }

      .first_panel,
      .second_panel {
        min-width: 0px;
        min-height: 0px;
        overflow: hidden;
      }

      .first_panel {
        flex: none;
      }

      .second_panel {
        flex: 1;
      }

      .no_pointer {
        pointer-events: none;
      }
      |}]

let container_class ~direction =
  match direction with
  | Split_dir.Horizontal -> Style.horizontal
  | Vertical -> Style.vertical
;;

let js_opt_value_exn js ~here = Js_of_ocaml.Js.Opt.to_option js |> Option.value_exn ~here

let get_element_start (element : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t) ~direction
  =
  let rect = element##getBoundingClientRect in
  match direction with
  | Split_dir.Horizontal -> Js_of_ocaml.Js.float_of_number rect##.left
  | Vertical -> Js_of_ocaml.Js.float_of_number rect##.top
;;

let get_parent_element_exn (element : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t)
  : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
  =
  let open Js_of_ocaml in
  element##.parentNode
  |> js_opt_value_exn ~here:[%here]
  |> Dom.CoerceTo.element
  |> js_opt_value_exn ~here:[%here]
  |> Dom_html.element
;;

let get_mouse_pos
  ~(mouse_event : Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
  ~direction
  =
  match direction with
  | Split_dir.Horizontal -> mouse_event##.clientX
  | Vertical -> mouse_event##.clientY
;;

module Container_dimensions = struct
  type t =
    { width : float
    ; height : float
    }
  [@@deriving sexp, equal, fields ~iterators:create] [@@fields.no_zero_alloc]

  let for_direction t ~direction =
    match direction with
    | Split_dir.Horizontal -> t.width
    | Vertical -> t.height
  ;;
end

module Action = struct
  type t =
    | Set_size of Panel_and_size.t
    | Parameters_changed
    | Container_resized of Container_dimensions.t
    | Drag_start of
        { container_start : float
        ; separator_start : float
        ; mouse_pos : int
        }
    | Drag_cancelled
    | Drag_move of { mouse_pos : int }
    | Drag_end of { mouse_pos : int }
  [@@deriving sexp_of]
end

module Panel_sizes = struct
  type t =
    { first_panel_px : float
    ; second_panel_px : float
    }
  [@@deriving fields ~getters] [@@fields.no_zero_alloc]
end

module State = struct
  module Drag = struct
    type t =
      { mouse_offset : float
      ; container_start : float
      }
    [@@deriving sexp, equal]
  end

  type t =
    { first_panel_px : float option
    ; current_drag : Drag.t option
    ; container_dimensions : Container_dimensions.t option
    }
  [@@deriving sexp, equal, fields ~getters ~iterators:create]

  let is_dragging t = Option.is_some t.current_drag

  let initial =
    Fields.create ~first_panel_px:None ~current_drag:None ~container_dimensions:None
  ;;

  let panel_sizes t ~direction ~separator_size_px : Panel_sizes.t option =
    let%bind.Option first_panel_px = t.first_panel_px in
    let%bind.Option container_dimensions = t.container_dimensions in
    let container_size_px =
      Container_dimensions.for_direction container_dimensions ~direction
    in
    let second_panel_px =
      Panel.get_panel_px Second ~first_panel_px ~separator_size_px ~container_size_px
    in
    Some { Panel_sizes.first_panel_px; second_panel_px }
  ;;

  let first_panel_size_css t ~direction =
    let%map.Option px = t.first_panel_px in
    let size_css = `Px (Float.to_int px) in
    match direction with
    | Split_dir.Horizontal -> Css_gen.width size_css
    | Vertical -> Css_gen.height size_css
  ;;

  let update_size t ~panel_and_size ~direction ~separator_size_px ~constraints =
    match t.container_dimensions with
    | Some container_dimensions ->
      let container_size_px =
        Container_dimensions.for_direction container_dimensions ~direction
      in
      let desired_size =
        Panel_and_size.to_first_panel_px
          panel_and_size
          ~separator_size_px
          ~container_size_px
      in
      let sep = Float.of_int separator_size_px in
      if Float.O.(container_size_px < sep)
      then
        (* If the container is really small, don't bother doing anything to avoid
           divisions with small denominators *)
        t
      else (
        let min_size, max_size =
          constraints_to_bounds ~constraints ~container_size_px ~separator_size_px
        in
        let new_px =
          if Float.O.(min_size > max_size)
          then
            (* We just split the difference here *)
            max_size +. ((min_size -. max_size) /. 2.)
          else desired_size |> Float.min max_size |> Float.max min_size
        in
        match t.first_panel_px with
        | Some prev_px when Float.O.(abs (prev_px - new_px) < 0.1) -> t
        | _ -> { t with first_panel_px = Some new_px })
    | None -> t
  ;;

  let change_container_size
    t
    ~container_dimensions
    ~direction
    ~separator_size_px
    ~constraints
    ~initial_size
    ~on_container_resize
    =
    let new_panel_and_size =
      match t.container_dimensions with
      | None -> initial_size
      | Some prev ->
        (match t.first_panel_px with
         | None -> initial_size
         | Some first_panel_px ->
           let old_container_size_px =
             Container_dimensions.for_direction prev ~direction
           in
           On_container_resize.to_new_size
             on_container_resize
             ~first_panel_px
             ~old_container_size_px
             ~separator_size_px)
    in
    { t with container_dimensions = Some container_dimensions }
    |> update_size
         ~panel_and_size:new_panel_and_size
         ~direction
         ~separator_size_px
         ~constraints
  ;;

  let update_size_during_drag
    t
    ~drag
    ~mouse_pos
    ~direction
    ~separator_size_px
    ~constraints
    =
    let { Drag.container_start; mouse_offset } = drag in
    let new_first_px = Float.of_int mouse_pos +. mouse_offset -. container_start in
    update_size
      t
      ~panel_and_size:(Panel_and_size.create First (Size.px new_first_px))
      ~direction
      ~separator_size_px
      ~constraints
  ;;
end

module Parameters = struct
  type t =
    { direction : Split_dir.t
    ; initial_size : Panel_and_size.t
    ; separator_size_px : int
    ; separator_color : Css_gen.Color.t option
    ; on_container_resize : On_container_resize.t
    ; constraints : Constraint.t list
    }
  [@@deriving sexp, equal, sexp_grammar, fields ~iterators:create]

  let default =
    Fields.create
      ~direction:Horizontal
      ~initial_size:(Panel_and_size.percent First (Percent.of_percentage 50.))
      ~separator_size_px:10
      ~separator_color:None
      ~on_container_resize:Keep_proportion
      ~constraints:[]
  ;;
end

let state_machine ~parameters graph =
  let state_machine, inject_action =
    Bonsai.state_machine1
      ~sexp_of_model:[%sexp_of: State.t]
      ~equal:[%equal: State.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model:State.initial
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) input state action ->
        match input with
        | Active
            { Parameters.direction
            ; initial_size
            ; separator_size_px
            ; separator_color = _
            ; on_container_resize
            ; constraints
            } ->
          let state' =
            match action, State.current_drag state with
            | Set_size panel_and_size, _ ->
              State.update_size
                state
                ~panel_and_size
                ~direction
                ~separator_size_px
                ~constraints
            | Parameters_changed, _ ->
              (match state.first_panel_px with
               | None -> (* not yet initialised *) state
               | Some first_panel_px ->
                 State.update_size
                   state
                   ~panel_and_size:(Panel_and_size.px First first_panel_px)
                   ~direction
                   ~separator_size_px
                   ~constraints)
            | Container_resized container_dimensions, _ ->
              State.change_container_size
                state
                ~container_dimensions
                ~direction
                ~initial_size
                ~separator_size_px
                ~on_container_resize
                ~constraints
            | Drag_start { container_start; separator_start; mouse_pos }, _ ->
              let mouse_offset = separator_start -. Float.of_int mouse_pos in
              { state with current_drag = Some { container_start; mouse_offset } }
            | Drag_cancelled, _ -> { state with current_drag = None }
            | Drag_move { mouse_pos }, Some drag ->
              State.update_size_during_drag
                state
                ~drag
                ~mouse_pos
                ~direction
                ~separator_size_px
                ~constraints
            | Drag_end { mouse_pos }, Some drag ->
              State.update_size_during_drag
                { state with current_drag = None }
                ~mouse_pos
                ~drag
                ~direction
                ~separator_size_px
                ~constraints
            | (Drag_move _ | Drag_end _), None -> state
          in
          state'
        | Inactive ->
          eprint_s
            [%message
              [%here]
                "An action sent to a [state_machine1] has been dropped because its input \
                 was not present. This happens when the [state_machine1] is inactive \
                 when it receives a message."
                (action : Action.t)];
          state)
      parameters
      graph
  in
  let () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: Parameters.t]
      ~equal:[%equal: Parameters.t]
      parameters
      ~callback:
        (let%map inject_action = inject_action in
         fun (_ : Parameters.t) -> inject_action Parameters_changed)
      graph
  in
  state_machine, inject_action
;;

module Panel_extra_attrs = struct
  type t =
    { first_panel : Vdom.Attr.t
    ; second_panel : Vdom.Attr.t
    ; separator : Vdom.Attr.t
    }

  let default =
    { first_panel = Vdom.Attr.empty
    ; second_panel = Vdom.Attr.empty
    ; separator = Vdom.Attr.empty
    }
  ;;
end

type t =
  { node : Vdom.Node.t
  ; inject_set_size : Panel_and_size.t -> unit Effect.t
  ; panel_sizes : Panel_sizes.t option
  }

let to_vdom t = t.node
let inject_set_size t = t.inject_set_size
let panel_sizes t = t.panel_sizes

let create_separator ~listeners ~direction ~size ~extra_attr =
  let width, height =
    match direction with
    | Split_dir.Horizontal -> `Px size, Css_gen.Length.percent100
    | Split_dir.Vertical -> Css_gen.Length.percent100, `Px size
  in
  Node.div
    ~attrs:
      [ Attr.style (Css_gen.concat [ Css_gen.width width; Css_gen.height height ])
      ; Style.separator
      ; listeners
      ; extra_attr
      ]
    []
;;

let create_from_parameters
  ?(panel_extra_attrs = Bonsai.return Panel_extra_attrs.default)
  parameters
  ~first_panel
  ~second_panel
  graph
  =
  let state, inject_action = state_machine ~parameters graph in
  let size_change_attr =
    (* compute this attr separately in order to be precise about its dependencies
       (only depending on inject_action) so that the tracker isn't being continuously
       recreated *)
    let%arr inject_action = inject_action in
    Size_tracker.on_change (fun ~width ~height ->
      inject_action
        (Container_resized (Container_dimensions.Fields.create ~width ~height)))
  in
  let container_builder =
    let%arr state = state
    and inject_action = inject_action
    and size_change_attr = size_change_attr
    and { Parameters.separator_size_px; direction; separator_color; _ } = parameters
    and panel_extra_attrs = panel_extra_attrs in
    let separator_listeners =
      if State.is_dragging state
      then Attr.empty
      else
        Attr.many
          [ Attr.on_mousedown (fun mouse_event ->
              let separator =
                mouse_event##.currentTarget |> js_opt_value_exn ~here:[%here]
              in
              let separator_start = get_element_start separator ~direction in
              let container = get_parent_element_exn separator in
              let container_start = get_element_start container ~direction in
              let mouse_pos = get_mouse_pos ~mouse_event ~direction in
              inject_action
                (Action.Drag_start { mouse_pos; container_start; separator_start }))
          ; (* This is removed during dragging, but needed to capture clicks on the
               separator within one DOM frame (without any mouse move) as we won't have
               the global mouseup listener until after the vdom is applied next *)
            Attr.on_mouseup (fun _ -> inject_action Action.Drag_cancelled)
          ]
    in
    let global_listeners =
      if State.is_dragging state
      then
        Attr.many
          [ Attr.Global_listeners.mousemove ~phase:Capture ~f:(fun mouse_event ->
              let mouse_pos = get_mouse_pos ~mouse_event ~direction in
              inject_action (Action.Drag_move { mouse_pos }))
          ; Attr.Global_listeners.mouseup ~phase:Capture ~f:(fun mouse_event ->
              let mouse_pos = get_mouse_pos ~mouse_event ~direction in
              inject_action (Action.Drag_end { mouse_pos }))
          ]
      else Attr.empty
    in
    let container_css =
      if State.is_dragging state
      then Attr.style (Css_gen.concat [ Css_gen.user_select `None ])
      else Attr.empty
    in
    let dragging_style =
      if State.is_dragging state then Style.no_pointer else Attr.empty
    in
    let separator =
      create_separator
        ~direction
        ~size:separator_size_px
        ~listeners:separator_listeners
        ~extra_attr:panel_extra_attrs.separator
    in
    let wrapper_attr =
      Attr.(
        container_class ~direction
        @ Style.Variables.set
            ?separator_color:(Option.map separator_color ~f:Css_gen.Color.to_string_css)
            ()
        @ global_listeners
        @ container_css
        @ size_change_attr)
    in
    let inject_set_size panel_and_size = inject_action (Set_size panel_and_size) in
    fun first_panel second_panel ->
      let children =
        match State.first_panel_size_css state ~direction with
        | None ->
          (match Bonsai_web.am_running_how with
           | `Browser | `Browser_benchmark ->
             (* We don't render the children until we know the right sizes to use *)
             []
           | `Node | `Node_benchmark | `Node_test ->
             [ Node.div
                 ~attrs:
                   [ Style.first_panel; dragging_style; panel_extra_attrs.first_panel ]
                 [ first_panel ]
             ; separator
             ; Node.div
                 ~attrs:
                   [ Style.second_panel; dragging_style; panel_extra_attrs.second_panel ]
                 [ second_panel ]
             ])
        | Some first_panel_css ->
          [ Node.div
              ~attrs:
                [ Attr.(style first_panel_css @ Style.first_panel)
                ; dragging_style
                ; panel_extra_attrs.first_panel
                ]
              [ first_panel ]
          ; separator
          ; Node.div
              ~attrs:
                [ Style.second_panel; dragging_style; panel_extra_attrs.second_panel ]
              [ second_panel ]
          ]
      in
      let node = Node.div ~attrs:[ wrapper_attr ] children in
      { node
      ; inject_set_size
      ; panel_sizes = State.panel_sizes ~direction ~separator_size_px state
      }
  in
  let%arr container_builder = container_builder
  and first_panel = first_panel
  and second_panel = second_panel in
  container_builder first_panel second_panel
;;

let create
  ?(initial_size = Bonsai.return Parameters.default.initial_size)
  ?(separator_size_px = Bonsai.return Parameters.default.separator_size_px)
  ?separator_color
  ?(on_container_resize = Bonsai.return Parameters.default.on_container_resize)
  ?(constraints = Bonsai.return Parameters.default.constraints)
  ?panel_extra_attrs
  ~direction
  ~first_panel
  ~second_panel
  ()
  graph
  =
  let separator_color =
    match separator_color with
    | Some value -> Bonsai.map ~f:Option.some value
    | None -> Bonsai.return None
  in
  let parameters =
    let%arr initial_size = initial_size
    and separator_size_px = separator_size_px
    and separator_color = separator_color
    and on_container_resize = on_container_resize
    and constraints = constraints
    and direction = direction in
    { Parameters.initial_size
    ; separator_size_px
    ; separator_color
    ; on_container_resize
    ; constraints
    ; direction
    }
  in
  create_from_parameters ?panel_extra_attrs parameters ~first_panel ~second_panel graph
;;

module For_testing = struct
  module Parameters = Parameters

  let create_from_parameters = create_from_parameters ?panel_extra_attrs:None

  module Container_dimensions = Container_dimensions
  module Action = Action
  module State = State

  let state_machine = state_machine
end
