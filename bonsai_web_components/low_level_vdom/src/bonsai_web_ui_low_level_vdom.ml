open! Core
open! Bonsai_web
open! Js_of_ocaml
open Bonsai.Let_syntax
open Virtual_dom

module Mutable_state_tracker = struct
  module Id = Unique_id.Int ()

  let zero = Id.create ()

  type 's t =
    { unsafe_init : 's -> Id.t
    ; unsafe_destroy : Id.t -> unit
    ; modify : ('s -> unit) -> unit Effect.t
    ; read : 'a. ('s -> 'a) -> 'a list Effect.t
    }

  module Model = struct
    type 's t = 's Id.Map.t

    let sexp_of_t = sexp_of_opaque
    let equal = phys_equal
  end

  module Action = struct
    type 's t =
      | Register of
          { id : Id.t
          ; state : 's
          }
      | Destroy of Id.t
      | Modify of ('s -> unit)
  end

  let component (type s) () graph =
    let module Model = struct
      include Model

      type nonrec t = s t
    end
    in
    let model, inject =
      Bonsai.state_machine0
        graph
        ~sexp_of_model:[%sexp_of: Model.t]
        ~equal:[%equal: Model.t]
        ~sexp_of_action:sexp_of_opaque
        ~reset:(fun (_ : _ Bonsai.Apply_action_context.t) m -> m)
        ~default_model:Id.Map.empty
        ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model -> function
          | Action.Register { id; state } -> Map.set model ~key:id ~data:state
          | Destroy id -> Map.remove model id
          | Modify f ->
            Map.iter model ~f;
            model)
    in
    let get_model = Bonsai.peek model graph in
    let%arr inject = inject
    and get_model = get_model in
    let unsafe_init state =
      let id = Id.create () in
      Effect.Expert.handle_non_dom_event_exn (inject (Register { id; state }));
      id
    in
    let unsafe_destroy id =
      Effect.Expert.handle_non_dom_event_exn (inject (Destroy id))
    in
    let modify f = inject (Modify f) in
    let read r =
      match%map.Effect get_model with
      | Inactive -> []
      | Active m -> List.map (Map.data m) ~f:r
    in
    { unsafe_init; unsafe_destroy; modify; read }
  ;;
end

module State = struct
  type ('input, 'state) t =
    { mutable input : 'input
    ; state : 'state
    ; mutable id : Mutable_state_tracker.Id.t
    ; get_input : unit -> 'input
    }
end

type ('input, 'state) reader = { f : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t }

module Widget = struct
  module type S = sig
    type element = private #Dom_html.element
    type input
    type state

    val init : get_input:(unit -> input) -> input -> state * element Js.t
    val update : prev_input:input -> input -> state -> element Js.t -> element Js.t
    val destroy : input -> state -> element Js.t -> unit
  end

  type ('input, 'state) t =
    { view : Vdom.Node.t
    ; modify : ('input -> 'state -> unit) -> unit Effect.t
    ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
    }

  let component
    (type input state)
    ?(vdom_for_testing = fun _ -> Vdom.Node.create "widget" [])
    (module M : S with type input = input and type state = state)
    input
    graph
    =
    let id =
      Bonsai.Expert.thunk
        ~f:(fun () -> Type_equal.Id.create ~name:"widget" sexp_of_opaque)
        graph
    in
    let state_tracker = Mutable_state_tracker.component () graph in
    let view =
      let%arr input = input
      and id = id
      and { unsafe_init; unsafe_destroy; _ } = state_tracker in
      Vdom.Node.widget
        ~vdom_for_testing:(lazy (vdom_for_testing input))
        ~id
        ~init:(fun () ->
          let the_state = ref None in
          let get_input () =
            match !the_state with
            | None -> input
            | Some s -> s.State.input
          in
          let state, element = M.init ~get_input input in
          let s = { State.input; state; id = Mutable_state_tracker.zero; get_input } in
          the_state := Some s;
          let id = unsafe_init s in
          s.id <- id;
          s, element)
        ~update:(fun s element ->
          let { State.input = prev_input; state; get_input = _; id = _ } = s in
          if phys_equal input prev_input
          then s, element
          else (
            s.input <- input;
            let element = M.update ~prev_input input state element in
            s, element))
        ~destroy:(fun s element ->
          let { State.input; state; id; get_input = _ } = s in
          unsafe_destroy id;
          M.destroy input state element)
        ()
    in
    let funs =
      let%arr state_tracker = state_tracker in
      let modify f = state_tracker.modify (fun s -> f s.input s.state) in
      let reader = { f = (fun f -> state_tracker.read (fun s -> f s.input s.state)) } in
      modify, reader
    in
    let%arr view = view
    and modify, reader = funs in
    { view; modify; read = reader.f }
  ;;
end

module Hook = struct
  module type S = sig
    type input
    type state

    val init : get_input:(unit -> input) -> input -> Dom_html.element Js.t -> state
    val update : prev_input:input -> input -> state -> Dom_html.element Js.t -> unit
    val destroy : input -> state -> Dom_html.element Js.t -> unit
  end

  type ('input, 'state) t =
    { attr : Vdom.Attr.t
    ; modify : ('input -> 'state -> unit) -> unit Effect.t
    ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
    }

  let component
    (type input state)
    (module M : S with type input = input and type state = state)
    ~hook_name
    input
    graph
    =
    let id =
      Bonsai.Expert.thunk
        ~f:(fun () -> Type_equal.Id.create ~name:"hook-id" sexp_of_opaque)
        graph
    in
    let input_id =
      Bonsai.Expert.thunk
        ~f:(fun () -> Type_equal.Id.create ~name:"input-id" sexp_of_opaque)
        graph
    in
    let state_tracker = Mutable_state_tracker.component () graph in
    let attr =
      let%arr input = input
      and id = id
      and input_id = input_id
      and { unsafe_init; unsafe_destroy; _ } = state_tracker in
      Vdom.Attr.create_hook
        hook_name
        (Vdom.Attr.Hooks.unsafe_create
         (* This function's API only allows creating this hook with one input value
            at a time.*)
           ~combine_inputs:(fun _ i -> i)
           ~id
           ~extra:(input, input_id)
           ~init:(fun input element ->
             let the_state = ref None in
             let get_input () =
               match !the_state with
               | None -> input
               | Some s -> s.State.input
             in
             let state = M.init ~get_input input element in
             let s = { State.input; state; id = Mutable_state_tracker.zero; get_input } in
             the_state := Some s;
             let id = unsafe_init s in
             s.id <- id;
             input, (), s)
           ~update:(fun input (_, (), s) element ->
             let { State.input = prev_input; state; get_input = _; id = _ } = s in
             if phys_equal input prev_input
             then input, (), s
             else (
               s.input <- input;
               M.update ~prev_input input state element;
               input, (), s))
           ~destroy:(fun (_, (), s) element ->
             let { State.input; state; id; get_input = _ } = s in
             unsafe_destroy id;
             M.destroy input state element))
    in
    let funs =
      let%arr state_tracker = state_tracker in
      let modify f = state_tracker.modify (fun s -> f s.input s.state) in
      let reader = { f = (fun f -> state_tracker.read (fun s -> f s.input s.state)) } in
      modify, reader
    in
    let%arr attr = attr
    and modify, reader = funs in
    { attr; modify; read = reader.f }
  ;;
end

module Dom_ref = struct
  type t =
    { attr : Vdom.Attr.t
    ; nodes : Dom_html.element Js.t list Effect.t
    }

  module T = struct
    type input = unit
    type state = Dom_html.element Js.t

    let init ~get_input:_ () element = element
    let update ~prev_input:() () _element _element = ()
    let destroy () _element _element = ()
  end

  let tracker graph =
    let c = Hook.component (module T) ~hook_name:"ref" (Bonsai.return ()) graph in
    let%arr { Hook.attr; read; modify = _ } = c in
    let nodes = read (fun () state -> state) in
    { attr; nodes }
  ;;
end
