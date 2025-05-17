(* copy/paste from [bonsai_web_ui_visibility] *)
open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Js_of_ocaml

(* This Id is used to identify instances of hooks.  This is so that a state-machine can
   keep all the visibility information for instances separate from one another. *)
module Id = Unique_id.Int63 ()

module Action = struct
  type action =
    | Set_visible
    | Set_hidden
    | Install
    | Remove
  [@@deriving sexp_of]

  type t = Id.t * action [@@deriving sexp_of]
end

module T = struct
  module Input = struct
    type t = Action.t -> unit Effect.t [@@deriving sexp_of]

    (* Always schedule both because we don't want one consumer to be able to
       block another from receiving an action. *)
    let combine left right action = Effect.Many [ left action; right action ]
  end

  module State = struct
    type t =
      { mutable callback : Action.action -> unit Effect.t
      ; mutable last_state : [ `Visible | `Hidden ] option
      ; observer : IntersectionObserver.intersectionObserver Js.t
      ; id : Id.t
      }
  end

  let process_entries
    (state : State.t Lazy.t)
    (entries : IntersectionObserver.intersectionObserverEntry Js.t Js.js_array Js.t)
    _observer
    =
    let state = Lazy.force state in
    Array.iter (Js.to_array entries) ~f:(fun entry ->
      let new_state = if Js.to_bool entry##.isIntersecting then `Visible else `Hidden in
      (match state.last_state, new_state with
       | Some `Visible, `Visible -> ()
       | Some `Hidden, `Hidden -> ()
       | _, `Visible ->
         Effect.Expert.handle_non_dom_event_exn (state.callback Set_visible)
       | _, `Hidden -> Effect.Expert.handle_non_dom_event_exn (state.callback Set_hidden));
      state.last_state <- Some new_state);
    ()
  ;;

  let init callback element =
    let id = Id.create () in
    let callback action = callback (id, action) in
    let rec state =
      lazy
        (let options = IntersectionObserver.empty_intersection_observer_options () in
         (* A threshold of [0, 1] means "notify me when it crosses the visible-to-hidden
            boundary" *)
         options##.threshold := Js.array [| Js.float 0.0; Js.float 1.0 |];
         let observer =
           new%js IntersectionObserver.intersectionObserver
             (Js.wrap_callback (process_entries state))
             options
         in
         observer##observe element;
         { State.callback; observer; last_state = None; id })
    in
    (* start by sending an 'install' message to our consumer *)
    Effect.Expert.handle_non_dom_event_exn (callback Install);
    Lazy.force state
  ;;

  let destroy _input (state : State.t) _element =
    state.observer##disconnect;
    (* send 'remove' message *)
    Effect.Expert.handle_non_dom_event_exn (state.callback Remove)
  ;;

  let update ~old_input ~new_input state _element =
    (* if [old_input] and [new_input] are the same, then we have nothing to do *)
    if not (phys_equal old_input new_input)
    then (
      let callback action = new_input (state.State.id, action) in
      state.callback <- callback;
      (* Our [old_input] may have had an injection function that will be expecting a
         [Remove], so we send one regardless of if it's necessary or not. *)
      Effect.Expert.handle_non_dom_event_exn (old_input (state.id, Remove));
      (* If we got a new callback input (or if we just sent a [Remove] to an existing
         consumer), we should follow up with the last state that we just saw. *)
      Effect.Expert.handle_non_dom_event_exn
        (callback
           (match state.last_state with
            | None -> Install
            | Some `Hidden -> Set_hidden
            | Some `Visible -> Set_visible)));
    ()
  ;;

  let on_mount = `Do_nothing
end

module Hook = Vdom.Attr.Hooks.Make (T)

let attr inject = Vdom.Attr.create_hook "visibility-tracker" (Hook.create inject)

let create ~on_become_visible =
  attr (function
    | _, Set_visible -> on_become_visible
    | _ -> Effect.Ignore)
;;
