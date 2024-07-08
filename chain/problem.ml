open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module type Lease_pool = sig
  (* A lease-pool is a data structure for lending out values of type ['a] to
     a user of the pool for the duration of an effect.

     Values that can be lent out are provided as an input to the component in a
     set, and the pool just makes sure that no two ['a] are being lent out at
     the same time.

     - When a user attempts to perform an effect which requires an ['a], but
       there are no values in the pool at all (busy or available), the callback
       is invoked with [None],
     - if there are available values that aren't being lent out then the
       callback is immediately invoked with [Some a].
     - If all values are currently being lent out, then the effect is enqueued,
       and will be invoked as soon as a currently-lended ['a] becomes available. *)
  type 'item t

  val create
    :  ('item, 'cmp) Comparator.Module.t
    -> ('item, 'cmp) Set.t Bonsai.t
    -> Bonsai.graph
    -> 'item t

  val dispatcher
    :  'item t
    -> (f:('item option -> 'result Effect.t) -> 'result Effect.t) Bonsai.t

  (* you can put whatever you want in this sexp to help you debug *)
  val debug : 'item t -> Sexp.t Bonsai.t
end

module type Keyed_lease_pool = sig
  (* A "keyed lease-pool" is a lease-pool with a key!  Lendable values are
     provided to the component inside of a map which is keyed on their "kind".
     Then, during the dispatching stage, the caller of the dispatch function
     provides a key and only lendable values which are a member of that key are
     valid candidates for the dispatch. *)

  val create
    :  ('group, 'group_cmp) Bonsai.comparator
    -> ('item, 'cmp) Comparator.Module.t
    -> ('item, 'data, 'cmp) Map.t Bonsai.t
    -> group:('item -> 'data -> 'group)
    -> Bonsai.graph
    -> ('group -> f:('item option -> 'result Effect.t) -> 'result Effect.t) Bonsai.t
end

module T = struct
  module Callback = Effect.Private.Callback
  module Computation_status = Bonsai.Computation_status

  type 'a t =
    { take : 'a option Effect.t Bonsai.t
    ; return : ('a -> unit Effect.t) Bonsai.t
    ; debug : Sexp.t Lazy.t Bonsai.t
    }
  [@@deriving fields]

  let debug t = t.debug >>| Lazy.force

  module Model = struct
    type ('a, 'cmp) t =
      { leased_out : ('a, 'cmp) Set.t
      ; waiting : (unit, 'a option) Effect.Private.Callback.t Fdeque.t
      }

    let default cmp = { leased_out = Set.empty cmp; waiting = Fdeque.empty }

    let sexp_of_t { leased_out; waiting } =
      let sexp_of_a = (Set.comparator leased_out).sexp_of_t in
      let leased_out = Set.to_list leased_out in
      [%sexp { leased_out : a list; waiting = (Fdeque.length waiting : int) }]
    ;;

    let equal a b =
      Set.equal a.leased_out b.leased_out && Fdeque.equal phys_equal a.waiting b.waiting
    ;;
  end

  module Action = struct
    type 'a t =
      | Return of 'a
      | Take of (unit, 'a option) Callback.t
      | Pump
  end

  let take_impl ~ctx ~cb ~all ~(model : _ Model.t) =
    let available = Set.diff all model.leased_out in
    match Set.min_elt available with
    | Some hd ->
      Bonsai.Apply_action_context.schedule_event ctx (Callback.respond_to cb (Some hd));
      { model with leased_out = Set.add model.leased_out hd }
    | None -> { model with waiting = Fdeque.enqueue_back model.waiting cb }
  ;;

  let apply_action
    (type a cmp)
    (ctx : _ Bonsai.Apply_action_context.t)
    (input : (a, cmp) Set.t Bonsai.Computation_status.t)
    (model : (a, cmp) Model.t)
    (action : a Action.t)
    : (a, cmp) Model.t
    =
    match action, input with
    | Return a, _ ->
      (match Fdeque.dequeue_front model.waiting with
       | None -> { model with leased_out = Set.remove model.leased_out a }
       | Some (cb, waiting) ->
         Bonsai.Apply_action_context.schedule_event ctx (Callback.respond_to cb (Some a));
         { model with waiting })
    | Take cb, Inactive ->
      Bonsai.Apply_action_context.schedule_event ctx (Callback.respond_to cb None);
      model
    | Take cb, Active all when Set.is_empty all ->
      Bonsai.Apply_action_context.schedule_event ctx (Callback.respond_to cb None);
      model
    | Take cb, Active all -> take_impl ~ctx ~cb ~all ~model
    | Pump, Inactive -> model
    | Pump, Active all ->
      let waiting = model.waiting in
      let model = { model with waiting = Fdeque.empty } in
      Fdeque.fold waiting ~init:model ~f:(fun model cb -> take_impl ~ctx ~cb ~all ~model)
  ;;

  let create cmp set graph =
    let model, inject =
      Bonsai.state_machine1 ~default_model:(Model.default cmp) ~apply_action set graph
    in
    let take =
      let%arr inject = inject in
      Effect.Private.make ~request:() ~evaluator:(fun r ->
        Effect.Expert.handle_non_dom_event_exn (inject (Take r)))
    in
    let return =
      let%arr inject = inject in
      fun a -> inject (Return a)
    in
    let debug =
      let%arr model = model in
      lazy (Model.sexp_of_t model)
    in
    { take; return; debug }
  ;;

  let dispatcher { take; return; _ } =
    let%arr take = take
    and return = return in
    fun ~f ->
      match%bind.Effect take with
      | None -> f None
      | Some a ->
        let%bind.Effect r = f (Some a) in
        let%bind.Effect () = return a in
        Effect.return r
  ;;
end

module Y = struct
  let create group_cmp cmp map ~group graph =
    let map =
      Bonsai.Map.index_byi map ~comparator:group_cmp graph ~index:(fun ~key ~data ->
        Some (group key data))
    in
    let pools =
      Bonsai.assoc group_cmp map graph ~f:(fun _ map graph ->
        let set = Bonsai.Map.keys map graph in
        T.dispatcher (T.create cmp set graph))
    in
    let%arr pools = pools in
    fun key ~f ->
      match Map.find pools key with
      | None -> f None
      | Some dispatcher -> dispatcher ~f
  ;;

  let debug _ = Bonsai.return (sexp_of_opaque ())
end

module _ : Lease_pool = T
module _ : Keyed_lease_pool = Y
