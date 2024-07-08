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
  type ('key, 'data) t

  val create
    :  ('key, 'cmp) Comparator.Module.t
    -> ('key, 'data, 'cmp) Map.t Bonsai.t
    -> Bonsai.graph
    -> ('key, 'data) t

  val dispatcher
    :  ('key, 'data) t
    -> (pred:('key -> 'data -> bool)
        -> f:(('key * 'data) option -> 'result Effect.t)
        -> 'result Effect.t)
         Bonsai.t
end

module T = struct
  module Callback = Effect.Private.Callback
  module Computation_status = Bonsai.Computation_status

  type ('key, 'data) t =
    { take : (pred:('key -> 'data -> bool) -> ('key * 'data) option Effect.t) Bonsai.t
    ; return : ('key -> unit Effect.t) Bonsai.t
    ; debug : Sexp.t Lazy.t Bonsai.t
    }
  [@@deriving fields]

  let debug t = t.debug >>| Lazy.force

  module Pred_and_cb = struct
    type ('key, 'data) t =
      { pred : 'key -> 'data -> bool
      ; cb : (unit, ('key * 'data) option) Effect.Private.Callback.t
      }
  end

  module Model = struct
    type ('key, 'data, 'cmp) t =
      { leased_out : ('key, 'cmp) Set.t
      ; waiting : ('key, 'data) Pred_and_cb.t Fdeque.t
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
    type ('key, 'data) t =
      | Return of 'key
      | Take of ('key, 'data) Pred_and_cb.t
      | Pump
  end

  let take_impl ~ctx ~cb ~all ~pred ~(model : _ Model.t) =
    let pickable = List.filter (Map.to_alist all) ~f:(fun (k, v) -> pred k v) in
    if List.is_empty pickable
    then (
      Bonsai.Apply_action_context.schedule_event ctx (Callback.respond_to cb None);
      model)
    else (
      match List.find pickable ~f:(fun (k, _) -> not (Set.mem model.leased_out k)) with
      | Some (k, v) ->
        Bonsai.Apply_action_context.schedule_event
          ctx
          (Callback.respond_to cb (Some (k, v)));
        { model with leased_out = Set.add model.leased_out k }
      | None -> { model with waiting = Fdeque.enqueue_back model.waiting { pred; cb } })
  ;;

  let pump_impl ~(model : _ Model.t) ~ctx ~all =
    let waiting = model.waiting in
    let model = { model with waiting = Fdeque.empty } in
    Fdeque.fold waiting ~init:model ~f:(fun model { cb; pred } ->
      take_impl ~ctx ~cb ~all ~model ~pred)
  ;;

  let apply_action
    (type key data cmp)
    (ctx : _ Bonsai.Apply_action_context.t)
    (input : (key, data, cmp) Map.t Bonsai.Computation_status.t)
    (model : (key, data, cmp) Model.t)
    (action : (key, data) Action.t)
    : (key, data, cmp) Model.t
    =
    match action, input with
    | Return key, Inactive -> { model with leased_out = Set.remove model.leased_out key }
    | Take { cb; pred = _ }, Inactive ->
      Bonsai.Apply_action_context.schedule_event ctx (Callback.respond_to cb None);
      model
    | Take { cb; pred }, Active all -> take_impl ~ctx ~cb ~all ~model ~pred
    | Pump, Inactive -> model
    | Pump, Active all -> pump_impl ~model ~all ~ctx
    | Return key, Active all ->
      let model = { model with leased_out = Set.remove model.leased_out key } in
      pump_impl ~model ~all ~ctx
  ;;

  let create cmp set graph =
    let model, inject =
      Bonsai.state_machine1 ~default_model:(Model.default cmp) ~apply_action set graph
    in
    let take =
      let%arr inject = inject in
      fun ~pred ->
        Effect.Private.make ~request:() ~evaluator:(fun cb ->
          Effect.Expert.handle_non_dom_event_exn (inject (Take { pred; cb })))
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
    fun ~pred ~f ->
      match%bind.Effect take ~pred with
      | None -> f None
      | Some (key, data) ->
        let%bind.Effect r = f (Some (key, data)) in
        let%bind.Effect () = return key in
        Effect.return r
  ;;
end

module _ : Lease_pool = T
