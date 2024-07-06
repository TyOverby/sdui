open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module type Lease_pool = sig
  (* A ['a Lease_pool.t] is a data structure for lending out unique access to
     values of type ['a] for the duration of an Effect.  The set of loanable
     items can change over time *)
  type 'a t

  (* [take] gives you an effect that - when scheduled - retrieves an item
     from the pool.  If there are no items, (or if the pool is inactive),
     then the Effect won't complete until an item is available. *)
  val take : 'a t -> 'a option Effect.t Bonsai.t

  (* [return] allows the user to return an item to the pool. *)
  val return : 'a t -> ('a -> unit Effect.t) Bonsai.t

  (* you can put whatever you want in this sexp to help you debug *)
  val debug : 'a t -> Sexp.t Bonsai.t

  (* creates an instance of the pool *)
  val create
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a, 'cmp) Set.t Bonsai.t
    -> Bonsai.graph
    -> 'a t
end

module type Keyed_pool = sig
  (* creates an instance of the pool *)
  val create
    :  ('key, 'key_cmp) Comparator.Module.t
    -> ('a, 'cmp) Comparator.Module.t
    -> ('key, 'key_cmp, ('a, 'cmp) Set.t) Map.t Bonsai.t
    -> Bonsai.graph
    -> ('key -> f:('a option -> 'b Effect.t) -> 'b Effect.t) Bonsai.t
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
end

module Y = struct
  let create key_cmp cmp map graph =
    let pools =
      Bonsai.assoc key_cmp map graph ~f:(fun _ set graph ->
        let { T.take; return; debug = _ } = T.create cmp set graph in
        Bonsai.map2 take return ~f:Tuple2.create)
    in
    let%arr pools = pools in
    fun key ~f ->
      match Map.find pools key with
      | None -> Effect.return None
      | Some (take, return) ->
        (match%bind.Effect take with
         | None -> Effect.return None
         | Some a ->
           let%bind.Effect r = f a in
           let%bind.Effect () = return a in
           Effect.return r)
  ;;
end

module _ : Lease_pool = T
