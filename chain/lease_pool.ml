open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Callback = Effect.Private.Callback
module Computation_status = Bonsai.Computation_status

module Pred_and_cb = struct
  type ('key, 'data) t =
    { pred : 'key -> 'data -> bool
    ; cb : (unit, ('key * 'data) Or_error.t) Effect.Private.Callback.t
    ; info : Sexp.t option
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
end

module Action = struct
  type ('key, 'data) t =
    | Return of 'key
    | Take of ('key, 'data) Pred_and_cb.t
    | Pump
    | Clear_all
end

let no_items_in_pool_error = Error.of_string "no items in pool"

let take_impl ~schedule_event ~cb ~all ~pred ~info ~(model : _ Model.t) =
  let pickable = List.filter (Map.to_alist all) ~f:(fun (k, v) -> pred k v) in
  if List.is_empty pickable
  then (
    schedule_event (Callback.respond_to cb (Error no_items_in_pool_error));
    model)
  else (
    match List.find pickable ~f:(fun (k, _) -> not (Set.mem model.leased_out k)) with
    | Some (k, v) ->
      schedule_event (Callback.respond_to cb (Ok (k, v)));
      { model with leased_out = Set.add model.leased_out k }
    | None ->
      { model with waiting = Fdeque.enqueue_back model.waiting { pred; cb; info } })
;;

let pump_impl ~model ~schedule_event ~all =
  let waiting = model.Model.waiting in
  let model = { model with waiting = Fdeque.empty } in
  Fdeque.fold waiting ~init:model ~f:(fun model { cb; pred; info } ->
    take_impl ~schedule_event ~cb ~all ~model ~pred ~info)
;;

let apply_action ctx input model action =
  let schedule_event = Bonsai.Apply_action_context.schedule_event ctx in
  match (action : _ Action.t), (input : _ Bonsai.Computation_status.t) with
  | Return key, Inactive ->
    { (model : _ Model.t) with leased_out = Set.remove model.leased_out key }
  | Take { cb; pred = _; info = _ }, Inactive ->
    schedule_event (Callback.respond_to cb (Error no_items_in_pool_error));
    model
  | Take { cb; pred; info }, Active all ->
    take_impl ~schedule_event ~cb ~all ~model ~pred ~info
  | Pump, Inactive -> model
  | Pump, Active all -> pump_impl ~model ~all ~schedule_event
  | Return key, Active all ->
    let model = { model with leased_out = Set.remove model.leased_out key } in
    pump_impl ~model ~all ~schedule_event
  | Clear_all, _ ->
    model.waiting
    |> Fdeque.iter ~f:(fun { cb; _ } ->
      schedule_event (Callback.respond_to cb (Error (Error.of_string "evicted"))));
    { model with waiting = Fdeque.empty }
;;

type ('key, 'data) t =
  { take :
      (info:Sexp.t option
       -> pred:('key -> 'data -> bool)
       -> ('key * 'data) Or_error.t Effect.t)
        Bonsai.t
  ; return : ('key -> unit Effect.t) Bonsai.t
  ; clear_all : unit Effect.t Bonsai.t
  ; debug : Sexp.t Lazy.t Bonsai.t
  }
[@@deriving fields]

let debug t = t.debug >>| Lazy.force

let create cmp ?(data_equal = phys_equal) map graph =
  let model, inject =
    Bonsai.state_machine1 ~default_model:(Model.default cmp) ~apply_action map graph
  in
  let pump =
    let%map inject = inject in
    inject Pump
  in
  Bonsai.Edge.on_change
    map
    graph
    ~equal:(Map.equal data_equal)
    ~callback:(pump >>| Fn.const);
  Bonsai.Edge.lifecycle ~on_activate:pump graph;
  let take =
    let%arr inject = inject in
    fun ~info ~pred ->
      Effect.Private.make ~request:() ~evaluator:(fun cb ->
        Effect.Expert.handle_non_dom_event_exn (inject (Take { pred; cb; info })))
  in
  let return =
    let%arr inject = inject in
    fun a -> inject (Return a)
  in
  let clear_all =
    let%arr inject = inject in
    inject Clear_all
  in
  let debug =
    let%arr model = model in
    lazy (Model.sexp_of_t model)
  in
  { take; return; debug; clear_all }
;;

let default_pred _ _ = true

let dispatcher { take; return; _ } =
  let%arr take = take
  and return = return in
  fun ?info ?(pred = default_pred) f ->
    match%bind.Effect take ~pred ~info with
    | Error _ as e -> f e
    | Ok (key, _) as v ->
      let%bind.Effect r = f v in
      let%bind.Effect () = return key in
      Effect.return r
;;
