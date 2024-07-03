open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Job = struct
  type ('resource, 'spec) t =
    | T :
        { callback : ('arg, 'result) Effect.Private.Callback.t
        ; dispatch : 'spec -> 'resource -> 'arg -> 'result Effect.t
        ; arg : 'arg
        ; spec : 'spec
        ; sexp_of_arg : 'arg -> Sexp.t
        }
        -> ('resource, 'spec) t

  let sexp_of_t
    _sexp_of_resource
    sexp_of_spec
    (T { arg; spec; sexp_of_arg; callback = _; dispatch = _ })
    =
    [%sexp { arg : arg; spec : spec }]
  ;;

  let create ?(sexp_of_arg = sexp_of_opaque) ~dispatch ~queue spec arg =
    Effect.Private.make ~request:arg ~evaluator:(fun callback ->
      let job = T { dispatch; arg; spec; callback; sexp_of_arg } in
      Effect.Expert.handle_non_dom_event_exn
        (Effect.ignore_m (Job_queue.push_back queue spec job)))
  ;;
end

module Status = struct
  type t =
    | Loitering_or_inactive
    | Waiting_for_work
    | Working
  [@@deriving sexp_of]

  let loitering_or_inactive = Loitering_or_inactive
end

module Action = struct
  type ('resource, 'spec) t =
    | Wake_up
    | Start_work of
        { job : ('resource, 'spec) Job.t
        ; queue : (('resource, 'spec) Job.t, 'spec) Job_queue.t
        }
    | Finished_work
end

let apply_action
  ctx
  (input : _ Bonsai.Computation_status.t)
  (model : Status.t)
  (action : _ Action.t)
  : Status.t
  =
  let schedule = Bonsai.Apply_action_context.schedule_event ctx in
  let inject = Bonsai.Apply_action_context.inject ctx in
  match model, input, action with
  | Loitering_or_inactive, Active (queue, _resource, spec), (Finished_work | Wake_up)
  | Working, Active (queue, _resource, spec), Finished_work ->
    (* if woken up _or_ just finished a job, request a new job *)
    schedule
      (let%bind.Effect job = Job_queue.pop_front queue spec in
       inject (Action.Start_work { job; queue }));
    Waiting_for_work
  | Waiting_for_work, Active (_, resource, _), Start_work { job; queue = _ } ->
    (* if you're waiting for work and get a job, dispatch it and transition to
       the "working" state *)
    let (T { callback; dispatch; arg; spec; sexp_of_arg = _ }) = job in
    schedule
      (let%bind.Effect result = dispatch spec resource arg in
       let%bind.Effect () = Effect.Private.Callback.respond_to callback result in
       inject Finished_work);
    Working
  | ( (Loitering_or_inactive | Waiting_for_work)
    , Inactive
    , Start_work { job = T { spec; _ } as job; queue } ) ->
    (* If you got work while inactive, send it back *)
    schedule (Effect.ignore_m (Job_queue.push_back queue spec job));
    Loitering_or_inactive
  | Working, _, Wake_up -> model
  | Working, Inactive, Finished_work -> Loitering_or_inactive
  | Working, (Active _ | Inactive), Start_work { job = T { spec; _ } as job; queue } ->
    (* If you got work while working, send it back *)
    print_s [%message "BUG" [%here]];
    schedule (Effect.ignore_m (Job_queue.push_back queue spec job));
    Loitering_or_inactive
  | model, Inactive, Wake_up ->
    (* if you got woken up and are inactive, don't do anything *)
    model
  | Loitering_or_inactive, Active _, Start_work _ ->
    print_s [%message "BUG" [%here]];
    model
  | Loitering_or_inactive, Inactive, Finished_work ->
    print_s [%message "BUG" [%here]];
    model
  | Waiting_for_work, _, Wake_up ->
    print_s [%message "BUG" [%here]];
    model
  | Waiting_for_work, _, Finished_work ->
    print_s [%message "BUG" [%here]];
    model
;;

let component
  (type spec resource)
  ~(queue : ((resource, spec) Job.t, spec) Job_queue.t Bonsai.t)
  ~(resource : resource Bonsai.t)
  ~(spec : spec Bonsai.t)
  ~(spec_compare : spec -> spec -> int)
  graph
  =
  let module Spec = struct
    module T = struct
      type t = spec

      let sexp_of_t = sexp_of_opaque
      let compare = spec_compare
    end

    include T
    include Comparator.Make (T)
  end
  in
  Bonsai.scope_model
    (module Spec)
    graph
    ~on:spec
    ~for_:(fun graph ->
      let model, inject =
        Bonsai.state_machine1
          ~default_model:Status.Loitering_or_inactive
          ~apply_action
          (Bonsai.map3 queue resource spec ~f:Tuple3.create)
          graph
      in
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr inject = inject in
           inject Wake_up)
        graph;
      model)
;;
