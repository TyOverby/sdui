open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Item = struct
  type 'resource t =
    { on_start : unit Effect.t
    ; on_done : unit Effect.t
    ; resource : 'resource
    }
end

module Status = struct
  type t =
    | Loitering
    | In_queue of Job_queue.Item_id.t
    | Working
end

module Action = struct
  type ('resource, 'spec) t =
    | Activate of
        { queue : ('resource Item.t, 'spec) Job_queue.t
        ; item : 'resource
        ; spec : 'spec
        }
    | Start
    | Complete
    | Set_job_id of Job_queue.Item_id.t
end

module Model = struct
  type ('resource, 'spec) t =
    { queue : ('resource Item.t, 'spec) Job_queue.t option
    ; status : Status.t
    }
end

let component
  (type resource spec)
  ~(job_queue : (resource Item.t, spec) Job_queue.t Bonsai.t)
  ~(item : resource option Bonsai.t)
  ~(spec : spec option Bonsai.t)
  graph
  =
  let _ : _ = job_queue, item, spec in
  let status, _inject =
    Bonsai.state_machine0
      ~default_model:{ Model.status = Loitering; queue = None }
      ~apply_action:(fun ctx model action ->
        let inject = Bonsai.Apply_action_context.inject ctx in
        let schedule_event = Bonsai.Apply_action_context.schedule_event ctx in
        let update_queue, status =
          match model.status, (action : (resource, spec) Action.t) with
          | Loitering, Activate { queue; item; spec } ->
            let on_start = inject Action.Start in
            let on_done = inject Action.Complete in
            schedule_event
              (let%bind.Effect id =
                 queue.Job_queue.push_back spec { on_start; on_done; resource = item }
               in
               inject (Set_job_id id));
            None, Status.Loitering
          | Working, Complete -> None, Loitering
          | Loitering, Set_job_id job_id -> None, In_queue job_id
          | In_queue _, Start -> None, Working
          (* bugs *)
          | In_queue job_id, Activate { queue; item = _; spec = _ } ->
            print_s [%message "BUG" [%here]];
            schedule_event (queue.Job_queue.remove job_id);
            None, Status.Loitering
          | In_queue job_id, Set_job_id new_job_id ->
            print_s [%message "BUG" [%here]];
            Option.iter model.queue ~f:(fun queue ->
              schedule_event (queue.Job_queue.remove job_id));
            None, Status.In_queue new_job_id
          | Loitering, Start ->
            print_s [%message "BUG" [%here]];
            None, Working
          | Working, Activate { queue; _ } ->
            print_s [%message "BUG" [%here]];
            Some queue, Working
          | Working, Start ->
            print_s [%message "BUG" [%here]];
            None, Working
          | Working, Set_job_id job_id ->
            print_s [%message "BUG" [%here]];
            Option.iter model.queue ~f:(fun queue ->
              schedule_event (queue.Job_queue.remove job_id));
            None, Working
          | In_queue job_id, Complete ->
            print_s [%message "BUG" [%here]];
            Option.iter model.queue ~f:(fun queue ->
              schedule_event (queue.Job_queue.remove job_id));
            None, Loitering
          | Loitering, Complete ->
            print_s [%message "BUG" [%here]];
            None, Loitering
        in
        match update_queue with
        | Some queue -> { Model.queue = Some queue; status }
        | None -> { model with status })
      graph
  in
  status
;;

let component
  (type resource spec)
  ~(job_queue : (resource Item.t, spec) Job_queue.t Bonsai.t)
  ~(item : resource Bonsai.t)
  ~(spec : spec option Bonsai.t)
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
  match%sub spec with
  | None -> return Status.Loitering
  | Some spec ->
    Bonsai.scope_model
      (module Spec)
      graph
      ~on:spec
      ~for_:(fun graph ->
        let iteration, incr_iteration =
          Bonsai.state_machine0
            ~default_model:0
            ~apply_action:(fun _ctx i () -> i + 1)
            graph
        in
        Bonsai.scope_model
          (module Int)
          graph
          ~on:iteration
          ~for_:(fun graph ->
            let status, set_status = Bonsai.state Status.Loitering graph in
            Bonsai.Edge.lifecycle
              ~on_activate:
                (let%arr job_queue = job_queue
                 and spec = spec
                 and status = status
                 and set_status = set_status
                 and incr_iteration = incr_iteration
                 and item = item in
                 let on_start = set_status Working in
                 let on_done = incr_iteration () in
                 match status with
                 | Loitering ->
                   let%bind.Effect id =
                     job_queue.Job_queue.push_back
                       spec
                       { Item.on_start; on_done; resource = item }
                   in
                   set_status (In_queue id)
                 | Working -> Effect.Ignore
                 | In_queue _ ->
                   print_s [%message "BUG" [%here]];
                   Effect.Ignore)
              graph;
            status))
;;
