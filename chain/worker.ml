open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Item = struct
  type 'resource t =
    { on_start : unit Effect.t
    ; on_done : unit Effect.t
    ; resource : 'resource
    }

  let sexp_of_t sexp_of_resource { resource; _ } = sexp_of_resource resource
end

module Status = struct
  type t =
    | Loitering
    | In_queue of Job_queue.Item_id.t
    | Working
  [@@deriving sexp_of]
end

let component
  (type resource spec)
  ~(queue : (resource Item.t, spec) Job_queue.t Bonsai.t)
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
            Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
              let status, set_status = Bonsai.state Status.Loitering graph in
              Bonsai.Edge.lifecycle
                graph
                ~on_deactivate:
                  (let%arr queue = queue
                   and reset = reset
                   and status = status in
                   let remove_effect_opt =
                     match status with
                     | In_queue id -> Job_queue.remove queue id
                     | _ -> Effect.Ignore
                   in
                   Effect.Many [ reset; remove_effect_opt ])
                ~on_activate:
                  (let%arr queue = queue
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
                       Job_queue.push_back
                         queue
                         spec
                         { Item.on_start; on_done; resource = item }
                     in
                     set_status (In_queue id)
                   | Working -> Effect.Ignore
                   | In_queue _ -> Effect.Ignore);
              status)))
;;
