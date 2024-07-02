open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Item_id = Unique_id.Int63 ()

type ('a, 'spec) t =
  { push_back : 'spec -> 'a -> Item_id.t Effect.t
  ; pop_front : 'spec -> 'a Effect.t
  ; remove : Item_id.t -> unit Effect.t
  ; debug : Sexp.t Lazy.t
  }

let pipe
  (type a spec)
  ?(sexp_of_a = sexp_of_opaque)
  ?(sexp_of_spec = sexp_of_opaque)
  ~compare
  graph
  =
  let module Spec = struct
    module T = struct
      module T' = struct
        type t = spec

        let sexp_of_t = sexp_of_spec
        let compare = compare
      end

      include T'
      include Comparator.Make (T')
    end

    include T
    include Comparable.Make_plain (T)
  end
  in
  let module Callback = struct
    type t = (unit, a) Effect.Private.Callback.t

    let sexp_of_t _ = Sexp.Atom "<callback>"
  end
  in
  let module Model = struct
    type t =
      { queued_items : (a * Item_id.t) Fdeque.t Spec.Map.t
      ; queued_receivers : Callback.t Fdeque.t Spec.Map.t
      }
    [@@deriving sexp_of]

    let equal = phys_equal
    let default = { queued_items = Spec.Map.empty; queued_receivers = Spec.Map.empty }
  end
  in
  let module Action = struct
    type t =
      | Add_item of
          { spec : Spec.t
          ; item : a
          ; id : Item_id.t
          }
      | Add_receiver of Spec.t * Callback.t
      | Clear of Item_id.t
    [@@deriving sexp_of]
  end
  in
  let model, inject =
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model:Model.default
      ~apply_action:(fun context model ->
        function
        | Clear item_id ->
          let queued_items =
            Map.map model.queued_items ~f:(fun deque ->
              Fdeque.to_list deque
              |> List.filter ~f:(fun (_, item_id') ->
                not (Item_id.equal item_id item_id'))
              |> Fdeque.of_list)
          in
          { model with queued_items }
        | Add_item { spec; item = a; id } ->
          let queued_receivers =
            Map.find model.queued_receivers spec |> Option.value ~default:Fdeque.empty
          in
          (match Fdeque.dequeue_front queued_receivers with
           | None ->
             let queued_items =
               Map.find model.queued_items spec
               |> Option.value ~default:Fdeque.empty
               |> Fn.flip Fdeque.enqueue_back (a, id)
             in
             { model with
               queued_items = Map.set model.queued_items ~key:spec ~data:queued_items
             }
           | Some (hd, queued_receivers) ->
             Bonsai.Apply_action_context.schedule_event
               context
               (Effect.Private.Callback.respond_to hd a);
             { model with
               queued_receivers =
                 Map.set model.queued_receivers ~key:spec ~data:queued_receivers
             })
        | Add_receiver (spec, r) ->
          let queued_items =
            Map.find model.queued_items spec |> Option.value ~default:Fdeque.empty
          in
          (match Fdeque.dequeue_front queued_items with
           | None ->
             let queued_receivers =
               Map.find model.queued_receivers spec |> Option.value ~default:Fdeque.empty
             in
             let queued_receivers = Fdeque.enqueue_back queued_receivers r in
             { model with
               queued_receivers =
                 Map.set model.queued_receivers ~key:spec ~data:queued_receivers
             }
           | Some ((item, _id), queued_items) ->
             Bonsai.Apply_action_context.schedule_event
               context
               (Effect.Private.Callback.respond_to r item);
             { model with
               queued_items = Map.set model.queued_items ~key:spec ~data:queued_items
             }))
  in
  let triple =
    let%arr inject = inject in
    let push_back spec item =
      let%bind.Effect id = Effect.of_thunk Item_id.create in
      let%bind.Effect () = inject (Add_item { spec; item; id }) in
      Effect.return id
    in
    let pop_front spec =
      Effect.Private.make ~request:() ~evaluator:(fun r ->
        Effect.Expert.handle_non_dom_event_exn (inject (Add_receiver (spec, r))))
    in
    let remove id = inject (Clear id) in
    { push_back; pop_front; remove; debug = Lazy.from_val (Sexp.Atom "") }
  in
  let%arr triple = triple
  and model = model in
  { triple with debug = lazy (Model.sexp_of_t model) }
;;
