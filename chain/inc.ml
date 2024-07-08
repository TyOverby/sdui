open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Or_stale = struct
  type 'a t =
    | Fresh of 'a
    | Stale of 'a
    | Not_computed
end

type 'a t = 'a Or_stale.t Bonsai.t

let of_bonsai ~equal ?time_to_stable a graph =
  let a = Bonsai.cutoff ~equal a in
  match time_to_stable with
  | None ->
    let%arr a = a in
    Or_stale.Fresh a
  | Some time_to_stable ->
    (match%arr Bonsai_extra.value_stability ~equal ~time_to_stable a graph with
     | Stable a -> Or_stale.Fresh a
     | Unstable { previously_stable = None; _ } -> Not_computed
     | Unstable { previously_stable = Some a; _ } -> Stale a)
;;

let map ~equal a ~f graph =
  let module Id_gen = Bonsai_extra.Id_gen (Int63) () in
  let id_gen = Id_gen.component graph in
  let state, set_state =
    Bonsai.state_machine0
      ~default_model:None
      graph
      ~apply_action:(fun _ model (id, input, result) ->
        match model with
        | Some (old_id, _, _) when Id_gen.(old_id > id) -> model
        | _ -> Some (id, input, result))
  in
  let needs_recalc =
    let%arr a = a
    and state = state in
    match (a : _ Or_stale.t), state with
    | Fresh a, None -> Some a
    | Fresh a, Some (_, old, _) when not (equal a old) -> Some a
    | _ -> None
  in
  let _ : unit Bonsai.t =
    match%sub needs_recalc with
    | Some a ->
      Bonsai.Edge.on_change
        a
        ~equal
        ~callback:
          (let%arr f = f
           and set_state = set_state
           and id_gen = id_gen in
           fun a ->
             let%bind.Effect id = id_gen in
             let%bind.Effect r = f a in
             set_state (id, a, r))
        graph;
      Bonsai.return ()
    | None -> Bonsai.return ()
  in
  let%arr needs_recalc = needs_recalc
  and state = state in
  match needs_recalc, state with
  | None, Some (_, _, out) -> Or_stale.Fresh out
  | Some _, Some (_, _, out) -> Stale out
  | Some _, None | None, None -> Not_computed
;;

let map2 ~equal_a ~equal_b a b ~f graph =
  let a_and_b =
    let%arr a = a
    and b = b in
    match a, b with
    | Or_stale.Fresh a, Or_stale.Fresh b -> Or_stale.Fresh (a, b)
    | Not_computed, _ | _, Not_computed -> Not_computed
    | Stale a, Stale b | Fresh a, Stale b | Stale a, Fresh b -> Stale (a, b)
  in
  let f =
    let%arr f = f in
    fun (a, b) -> f a b
  in
  let equal = Tuple2.equal ~eq1:equal_a ~eq2:equal_b in
  map ~equal a_and_b ~f graph
;;
