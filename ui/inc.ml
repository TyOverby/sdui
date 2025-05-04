open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Or_error_or_stale = struct
  type 'a t =
    | Fresh of 'a
    | Stale of 'a
    | Error of Error.t
    | Not_computed

  let both a b =
    match a, b with
    | Fresh a, Fresh b -> Fresh (a, b)
    | Stale a, Fresh b | Fresh a, Stale b | Stale a, Stale b -> Stale (a, b)
    | Error e1, Error e2 -> Error (Error.of_list [ e1; e2 ])
    | _, Error e -> Error e
    | Error e, _ -> Error e
    | _, Not_computed | Not_computed, _ -> Not_computed
  ;;

  let map a ~f =
    match a with
    | Fresh a -> Fresh (f a)
    | Stale a -> Stale (f a)
    | Error e -> Error e
    | Not_computed -> Not_computed
  ;;

  let join t =
    match t with
    | Fresh (Fresh a) -> Fresh a
    | Fresh (Stale a) | Stale (Fresh a) | Stale (Stale a) -> Stale a
    | Error e | Fresh (Error e) | Stale (Error e) -> Error e
    | Not_computed | Fresh Not_computed | Stale Not_computed -> Not_computed
  ;;

  let bind t ~f = map t ~f |> join
  let unzip t = map t ~f:Tuple2.get1, map t ~f:Tuple2.get2

  let rec all = function
    | [] -> Fresh []
    | a :: rest -> map (both a (all rest)) ~f:(fun (a, rest) -> a :: rest)
  ;;
end

type 'a t = 'a Or_error_or_stale.t Bonsai.t

let of_bonsai ~equal ?time_to_stable a graph =
  let a = Bonsai.cutoff ~equal a in
  match time_to_stable with
  | None ->
    let%arr a in
    Or_error_or_stale.Fresh a
  | Some time_to_stable ->
    (match%arr Bonsai_extra.value_stability ~equal ~time_to_stable a graph with
     | Stable a -> Or_error_or_stale.Fresh a
     | Unstable { previously_stable = None; _ } -> Not_computed
     | Unstable { previously_stable = Some a; _ } -> Stale a)
;;

let of_or_error_bonsai ~equal ?time_to_stable a graph =
  let equal = Or_error.equal equal in
  let a = Bonsai.cutoff ~equal a in
  match time_to_stable with
  | None ->
    (match%arr a with
     | Ok a -> Or_error_or_stale.Fresh a
     | Error e -> Error e)
  | Some time_to_stable ->
    (match%arr Bonsai_extra.value_stability ~equal ~time_to_stable a graph with
     | Stable (Ok a) -> Or_error_or_stale.Fresh a
     | Stable (Error e) -> Error e
     | Unstable { previously_stable = None; _ } -> Not_computed
     | Unstable { previously_stable = Some (Ok a); _ } -> Stale a
     | Unstable { previously_stable = Some (Error e); _ } -> Error e)
;;

let map_pure a ~f =
  match%arr a with
  | Or_error_or_stale.Fresh a -> Or_error_or_stale.Fresh (f a)
  | Stale a -> Stale (f a)
  | Error e -> Error e
  | Not_computed -> Not_computed
;;

let optional a =
  match%arr a with
  | None -> Or_error_or_stale.Fresh None
  | Some (Or_error_or_stale.Fresh a) -> Fresh (Some a)
  | Some (Stale a) -> Stale (Some a)
  | Some (Error e) -> Error e
  | Some Not_computed -> Not_computed
;;

let map2_pure a b ~f =
  let a_and_b = Bonsai.both a b in
  match%arr a_and_b with
  | Or_error_or_stale.Fresh a, Or_error_or_stale.Fresh b ->
    Or_error_or_stale.Fresh (f a b)
  | Stale a, Fresh b | Fresh a, Stale b | Stale a, Stale b -> Stale (f a b)
  | Error e1, Error e2 -> Error (Error.of_list [ e1; e2 ])
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Not_computed, _ | _, Not_computed -> Not_computed
;;

let map ~equal a ~f graph =
  let module Id_gen = Bonsai_extra.Id_gen (Int63) () in
  let id_gen = Id_gen.component graph in
  let state, set_state =
    Bonsai.state_machine
      ~default_model:None
      graph
      ~apply_action:(fun _ model (id, input, done_, change) ->
        match model with
        | Some (old_id, _, true, _) when Id_gen.(old_id = id) -> model
        | Some (old_id, _, _, _) when Id_gen.(old_id > id) -> model
        | Some (old_id, _, _, _) when Id_gen.(old_id < id) ->
          Some (id, input, done_, change None)
        | Some (_, _, _, old_result) -> Some (id, input, done_, change (Some old_result))
        | None -> Some (id, input, done_, change None))
  in
  let needs_recalc =
    let%arr a and state in
    match (a : _ Or_error_or_stale.t), state with
    | Fresh a, None -> Some a
    | Fresh a, Some (_, old, _, _) when not (equal a old) -> Some a
    | _ -> None
  in
  let _ : unit Bonsai.t =
    match%sub needs_recalc with
    | Some a ->
      Bonsai.Edge.on_change
        a
        ~equal
        ~callback:
          (let%arr f and set_state and id_gen in
           fun a ->
             let%bind.Effect id = id_gen in
             let update f = set_state (id, a, false, f) in
             let%bind.Effect r = f ~update a in
             set_state (id, a, true, fun _ -> r))
        graph;
      Bonsai.return ()
    | None -> Bonsai.return ()
  in
  let%arr needs_recalc and state and a in
  match needs_recalc, a, state with
  | None, Fresh _, Some (_, _, true, out) -> Or_error_or_stale.Fresh out
  | None, _, Some (_, _, _, out) -> Stale out
  | Some _, _, None | None, _, None -> Not_computed
  | Some _, _, Some (_, _, _, out) -> Stale out
;;

let map2 ~equal_a ~equal_b a b ~f graph =
  let a_and_b =
    let%arr a and b in
    match a, b with
    | Or_error_or_stale.Fresh a, Or_error_or_stale.Fresh b ->
      Or_error_or_stale.Fresh (a, b)
    | Error e1, Error e2 -> Error (Error.of_list [ e1; e2 ])
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Not_computed, _ | _, Not_computed -> Not_computed
    | Stale a, Stale b | Fresh a, Stale b | Stale a, Fresh b -> Stale (a, b)
  in
  let f =
    let%arr f in
    fun ~update (a, b) -> f ~update a b
  in
  let equal = Tuple2.equal ~eq1:equal_a ~eq2:equal_b in
  map ~equal a_and_b ~f graph
;;

let map3 ~equal_a ~equal_b ~equal_c a b c ~f graph =
  let a_and_b_and_c =
    let%arr a and b and c in
    match a, b, c with
    | Or_error_or_stale.Fresh a, Or_error_or_stale.Fresh b, Or_error_or_stale.Fresh c ->
      Or_error_or_stale.Fresh (a, b, c)
    | Error e1, Error e2, Error e3 -> Error (Error.of_list [ e1; e2; e3 ])
    | Error e1, Error e2, _ -> Error (Error.of_list [ e1; e2 ])
    | Error e1, _, Error e2 -> Error (Error.of_list [ e1; e2 ])
    | _, Error e1, Error e2 -> Error (Error.of_list [ e1; e2 ])
    | Error e, _, _ -> Error e
    | _, Error e, _ -> Error e
    | _, _, Error e -> Error e
    | Not_computed, _, _ | _, Not_computed, _ | _, _, Not_computed -> Not_computed
    | Stale a, Stale b, Stale c
    | Fresh a, Stale b, Stale c
    | Stale a, Fresh b, Stale c
    | Stale a, Stale b, Fresh c
    | Fresh a, Stale b, Fresh c
    | Stale a, Fresh b, Fresh c
    | Fresh a, Fresh b, Stale c -> Stale (a, b, c)
  in
  let f =
    let%arr f in
    fun ~update (a, b, c) -> f ~update a b c
  in
  let equal = Tuple3.equal ~eq1:equal_a ~eq2:equal_b ~eq3:equal_c in
  map ~equal a_and_b_and_c ~f graph
;;

let collapse_error t =
  match%arr t with
  | Or_error_or_stale.Fresh (Ok a) -> Or_error_or_stale.Fresh a
  | Fresh (Error a) -> Error a
  | Stale (Ok a) -> Stale a
  | Stale (Error a) -> Error a
  | Not_computed -> Not_computed
  | Error e -> Error e
;;
