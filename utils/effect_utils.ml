open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let parallel_both a b =
  Effect.Private.make ~request:() ~evaluator:(fun cb ->
    let a_res = ref None
    and b_res = ref None in
    let maybe_finalize () =
      match !a_res, !b_res with
      | Some a, Some b ->
        Effect.Expert.handle_non_dom_event_exn
          (Effect.Private.Callback.respond_to cb (a, b))
      | _ -> ()
    in
    Effect.Expert.handle_non_dom_event_exn
      (let%map.Effect a = a in
       a_res := Some a;
       maybe_finalize ());
    Effect.Expert.handle_non_dom_event_exn
      (let%map.Effect b = b in
       b_res := Some b;
       maybe_finalize ()))
;;

let rec parallel_all = function
  | [] -> Effect.return []
  | a :: rest ->
    let%map.Effect a, rest = parallel_both a (parallel_all rest) in
    a :: rest
;;

let parallel_n
  (type a)
  ~(update : (a Or_error.t Int.Map.t option -> a Or_error.t Int.Map.t) -> unit Effect.t)
  n
  ~(f : int -> a Or_error.t Effect.t)
  =
  List.init n ~f:(fun i ->
    let%bind.Effect r = f i in
    let%bind.Effect () =
      update (fun state ->
        match state with
        | None -> Int.Map.singleton i r
        | Some map -> Map.set map ~key:i ~data:r)
    in
    Effect.return r)
  |> parallel_all
  |> Effect.map ~f:(fun all -> List.mapi all ~f:Tuple2.create |> Int.Map.of_alist_exn)
;;

let while_running eff ~do_this =
  Ui_effect.Expert.of_fun ~f:(fun ~callback ->
    let finished = ref false in
    Ui_effect.Expert.eval eff ~f:(fun result ->
      finished := true;
      callback result);
    let rec loop () =
      if !finished then () else Ui_effect.Expert.eval do_this ~f:(fun () -> loop ())
    in
    loop ())
;;
