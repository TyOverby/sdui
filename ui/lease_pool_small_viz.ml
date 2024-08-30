open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
.worker {
  width: 50px;
  height: 50px;
  border: 2px solid white;
  border-radius: 10px;
}

.leased {
  background: green;
}

.job {
  width: 50px;
  height: 50px;
  border-radius: 20px;
  background: green;
}

|}]

let component ~pool =
  let%arr leased_out = Lease_pool.leased_out pool
  and available = Lease_pool.available pool
  and queued_jobs = Lease_pool.queued_jobs pool in
  let map =
    let combine ~key:_ a b =
      match a, b with
      | `Leased, `Available -> `Leased
      | `Available, `Leased -> `Leased
      | `Leased, `Leased -> `Leased
      | `Available, `Available -> `Available
    in
    Map.merge_skewed
      (Set.to_map leased_out ~f:(fun _ -> `Leased))
      (Set.to_map available ~f:(fun _ -> `Available))
      ~combine
  in
  let workers =
    Map.data map
    |> List.map ~f:(function
      | `Available -> Vdom.Node.div ~attrs:[ Style.worker ] []
      | `Leased -> Vdom.Node.div ~attrs:[ Style.worker; Style.leased ] [])
  in
  let queued_jobs =
    List.map queued_jobs ~f:(fun _ -> Vdom.Node.div ~attrs:[ Style.job ] [])
  in
  View.hbox ~gap:(`Px 10) (workers @ queued_jobs)
;;
