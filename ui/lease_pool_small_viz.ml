open! Core
open! Bonsai_web
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

let component ~data_to_string ~pool =
  let%arr leased_out = Lease_pool.leased_out pool
  and available = Lease_pool.available pool
  and queued_jobs = Lease_pool.queued_jobs pool in
  let map =
    let combine ~key:_ a b =
      match a, b with
      | `Leased data, `Available _ -> `Leased data
      | `Available _, `Leased data -> `Leased data
      | `Leased data, `Leased _ -> `Leased data
      | `Available _, `Available data -> `Available data
    in
    Map.merge_skewed
      (Map.map leased_out ~f:(fun data -> `Leased data))
      (Map.map available ~f:(fun data -> `Available data))
      ~combine
  in
  let workers =
    Map.data map
    |> List.map ~f:(function
      | `Available data ->
        Vdom.Node.div ~attrs:[ Style.worker; Vdom.Attr.title (data_to_string data) ] []
      | `Leased data ->
        Vdom.Node.div
          ~attrs:[ Style.worker; Style.leased; Vdom.Attr.title (data_to_string data) ]
          [])
  in
  let queued_jobs =
    List.map queued_jobs ~f:(fun _ -> Vdom.Node.div ~attrs:[ Style.job ] [])
  in
  View.hbox ~gap:(`Px 10) (workers @ queued_jobs)
;;
