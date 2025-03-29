open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
.worker {
  width: 20px;
  height: 20px;
  border: 1px solid #ffffff94;
  border-radius: 5px;
  margin:5px;
}

.leased {
  background: green;
  background: #20592794; 
  border: 2px solid  #91df94;
}

.job {
  width: 20px;
  height: 20px;
  border-radius: 10px;
  background: green;
  background: #91df94;
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
  View.hbox ~cross_axis_alignment:Center (workers @ queued_jobs)
;;
