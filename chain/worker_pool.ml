open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type t =
  { enqueue : 'a. Sd.Models.t -> (Sd.Hosts.Host.t -> 'a Effect.t) -> 'a Effect.t
  ; debug : Vdom.Node.t
  }

let component ~(hosts : Sd.Hosts.Host.Set.t Bonsai.t) graph =
  let queue =
    Job_queue.pipe
      ~compare:Sd.Models.compare
      ~sexp_of_spec:Sd.Models.sexp_of_t
      ~sexp_of_a:(Worker.Item.sexp_of_t Sd.Hosts.Host.sexp_of_t)
      graph
  in
  let statuses =
    Bonsai.assoc_set
      (module Sd.Hosts.Host)
      hosts
      ~f:(fun host graph ->
        let current_model =
          Sd.Models.Current_model.current
            ~request_host:
              (let%arr host = host in
               Effect.return { Sd.Hosts.Work.host; f = (fun f -> f host) })
            graph
        in
        let current_model = current_model >>| Option.some in
        Worker.component
          ~spec_compare:Sd.Models.compare
          ~queue
          ~item:host
          ~spec:current_model
          graph)
      graph
  in
  let%arr statuses = statuses
  and queue = queue in
  let enqueue spec f =
    let%bind.Effect { Worker.Item.on_start; on_done; resource } =
      Job_queue.pop_front queue spec
    in
    let%bind.Effect () = on_start in
    let%bind.Effect result = f resource in
    let%bind.Effect () = on_done in
    Effect.return result
  in
  let debug =
    View.vbox
      [ Job_queue.debug queue
      ; Vdom.Node.sexp_for_debugging
          [%sexp (statuses : Worker.Status.t Sd.Hosts.Host.Map.t)]
      ]
  in
  { enqueue; debug }
;;
