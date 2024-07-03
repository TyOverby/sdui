open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

type t =
  { enqueue :
      'a 'b.
      ?sexp_of:('a -> Sexp.t)
      -> spec:Sd.Models.t
      -> f:(Sd.Hosts.Host.t -> 'a -> 'b Effect.t)
      -> 'a
      -> 'b Effect.t
  ; debug : Vdom.Node.t
  }

let enqueue t = t.enqueue
let debug t = t.debug

let component ~(hosts : Sd.Hosts.Host.Set.t Bonsai.t) graph =
  let queue =
    Job_queue.pipe
      ~compare:Sd.Models.compare
      ~sexp_of_spec:Sd.Models.sexp_of_t
      ~sexp_of_a:[%sexp_of: (Sd.Hosts.Host.t, Sd.Models.t) Worker2.Job.t]
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
        match%sub current_model >>| Option.some with
        | None -> return Worker2.Status.loitering_or_inactive
        | Some current_model ->
          Worker2.component
            ~spec_compare:Sd.Models.compare
            ~queue
            ~resource:host
            ~spec:current_model
            graph)
      graph
  in
  let%arr statuses = statuses
  and queue = queue in
  let enqueue ?sexp_of ~spec ~f arg =
    Worker2.Job.create
      ?sexp_of_arg:sexp_of
      ~dispatch:(fun _spec resource arg -> f resource arg)
      ~queue
      spec
      arg
  in
  let debug =
    View.vbox
      [ Job_queue.debug queue
      ; Vdom.Node.sexp_for_debugging
          [%sexp (statuses : Worker2.Status.t Sd.Hosts.Host.Map.t)]
      ]
  in
  { enqueue; debug }
;;
