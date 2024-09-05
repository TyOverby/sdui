open! Core
open! Bonsai_web
open Bonsai.Let_syntax

type t =
  { path_id : string
  ; generation_id : int
  }
[@@deriving sexp, equal, hash, compare]

let var = Ui_incr.Var.create Bonsai.Path.Map.empty
let incr = Ui_incr.Var.watch var
let lookup = Ui_incr.Map.Lookup.create (module Bonsai.Path) incr

let find path =
  let open Ui_incr.Let_syntax in
  (* paths are always constant nodes, so this bind will only fire once *)
  let%bind path = path in
  let%map generation = Ui_incr.Map.Lookup.find lookup path in
  Option.value generation ~default:(-1)
;;

let bump_generation =
  let f path =
    Ui_incr.Var.replace var ~f:(fun map ->
      Map.update map path ~f:(function
        | None -> 0
        | Some a -> a + 1))
  in
  Effect.of_sync_fun f
;;

let remove =
  let f path = Ui_incr.Var.replace var ~f:(fun map -> Map.remove map path) in
  Effect.of_sync_fun f
;;

let model_resetter_generation graph =
  let path = Bonsai.path graph in
  let generation = Bonsai.Incr.compute path ~f:find graph in
  let () =
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: Unit.t]
      ~equal:[%equal: Unit.t]
      (Bonsai.return ())
      ~callback:
        (* this callback will only get called in two scenarios:
           1. when the component becomes active for the first time
           2. when its model is cleared *)
        (let%map path = path in
         fun () -> bump_generation path)
      graph
  in
  let () = Bonsai.Edge.lifecycle ~on_deactivate:(Bonsai.map path ~f:remove) graph in
  let path_id = Bonsai.map ~f:Bonsai.Path.to_unique_identifier_string path in
  let to_return =
    let%arr path_id = path_id
    and generation_id = generation in
    { path_id; generation_id }
  in
  Bonsai.Incr.value_cutoff to_return ~equal graph
;;

let distinct a b = not (equal a b)
