open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Ee = Effect.Define1 (struct
    module Action = struct
      type 'a t = ('a -> unit Ui_effect.t) -> unit Ui_effect.t
    end

    let handle action ~on_response =
      on_response
      |> Effect.of_sync_fun
      |> action
      |> Effect.Expert.handle_non_dom_event_exn
    ;;
  end)

let with_yoink f =
  Bonsai.wrap
    ()
    ~default_model:()
    ~apply_action:(fun ctx result () cb ->
      Bonsai.Apply_action_context.schedule_event ctx (cb result))
    ~f:(fun _model inject ->
      let%sub result = Bonsai.pure Ee.inject inject in
      f result)
;;

module Path = Bonsai.Private.Path

let bonsai_map_map modul map ~f =
  Bonsai.assoc modul map ~f:(fun _ v ->
    let%arr v = v in
    f v)
;;

let bonsai_map_mapi modul map ~f =
  Bonsai.assoc modul map ~f:(fun key data ->
    let%arr key = key
    and data = data in
    f ~key ~data)
;;

let assoc_into_path_map modul map ~f =
  let%sub r =
    Bonsai.assoc modul map ~f:(fun key data ->
      with_yoink (fun get_path ->
        let%sub get_path =
          let%arr get_path = get_path in
          let%map.Effect path, _ = get_path in
          path
        in
        let%sub r = f ~key ~my_path:get_path ~data in
        let%sub path = Bonsai.Private.path in
        let%arr path = path
        and r = r in
        path, r))
  in
  let%sub r' =
    Bonsai.Map.rekey r ~comparator:(module Path) ~f:(fun ~key:_ ~data:(path, _) -> path)
  in
  let%sub r'' = bonsai_map_map (module Path) r' ~f:snd in
  let%arr r = r
  and r'' = r'' in
  r, r''
;;

let multi_merge modul map ~f =
  let%sub result_map, gathered_map =
    assoc_into_path_map modul map ~f:(fun ~key ~my_path ~data -> f ~key ~my_path ~data)
  in
  let%sub result_map = bonsai_map_map modul result_map ~f:(fun (_path, (r, _)) -> r) in
  let%sub gathered_map =
    bonsai_map_mapi
      (module Path)
      gathered_map
      ~f:(fun ~key ~data:(data, map) -> Map.set map ~key ~data)
  in
  let%sub gathered_map =
    Bonsai.Map.collapse_by
      gathered_map
      ~comparator:(module Path)
      ~merge_keys:(fun _p1 p2 -> p2)
  in
  let%arr result_map = result_map
  and gathered_map = gathered_map in
  result_map, gathered_map
;;

type 'a t =
  { view : Vdom.Node.t
  ; value : 'a
  }

let tree f =
  let%sub results, _ =
    with_yoink (fun get_results_and_all ->
      let%sub get_all =
        let%arr get_results_and_all = get_results_and_all in
        let%map.Effect _results, all = get_results_and_all in
        Map.map all ~f:(fun { value; view = _ } -> value)
      in
      f ~get_all)
  in
  return results
;;

let rec alist_group_add ~equal ~key ~data = function
  | [] -> [ key, [ data ] ]
  | (k, v) :: rest when equal key k -> (k, v @ [ data ]) :: rest
  | hd :: rest -> hd :: alist_group_add rest ~equal ~key ~data
;;

let alist_group ~equal list =
  List.fold list ~init:[] ~f:(fun acc (key, data) ->
    alist_group_add ~equal ~key ~data acc)
;;
