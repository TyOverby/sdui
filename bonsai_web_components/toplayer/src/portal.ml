open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Expert = Vdom_toplayer.For_bonsai_web_ui_toplayer.Portal

let portals = Bonsai.Expert.Var.create String.Map.empty

let get_portal_root () : Dom_html.element Js.t =
  match am_running_how with
  | `Browser -> Dom_html.document##.documentElement
  | `Browser_benchmark | `Node | `Node_benchmark | `Node_test -> Js.Unsafe.obj [||]
;;

let apply_action_sync ~path action =
  Bonsai.Expert.Var.update portals ~f:(fun portals ->
    match action with
    | `Create vdom ->
      Map.update portals path ~f:(fun existing ->
        Option.iter existing ~f:Expert.destroy;
        Expert.create (get_portal_root ()) vdom)
    | `Apply_patch vdom ->
      Map.change
        portals
        path
        ~f:(Option.map ~f:(fun portal -> Expert.apply_patch portal vdom))
    | `Destroy ->
      (match Map.find portals path with
       | Some portal ->
         Expert.destroy portal;
         Map.remove portals path
       | None -> portals))
;;

let apply_action ~path action = Effect.of_sync_fun (apply_action_sync ~path) action

let bonsai_driven vdom graph =
  let path = Bonsai.path_id graph in
  let () =
    (* Create a new portal whenever the component is activated. *)
    let on_activate =
      let%arr vdom = vdom
      and path = path in
      apply_action ~path (`Create vdom)
    in
    (* Destroy any existing portal whenever the component is deactivated. *)
    let on_deactivate =
      let%arr path = path in
      apply_action ~path `Destroy
    in
    Bonsai.Edge.lifecycle ~on_activate ~on_deactivate graph
  in
  (* Patch the portal contents whenever they change. *)
  let callback =
    let%arr path = path in
    fun vdom -> apply_action ~path (`Apply_patch vdom)
  in
  Bonsai.Edge.on_change vdom ~equal:phys_equal ~callback graph
;;

module For_testing = struct
  let portals = portals
end
