open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let paint_impl ~prev graph =
  match%sub prev with
  | Inc.Or_error_or_stale.Fresh prev_for_paint | Stale prev_for_paint ->
    let t, view = Paint.component ~prev:prev_for_paint graph in
    let%arr t = t
    and view = view in
    t, view
  | Error e ->
    let%arr e = e in
    Inc.Or_error_or_stale.Error e, Vdom.Node.textf "Error: %s" (Error.to_string_hum e)
  | Not_computed ->
    Bonsai.return (Inc.Or_error_or_stale.Not_computed, Vdom.Node.text "not computed")
;;

let img2img_impl ~pool ~prev ~mask ~prev_params graph =
  let image, view, params = Single.component ~pool ~prev ~mask ~default_size:512 graph in
  let%sub () =
    match%sub prev_params with
    | Ok prev_params ->
      let _ = Form.Dynamic.with_default prev_params params graph in
      return ()
    | Error _ -> return ()
  in
  let%arr image = image
  and view = view
  and params = params in
  image, view, Form.value params
;;

let sketch_impl ~prev ~prev_params graph =
  let%sub image, view = paint_impl ~prev graph in
  let%arr t = image
  and view = view
  and params = prev_params in
  let image, mask =
    Inc.Or_error_or_stale.(unzip (map t ~f:(fun { image; mask } -> image, mask)))
  in
  let mask =
    match mask with
    | Fresh None -> None
    | Fresh (Some a) -> Some (Inc.Or_error_or_stale.Fresh a)
    | Stale None -> None
    | Stale (Some a) -> Some (Stale a)
    | Error e -> Some (Error e)
    | Not_computed -> None
  in
  image, mask, view, params
;;

let component
  ~(index : int Bonsai.t)
  ~(pool : (Sd.Hosts.Host.t, 'a, 'b) Lease_pool.t)
  ~(prev : Sd.Image.t Inc.Or_error_or_stale.t Bonsai.t)
  ~(prev_params : (Single.Parameters.t, Error.t) result Bonsai.t)
  ~(reset : unit Ui_effect.t Bonsai.t)
  graph
  =
  let image, view, params =
    let%sub image, mask, sketch_view, _ = sketch_impl ~prev ~prev_params graph in
    let%sub image, img2img_view, params =
      img2img_impl ~pool ~prev:(image >>| Option.some) ~mask ~prev_params graph
    in
    let view =
      let%arr sketch_view = sketch_view
      and params_form, gallery = img2img_view in
      View.hbox [ sketch_view; View.vbox [ params_form; gallery ] ]
    in
    image, view, params
  in
  let view =
    let%arr view = view
    and theme = View.Theme.current graph
    and reset = reset
    and index = index in
    let header =
      Vdom.Node.h2
        ~attrs:[ {%css| margin:0; |} ]
        [ Vdom.Node.textf "#%d " index; View.button theme "x" ~on_click:reset ]
    in
    Vdom.Node.div [ header; view ]
  in
  image, view, params
;;

let do_txt2img ~prev ~prev_params ~index ~pool ~reset ~recurse graph =
  let image, view, params = component ~index ~pool ~prev ~reset ~prev_params graph in
  let next = recurse index image params graph in
  let%arr view = view
  and image, view2 = next in
  image, View.vbox ~gap:(`Em 1) [ view; view2 ]
;;

let _fix4 a b c d ~f graph =
  let abcd = Bonsai.map4 a b c d ~f:(fun a b c d -> a, b, c, d) in
  Bonsai.fix abcd graph ~f:(fun ~recurse abcd graph ->
    let%sub a, b, c, d = abcd in
    let recurse a b c d graph =
      let abcd = Bonsai.map4 a b c d ~f:(fun a b c d -> a, b, c, d) in
      recurse abcd graph
    in
    f ~recurse a b c d graph)
;;

let fix3 a b c ~f graph =
  let abc = Bonsai.map3 a b c ~f:(fun a b c -> a, b, c) in
  Bonsai.fix abc graph ~f:(fun ~recurse abc graph ->
    let%sub a, b, c = abc in
    let recurse a b c graph =
      let abc = Bonsai.map3 a b c ~f:(fun a b c -> a, b, c) in
      recurse abc graph
    in
    f ~recurse a b c graph)
;;

let component ~pool ~index ~prev ~prev_params graph =
  fix3 index prev prev_params graph ~f:(fun ~recurse index prev prev_params graph ->
    let index = index >>| ( + ) 1 in
    let scope, incr_scope =
      Bonsai.state_machine0
        ~default_model:0
        ~apply_action:(fun _ model () -> model + 1)
        graph
    in
    Bonsai.scope_model
      (module Int)
      graph
      ~on:scope
      ~for_:(fun graph ->
        Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
          let reset =
            Bonsai.map2 reset incr_scope ~f:(fun reset incr_scope ->
              Effect.Many [ reset; incr_scope () ])
          in
          match%sub prev with
          | Inc.Or_error_or_stale.Fresh _ | Stale _ ->
            do_txt2img ~prev ~prev_params ~index ~pool ~reset ~recurse graph
          | _ ->
            let%arr prev = prev in
            prev, Vdom.Node.none)))
;;

let component ~pool graph =
  let%sub image, view, params =
    img2img_impl
      ~pool
      ~prev:(Bonsai.return None)
      ~prev_params:(Bonsai.return (Error (Error.of_string "no previous params")))
      ~mask:(Bonsai.return None)
      graph
  in
  let rest =
    component ~pool ~index:(Bonsai.return 1) ~prev:image ~prev_params:params graph
  in
  let%arr params, gallery = view
  and _, rest = rest in
  View.vbox [ View.vbox [ params; gallery ]; rest ]
;;
