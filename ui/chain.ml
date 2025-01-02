open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let img2img_impl ~direction ~pool ~prev ~mask ~prev_params graph =
  let t = Single.component ~direction ~pool ~prev ~mask graph in
  let%sub () =
    match%sub prev_params with
    | Ok prev_params ->
      let _ = Form.Dynamic.with_default prev_params t.form graph in
      return ()
    | Error _ -> return ()
  in
  t
;;

let sketch_impl ~prev ~prev_params graph =
  let%sub image, view =
    match%sub prev with
    | Inc.Or_error_or_stale.Fresh prev_for_paint | Stale prev_for_paint ->
      let { Paint.images = t; view; _ } = Paint.component ~prev:prev_for_paint graph in
      let%arr t and view in
      t, Some view
    | Error e ->
      let%arr e in
      Inc.Or_error_or_stale.Error e, None
    | Not_computed -> Bonsai.return (Inc.Or_error_or_stale.Not_computed, None)
  in
  let%arr t = image
  and view
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
  ~(prev_params : (Parameters.t, Error.t) result Bonsai.t)
  ~(reset : unit Ui_effect.t Bonsai.t)
  graph
  =
  let image, view, params =
    let%sub image, mask, sketch_view, _ = sketch_impl ~prev ~prev_params graph in
    let { Single.image; gallery_view; form_view; form } =
      img2img_impl
        ~direction:`Vertical
        ~pool
        ~prev:(image >>| Option.some)
        ~mask
        ~prev_params
        graph
    in
    let view =
      match%sub sketch_view with
      | None -> return Workspace.empty
      | Some
          { color_picker
          ; pen_size_slider
          ; layer_panel
          ; forward_button
          ; clear_button
          ; widget
          ; padding
          } ->
        Workspace.make
          ~index
          ~color_picker
          ~pen_size_slider
          ~layer_panel
          ~forward_button
          ~clear_button
          ~widget
          ~gallery_view
          ~form_view
          ~padding
          ~reset
          graph
    in
    image, view, form >>| Form.value
  in
  image, view, params
;;

let do_txt2img ~prev ~prev_params ~index ~pool ~reset ~recurse graph =
  let path = Bonsai.path graph in
  let image, view, params = component ~index ~pool ~prev ~reset ~prev_params graph in
  let next = recurse index image params graph in
  let%arr view
  and image, route, view2 = next
  and path in
  ( image
  , Route.branch ~key:path ~data:() ~children:[ route ]
  , Map.set view2 ~key:path ~data:view )
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
            let%arr prev in
            prev, Route.empty, Bonsai.Path.Map.empty)))
;;
