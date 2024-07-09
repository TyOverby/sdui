open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let component ~pool ~prev ~reset graph =
  let image, single = Single.component ~pool ~prev ~default_size:512 graph in
  let image2, single2 =
    Single.component ~pool ~prev:(image >>| Option.some) ~default_size:1024 graph
  in
  let view =
    let%arr single = single
    and single2 = single2
    and theme = View.Theme.current graph
    and reset = reset in
    View.card'
      theme
      ~container_attrs:[{%css| margin: 1em; |}]
      ~title:[ View.button theme "x" ~on_click:reset ]
      [ View.hbox ~gap:(`Em 1) ~main_axis_alignment:Space_around [ single; single2 ] ]
  in
  image2, view
;;

let component ~pool ~prev graph =
  Bonsai.fix prev graph ~f:(fun ~recurse prev graph ->
    Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
      let active, toggle_active = Bonsai.toggle ~default_model:false graph in
      match%sub active with
      | false ->
        let%arr toggle_active = toggle_active
        and theme = View.Theme.current graph in
        None, Vdom.Node.div ~attrs:[{%css| font-size: 3em !important; display: flex; justify-content: center; margin: 1em; |}] [View.button theme "+" ~on_click:toggle_active]
      | true ->
        let image, view = component ~pool ~prev ~reset graph in
        let next = recurse (image >>| Option.some) graph in
        let%arr view = view
        and image = image
        and _, view2 = next in
        Some image, View.vbox ~gap:(`Em 1) [ view; view2 ]))
;;
