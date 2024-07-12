open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let component ~index ~pool ~prev ~reset graph =
  let image, single = Single.component ~pool ~prev ~default_size:512 graph in
  let view =
    let%arr single = single
    and theme = View.Theme.current graph
    and reset = reset
    and index = index
    and prev = prev in
    let header =
      let text =
        match prev with
        | None -> "txt -> img"
        | Some _ -> "img -> img"
      in
      Vdom.Node.h2 ~attrs:[{%css| margin:0; |}]
        [ Vdom.Node.textf "#%d : %s" index text; View.button theme "x" ~on_click:reset ]
    in
    Vdom.Node.div [ header; single ]
  in
  image, view
;;

let component ~pool ~index ~prev graph =
  Bonsai.fix2 index prev graph ~f:(fun ~recurse index prev graph ->
    let index = index >>| ( + ) 1 in
    Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
      let active, toggle_active = Bonsai.toggle ~default_model:false graph in
      match%sub active with
      | false ->
        let%arr toggle_active = toggle_active
        and prev = prev
        and theme = View.Theme.current graph in
        ( prev
        , Vdom.Node.div
            ~attrs:
              [ {%css| font-size: 3em !important; display: flex; justify-content: center; margin: 1em; |}
              ]
            [ View.button theme "+" ~on_click:toggle_active ] )
      | true ->
        let image, view = component ~index ~pool ~prev ~reset graph in
        let next = recurse index (image >>| Option.some) graph in
        let%arr view = view
        and image, view2 = next in
        image, View.vbox ~gap:(`Em 1) [ view; view2 ]))
;;

let component ~pool ~prev graph = component ~pool ~index:(Bonsai.return 0) ~prev graph
