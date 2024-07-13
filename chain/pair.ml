open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Which = struct
  type t =
    | Txt2img
    | Sketch
end

let component ~index ~which ~pool ~prev ~reset graph =
  let%sub image, view =
    match%sub which, prev with
    | Which.Txt2img, prev ->
      let image, view = Single.component ~pool ~prev ~default_size:512 graph in
      Bonsai.both image view
    | Sketch, Some prev ->
      let image, view = Paint.multi ~prev graph in
      Bonsai.both image view
    | Sketch, None ->
      Bonsai.return
        ( Inc.Or_error_or_stale.Error
            (Error.of_string "sketch can't be the first item in the chain")
        , Vdom.Node.text "sketch can't be the first item in the chain" )
  in
  let view =
    let%arr view = view
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
      Vdom.Node.h2
        ~attrs:[ {%css| margin:0; |} ]
        [ Vdom.Node.textf "#%d : %s" index text; View.button theme "x" ~on_click:reset ]
    in
    Vdom.Node.div [ header; view ]
  in
  image, view
;;

let do_txt2img ~which ~prev ~index ~pool ~reset ~recurse graph =
  let image, view = component ~index ~which ~pool ~prev ~reset graph in
  let next = recurse index (image >>| Option.some) graph in
  let%arr view = view
  and image, view2 = next in
  image, View.vbox ~gap:(`Em 1) [ view; view2 ]
;;

let component ~pool ~index ~prev graph =
  Bonsai.fix2 index prev graph ~f:(fun ~recurse index prev graph ->
    let index = index >>| ( + ) 1 in
    Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
      let which, set_which = Bonsai.state_opt graph in
      match%sub which, index with
      | None, 1 ->
        let which = Bonsai.return Which.Txt2img in
        do_txt2img ~which ~prev ~index ~pool ~reset ~recurse graph
      | Some which, _ -> do_txt2img ~which ~prev ~index ~pool ~reset ~recurse graph
      | None, _ ->
        let%arr set_which = set_which
        and prev = prev
        and theme = View.Theme.current graph in
        ( prev
        , Vdom.Node.div
            ~attrs:
              [ {%css| font-size: 3em !important; display: flex; justify-content: center; margin: 1em; gap: 1em; |}
              ]
            [ View.button theme "img2img" ~on_click:(set_which (Some Which.Txt2img))
            ; View.button theme "sketch" ~on_click:(set_which (Some Which.Sketch))
            ] )))
;;

let component ~pool ~prev graph = component ~pool ~index:(Bonsai.return 0) ~prev graph
