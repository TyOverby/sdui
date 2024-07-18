open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

module Which = struct
  type t =
    | Txt2img
    | Sketch
    | Mask
end

let mask_impl ~prev graph =
  let prev = Inc.map_pure prev ~f:List.hd_exn in
  match%sub prev with
  | Inc.Or_error_or_stale.Fresh prev_for_paint | Stale prev_for_paint ->
    let mask, view = Paint.component ~prev:prev_for_paint ~is_mask:true graph in
    let%arr prev = prev
    and mask = mask
    and view = view in
    Inc.Or_error_or_stale.map prev ~f:List.return, Some mask, view
  | Error e ->
    let%arr e = e in
    ( Inc.Or_error_or_stale.Error e
    , None
    , Vdom.Node.textf "Error: %s" (Error.to_string_hum e) )
  | Not_computed ->
    Bonsai.return (Inc.Or_error_or_stale.Not_computed, None, Vdom.Node.text "not computed")
;;

let component ~index ~which ~pool ~prev ~mask ~reset graph =
  let%sub image, mask, view =
    match%sub which, prev with
    | Which.Txt2img, prev ->
      let image, view = Single.component ~pool ~prev ~mask ~default_size:512 graph in
      let%arr image = image
      and view = view in
      image, None, view
    | Sketch, Some prev ->
      let image, view = Paint.multi ~prev ~is_mask:false graph in
      let%arr image = image
      and view = view in
      image, None, view
    | Mask, Some prev -> mask_impl ~prev graph
    | (Sketch | Mask), None ->
      Bonsai.return
        ( Inc.Or_error_or_stale.Error
            (Error.of_string "sketch can't be the first item in the chain")
        , None
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
  image, mask, view
;;

let do_txt2img ~which ~prev ~mask ~index ~pool ~reset ~recurse graph =
  let image, mask, view = component ~index ~which ~pool ~prev ~mask ~reset graph in
  let next = recurse index (Bonsai.both (image >>| Option.some) mask) graph in
  let%arr view = view
  and image, view2 = next in
  image, View.vbox ~gap:(`Em 1) [ view; view2 ]
;;

let _fix4 a b c d ~f graph =
  let abcd = Bonsai.map4 a b c d ~f:(fun a b c d -> a, b, c, d) in
  Bonsai.fix abcd graph ~f:(fun ~recurse abcd graph ->
    let%sub a, b, c, d = abcd in
    let recurse a b c d =
      let abcd = Bonsai.map4 a b c d ~f:(fun a b c d -> a, b, c, d) in
      recurse abcd graph
    in
    f ~recurse a b c d graph)
;;

let component ~pool ~index ~prev_and_mask graph =
  Bonsai.fix2 index prev_and_mask graph ~f:(fun ~recurse index prev_and_mask graph ->
    let index = index >>| ( + ) 1 in
    let%sub prev, mask = prev_and_mask in
    Bonsai.with_model_resetter' graph ~f:(fun ~reset graph ->
      let which, set_which = Bonsai.state_opt graph in
      match%sub which, index with
      | None, 1 ->
        let which = Bonsai.return Which.Txt2img in
        do_txt2img ~which ~prev ~mask ~index ~pool ~reset ~recurse graph
      | Some which, _ -> do_txt2img ~which ~prev ~mask ~index ~pool ~reset ~recurse graph
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
            ; View.button theme "mask" ~on_click:(set_which (Some Which.Mask))
            ] )))
;;

let component ~pool ~prev graph =
  component
    ~pool
    ~index:(Bonsai.return 0)
    ~prev_and_mask:(prev >>| fun prev -> prev, None)
    graph
;;
