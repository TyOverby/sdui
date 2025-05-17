open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
    :root {
      interpolate-size: allow-keywords; 
    }

  .img {
    transform: translate(0px, 0px);
    transition: 0.1s ease-out transform, 0.5s ease-out height;
  }

  :root:has(.lock),
  :root:has(.lock) * {
    touch-action:none;
  }
|}]

let component ~on_remove (local_ graph) =
  let drag_start, set_drag_start = Bonsai.state_opt graph in
  let drag_pos, set_drag_pos = Bonsai.state_opt graph in
  let width, set_width = Bonsai.state_opt graph in
  let height, set_height = Bonsai.state_opt graph in
  let removing, set_removing = Bonsai.state false graph in
  let%arr drag_start
  and set_drag_start
  and drag_pos
  and set_drag_pos
  and width
  and set_width
  and removing
  and set_removing
  and height
  and set_height
  and sleep = Bonsai.Clock.sleep graph
  and on_remove in
  let height =
    match height with
    | None -> Vdom.Attr.empty
    | Some height -> {%css| height: %{Int.to_string height ^ "px"} !important;|}
  in
  let transform =
    match drag_start, drag_pos, width with
    | Some (sx, sy), Some (x, y), _ ->
      let dx = Float.abs (sx -. x)
      and dy = Float.abs (sy -. y) in
      Vdom.Attr.many
        [ (if Float.(dx > dy) then Style.lock else Vdom.Attr.empty)
        ; {%css|
        transition: 0s linear transform;
        transform: translate(%{ Virtual_dom.Dom_float.to_string (x -. sx) ^ "px"}, 0px); |}
        ]
    | _, _, Some w when removing ->
      {%css|
        transform: translate(%{ Int.to_string (-w) ^ "px"}, 0px);
        height: 0px !important; |}
    | _ -> Vdom.Attr.empty
  in
  let pointer_up evt =
    (match Js_of_ocaml.Js.Opt.to_option evt##.target with
     | Some target -> (Obj.magic target)##releasePointerCapture evt##.pointerId
     | None -> ());
    let dragged_dir =
      match drag_start, drag_pos, width with
      | Some (sx, _), Some (x, _), Some w ->
        let dx = sx -. x in
        print_s [%message (dx : float) (w : int)];
        if Float.(abs dx > of_int w / 4.0)
        then (
          match Float.robust_sign dx with
          | Neg -> `Right
          | Pos -> `Left (Float.of_int w)
          | _ -> `None)
        else (
          print_endline "b";
          `None)
      | _ ->
        print_endline "c";
        `None
    in
    print_s [%message (dragged_dir : [ `Left of float | `None | `Right ])];
    let effects =
      match dragged_dir with
      | `Left width ->
        Effect.Many
          [ set_drag_pos (Some (-.width, 0.0))
          ; set_removing true
          ; (let%bind.Effect () = sleep (Time_ns.Span.of_sec 0.6) in
             on_remove)
          ]
      | `Right | `None -> Effect.Many [ set_drag_start None; set_drag_pos None ]
    in
    Effect.Many [ Effect.Stop_propagation; effects; set_drag_start None ]
  in
  Vdom.Attr.many
    [ Vdom.Attr.on_pointerdown (fun evt ->
        (match Js_of_ocaml.Js.Opt.to_option evt##.target with
         | Some target -> (Obj.magic target)##setPointerCapture evt##.pointerId
         | None -> ());
        print_endline "down";
        let drag =
          let x = Js_of_ocaml.Js.float_of_number evt##.clientX
          and y = Js_of_ocaml.Js.float_of_number evt##.clientY in
          [ set_drag_start (Some (x, y)); set_drag_pos (Some (x, y)) ]
        in
        let width =
          match Js_of_ocaml.Js.Opt.to_option evt##.target with
          | Some target ->
            let w = target##.clientWidth in
            let h = target##.clientHeight in
            Effect.Many [ set_width (Some w); set_height (Some h) ]
          | None ->
            print_endline "c";
            Effect.Ignore
        in
        Effect.Many [ Effect.Prevent_default; Effect.Many drag; width ])
    ; Vdom.Attr.on_pointermove (fun evt ->
        (*    Js_of_ocaml.Console.console##log evt;*)
        let x = Js_of_ocaml.Js.float_of_number evt##.clientX
        and y = Js_of_ocaml.Js.float_of_number evt##.clientY in
        Effect.Many [ Effect.Prevent_default; set_drag_pos (Some (x, y)) ])
    ; Vdom.Attr.on_pointerup pointer_up
    ; Vdom.Attr.on_pointerleave pointer_up
    ; transform
    ; Style.img
    ; height
    ]
;;
