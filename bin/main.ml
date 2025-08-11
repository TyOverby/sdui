open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let () = Ui_incr.State.set_max_height_allowed Ui_incr.State.t (Int.shift_left 1024 3)

module Tab = struct
  type t =
    | Gen
    | Comp
    | Evolve
    | Swipe
  [@@deriving sexp, enumerate, equal]

  let default = Evolve
end

let ui (local_ graph) =
  let tab, set_tab = Bonsai.state Tab.default graph in
  let tabs =
    let%arr tab
    and set_tab
    and theme = View.Theme.current graph in
    View.tabs_enum
      theme
      (module Tab)
      ~active:tab
      ~on_change:(fun ~from:_ ~to_ -> set_tab to_)
  in
  let which =
    match%sub tab with
    | Gen -> Sd_chain.component graph [@nontail]
    | Comp ->
      let%arr view = Comp.component graph
      and tabs in
      View.vbox [ tabs; view ]
    | Evolve -> Evolve.component ~tabs graph [@nontail]
    | Swipe -> Swipe.component ~tabs graph
  in
  which
;;

let () =
  Bonsai_web.Start.start
    (View.Theme.set_for_app
       (Bonsai.return (Kado.theme ~style:Dark ~version:Bleeding ()))
       (fun graph -> ui graph [@nontail]))
;;
