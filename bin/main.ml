open! Core
open! Bonsai_web

let () = Ui_incr.State.set_max_height_allowed Ui_incr.State.t (Int.shift_left 1024 3)

let () =
  Bonsai_web.Start.start
    (View.Theme.set_for_app
       (Value.return (Kado.theme ~style:Dark ~version:Bleeding ()))
       Sd_chain.component)
;;
