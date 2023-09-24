open! Core
open! Bonsai_web

let () =
  Bonsai_web.Start.start
    (View.Theme.set_for_app
       (Value.return (Kado.theme ~style:Dark ~version:Bleeding ()))
       Sd.component)
;;
