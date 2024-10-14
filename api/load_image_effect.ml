open! Bonsai_web.Cont

let load_image_generic =
  let load_image_deferred f =
    let ivar = Async_kernel.Ivar.create () in
    f ~on_load:(Async_kernel.Ivar.fill_if_empty ivar);
    Async_kernel.Ivar.read ivar
  in
  Effect.of_deferred_fun load_image_deferred
;;

let load_image url = load_image_generic (Canvas2d.Image.of_url url)
