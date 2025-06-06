open! Core
open! Js_of_ocaml
module Dom_float = Virtual_dom.Dom_float

let the_string_''width'' = Js.string "width"
let the_string_''height'' = Js.string "height"

module Canvas0 = struct
  class type getContextOptions = object
    method willReadFrequently : bool Js.t Js.readonly_prop
    method alpha : bool Js.t Js.readonly_prop
  end

  class type extendedCanvasElement = object
    inherit Dom_html.canvasElement

    method getContext_options :
      Dom_html.context
      -> getContextOptions Js.t
      -> Dom_html.canvasRenderingContext2D Js.t Js.meth

    method toDataURL_quality : Js.js_string Js.t -> float -> Js.js_string Js.t Js.meth
  end

  type t = extendedCanvasElement Js.t
end

module Image0 = struct
  class type extendedImageElement = object
    inherit Dom_html.imageElement
    method crossOrigin : Js.js_string Js.t Js.prop
  end

  type t = extendedImageElement Js.t

  let of_image_element = (Obj.magic : Dom_html.imageElement Js.t -> t)
  let to_image_element = (Obj.magic : t -> Dom_html.imageElement Js.t)
  let width t = t##.naturalWidth
  let height t = t##.naturalHeight
end

module Image_data0 = struct
  type t =
    { obj : Dom_html.imageData Js.t
    ; data : Dom_html.canvasPixelArray Js.t
    ; width : int
    ; height : int
    }
end

module Ctx2d0 = struct
  class type extendedCanvasContext = object
    inherit Dom_html.canvasRenderingContext2D

    method putImageData_all :
      Dom_html.imageData Js.t -> int -> int -> int -> int -> int -> int -> unit Js.meth

    method filter : Js.js_string Js.t Js.prop
  end

  type t = Canvas0.t * extendedCanvasContext Js.t

  let backing_canvas (canvas, _ctx) = canvas

  let jsoo_ctx (_canvas, ctx) =
    (Obj.magic : extendedCanvasContext Js.t -> Dom_html.canvasRenderingContext2D Js.t) ctx
  ;;
end

module Pixel_array0 = struct
  type t = Dom_html.canvasPixelArray Js.t
end

module Canvas1 = struct
  include Canvas0

  let create ~width ~height : t =
    let canvas = Dom_html.createCanvas Dom_html.document in
    canvas##setAttribute the_string_''width'' (Js.string (Int.to_string width));
    canvas##setAttribute the_string_''height'' (Js.string (Int.to_string height));
    canvas##.style##.width
    := Dom_float.to_js_string
         (Int.to_float width
          /. Js_of_ocaml.Js.float_of_number Dom_html.window##.devicePixelRatio);
    canvas##.style##.height
    := Dom_float.to_js_string
         (Int.to_float height
          /. Js_of_ocaml.Js.float_of_number Dom_html.window##.devicePixelRatio);
    (Obj.magic : Dom_html.canvasElement Js.t -> extendedCanvasElement Js.t) canvas
  ;;

  let dom_element t =
    (Obj.magic : extendedCanvasElement Js.t -> Dom_html.canvasElement Js.t) t
  ;;

  let width (t : t) = t##.width
  let height (t : t) = t##.height

  let ctx2d ?(will_read_frequently = false) (t : t) : Ctx2d0.t =
    let ctx =
      t##getContext_options
        Dom_html._2d_
        (object%js
           val willReadFrequently = Js.bool will_read_frequently
           val alpha = Js._true
        end)
    in
    let ctx =
      (Obj.magic
       : Dom_html.canvasRenderingContext2D Js.t -> Ctx2d0.extendedCanvasContext Js.t)
        ctx
    in
    t, ctx
  ;;

  let to_data_url_raw canvas = canvas##toDataURL_quality (Js.string "image/png") 1.0
  let to_data_url canvas = Js.to_string (to_data_url_raw canvas)
end

module Ctx2d1 = struct
  include Ctx2d0

  let width (canvas, _) = Canvas1.width canvas
  let height (canvas, _) = Canvas1.height canvas

  let draw_canvas ?sx ?sy ?sw ?sh ?w ?h ((_, ctx) : t) (canvas : Canvas1.t) ~x ~y =
    let sx = Option.value sx ~default:0.0 in
    let sy = Option.value sy ~default:0.0 in
    let sw =
      Option.value_or_thunk sw ~default:(fun () -> Float.of_int (Canvas1.width canvas))
    in
    let sh =
      Option.value_or_thunk sh ~default:(fun () -> Float.of_int (Canvas1.height canvas))
    in
    let w = Option.value w ~default:sw in
    let h = Option.value h ~default:sh in
    ctx##drawImage_fullFromCanvas
      (canvas :> Dom_html.canvasElement Js.t)
      (Js_of_ocaml.Js.number_of_float sx)
      (Js_of_ocaml.Js.number_of_float sy)
      (Js_of_ocaml.Js.number_of_float sw)
      (Js_of_ocaml.Js.number_of_float sh)
      (Js_of_ocaml.Js.number_of_float x)
      (Js_of_ocaml.Js.number_of_float y)
      (Js_of_ocaml.Js.number_of_float w)
      (Js_of_ocaml.Js.number_of_float h)
  ;;

  let draw_image ?sx ?sy ?sw ?sh ?w ?h ((_, ctx) : t) (image : Image0.t) ~x ~y =
    let sx = Option.value sx ~default:0.0 in
    let sy = Option.value sy ~default:0.0 in
    let sw =
      Option.value_or_thunk sw ~default:(fun () -> Int.to_float (Image0.width image))
    in
    let sh =
      Option.value_or_thunk sh ~default:(fun () -> Int.to_float (Image0.height image))
    in
    let w = Option.value w ~default:sw in
    let h = Option.value h ~default:sh in
    ctx##drawImage_full
      (Image0.to_image_element image)
      (Js_of_ocaml.Js.number_of_float sx)
      (Js_of_ocaml.Js.number_of_float sy)
      (Js_of_ocaml.Js.number_of_float sw)
      (Js_of_ocaml.Js.number_of_float sh)
      (Js_of_ocaml.Js.number_of_float x)
      (Js_of_ocaml.Js.number_of_float y)
      (Js_of_ocaml.Js.number_of_float w)
      (Js_of_ocaml.Js.number_of_float h)
  ;;

  let get_image_data ?(x = 0) ?(y = 0) ?w ?h ((canvas, ctx) : t) =
    let w = Option.value_or_thunk w ~default:(fun () -> Canvas1.width canvas - x) in
    let h = Option.value_or_thunk h ~default:(fun () -> Canvas1.height canvas - y) in
    let obj =
      ctx##getImageData
        (Js_of_ocaml.Js.number_of_float (Int.to_float x))
        (Js_of_ocaml.Js.number_of_float (Int.to_float y))
        (Js_of_ocaml.Js.number_of_float (Int.to_float w))
        (Js_of_ocaml.Js.number_of_float (Int.to_float h))
    in
    { Image_data0.obj; width = obj##.width; height = obj##.height; data = obj##.data }
  ;;

  let put_image_data
    ?(dirty_x = 0)
    ?(dirty_y = 0)
    ?dirty_w
    ?dirty_h
    ((canvas, ctx) : t)
    (img_data : Image_data0.t)
    ~x
    ~y
    =
    let dirty_w =
      Option.value_or_thunk dirty_w ~default:(fun () -> Canvas1.width canvas - dirty_x)
    in
    let dirty_h =
      Option.value_or_thunk dirty_h ~default:(fun () -> Canvas1.height canvas - dirty_y)
    in
    ctx##putImageData_all img_data.obj x y dirty_x dirty_y dirty_w dirty_h
  ;;

  let set_fill_style (_, ctx) s = ctx##.fillStyle := Js.string s

  let fill_rect (_, ctx) ~x ~y ~w ~h =
    ctx##fillRect
      (Js_of_ocaml.Js.number_of_float x)
      (Js_of_ocaml.Js.number_of_float y)
      (Js_of_ocaml.Js.number_of_float w)
      (Js_of_ocaml.Js.number_of_float h)
  ;;

  let set_global_composite_operation (_, ctx) s =
    ctx##.globalCompositeOperation := Js.string s
  ;;

  let set_filter (_, ctx) s = ctx##.filter := Js.string s
end

module Pixel_array1 = struct
  include Pixel_array0

  let length (t : t) = t##.length
  let get (t : t) pos = Dom_html.pixel_get t pos
  let set (t : t) pos value = Dom_html.pixel_set t pos value

  let as_bigarray (t : t) =
    let typed_array =
      new%js Js_of_ocaml.Typed_array.uint8Array_fromTypedArray
        (Obj.magic t : Js_of_ocaml.Typed_array.uint8Array Js.t)
    in
    Js_of_ocaml.Typed_array.to_genarray typed_array
  ;;
end

module Image_data1 = struct
  include Image_data0

  let data (t : t) = t.data
  let width (t : t) = t.width
  let height (t : t) = t.height

  let[@inline always] get_rgba (t : t) ~x ~y =
    let index = ((y * t.width) + x) * 4 in
    let data = data t in
    let r = Pixel_array1.get data index in
    let g = Pixel_array1.get data (index + 1) in
    let b = Pixel_array1.get data (index + 2) in
    let a = Pixel_array1.get data (index + 3) in
    r, g, b, a
  ;;

  let[@inline always] get_rgba' (t : t) ~x ~y ~into =
    let index = ((y * t.width) + x) * 4 in
    let data = t.data in
    let r = Pixel_array1.get data index in
    let g = Pixel_array1.get data (index + 1) in
    let b = Pixel_array1.get data (index + 2) in
    let a = Pixel_array1.get data (index + 3) in
    Array.unsafe_set into 0 r;
    Array.unsafe_set into 1 g;
    Array.unsafe_set into 2 b;
    Array.unsafe_set into 3 a
  ;;

  let get_r (t : t) ~x ~y =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.get data index
  ;;

  let get_g (t : t) ~x ~y =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.get data (index + 1)
  ;;

  let get_b (t : t) ~x ~y =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.get data (index + 2)
  ;;

  let get_a (t : t) ~x ~y =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.get data (index + 3)
  ;;

  let set (t : t) ~x ~y ~r ~g ~b ~a =
    let index = ((y * t.width) + x) * 4 in
    let data = t.data in
    Pixel_array1.set data index r;
    Pixel_array1.set data (index + 1) g;
    Pixel_array1.set data (index + 2) b;
    Pixel_array1.set data (index + 3) a
  ;;

  let set_r (t : t) ~x ~y r =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.set data index r
  ;;

  let set_g (t : t) ~x ~y g =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.set data (index + 1) g
  ;;

  let set_b (t : t) ~x ~y b =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.set data (index + 2) b
  ;;

  let set_a (t : t) ~x ~y a =
    let index = ((y * width t) + x) * 4 in
    let data = data t in
    Pixel_array1.set data (index + 3) a
  ;;
end

module Canvas2 = struct
  include Canvas1

  let clone ?(will_read_frequently = false) (t : t) =
    let the_canvas = create ~width:(width t) ~height:(height t) in
    let the_ctx = Canvas1.ctx2d ~will_read_frequently the_canvas in
    Ctx2d1.draw_canvas the_ctx t ~x:0.0 ~y:0.0;
    the_canvas, the_ctx
  ;;

  let of_image ?will_read_frequently (image : Image0.t) =
    let canvas =
      Canvas1.create ~width:(Image0.width image) ~height:(Image0.height image)
    in
    let ctx = Canvas1.ctx2d ?will_read_frequently canvas in
    Ctx2d1.draw_image ctx image ~x:0.0 ~y:0.0;
    canvas, ctx
  ;;
end

module Image1 = struct
  include Image0

  let of_url_raw ~url ~on_load =
    let element = Image0.of_image_element (Dom_html.createImg Dom_html.document) in
    element##.crossOrigin := Js.string "Anonymous";
    element##setAttribute (Js.string "src") url;
    if Js.to_bool element##.complete
    then on_load element
    else
      element##.onload
      := Dom.handler (fun _ ->
           on_load element;
           Js._true)
  ;;

  let of_url url ~on_load = of_url_raw ~url:(Js.string url) ~on_load

  let to_data_url_raw (t : t) =
    let canvas = Canvas2.create ~width:(width t) ~height:(height t) in
    let ctx = Canvas2.ctx2d canvas in
    Ctx2d1.draw_image ctx t ~x:0.0 ~y:0.0;
    Canvas2.to_data_url_raw canvas
  ;;

  let to_data_url (t : t) = Js.to_string (to_data_url_raw (t : t))

  let of_ctx (ctx : Ctx2d1.t) ~on_load =
    let canvas = Canvas2.create ~width:(Ctx2d1.width ctx) ~height:(Ctx2d1.height ctx) in
    let ctx2 = Canvas2.ctx2d canvas in
    Ctx2d1.put_image_data ctx2 (Ctx2d1.get_image_data ctx) ~x:0 ~y:0;
    let url = Canvas2.to_data_url_raw canvas in
    of_url_raw ~url ~on_load
  ;;

  let dom_element = to_image_element

  let add_padding
    ?(left = 0)
    ?(right = 0)
    ?(top = 0)
    ?(bottom = 0)
    img
    ~fill_color
    ~on_load
    =
    match left, right, top, bottom with
    | 0, 0, 0, 0 -> on_load img
    | _ ->
      let canvas =
        Canvas2.create
          ~width:(width img + left + right)
          ~height:(height img + top + bottom)
      in
      let ctx = Canvas2.ctx2d canvas in
      Ctx2d1.set_fill_style ctx fill_color;
      Ctx2d1.fill_rect
        ~x:0.0
        ~y:0.0
        ~w:(Float.of_int (Canvas2.width canvas))
        ~h:(Float.of_int (Canvas2.height canvas))
        ctx;
      Ctx2d1.draw_image ctx img ~x:(Float.of_int left) ~y:(Float.of_int top);
      of_ctx ctx ~on_load
  ;;

  let resize (img : Image0.t) ~w ~h ~on_load =
    let canvas = Canvas2.create ~width:w ~height:h in
    let ctx = Canvas2.ctx2d canvas in
    Ctx2d1.draw_image ctx img ~x:0.0 ~y:0.0 ~w:(Float.of_int w) ~h:(Float.of_int h);
    of_ctx ctx ~on_load
  ;;
end

module Ctx2d2 = struct
  include Ctx2d1

  let to_image t ~on_load = Image1.of_ctx t ~on_load
end

module Canvas = Canvas2
module Ctx2d = Ctx2d2
module Pixel_array = Pixel_array1
module Image_data = Image_data1
module Image = Image1
