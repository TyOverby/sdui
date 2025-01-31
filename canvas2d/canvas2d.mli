open! Core

module rec Canvas : sig
  type t

  (** makes a new canvas with [width x height] pixels *)
  val create : width:int -> height:int -> t

  (** get current width *)
  val width : t -> int

  (** get current height *)
  val height : t -> int

  (** Create a 2d rendering context that is attached to this canvas.
      Set [~will_read_frequently] to true if you intend on using [get_image_data]
      on the returned context. *)
  val ctx2d : ?will_read_frequently:bool -> t -> Ctx2d.t

  (** Duplicate this canvas by drawing this canvas onto a new canvas.
      Set [~will_read_frequently] to true if you intend on using [get_image_data]
      on the returned context. *)
  val clone : ?will_read_frequently:bool -> t -> t * Ctx2d.t

  (** Returns the dom element for this canvas *)
  val dom_element : t -> Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t

  (** Returns the dom element for this canvas *)
  val to_data_url : t -> string

  (** Create a new canvas by copying the pixel data from an image
      Set [~will_read_frequently] to true if you intend on using [get_image_data]
      on the returned context. *)
  val of_image : ?will_read_frequently:bool -> Image.t -> t * Ctx2d.t
end

and Ctx2d : sig
  type t

  val width : t -> int
  val height : t -> int

  (** returns the canvas that this context is attached to *)
  val backing_canvas : t -> Canvas.t

  (** returns Js_of_ocaml's representation of the 2d context. *)
  val jsoo_ctx : t -> Js_of_ocaml.Dom_html.canvasRenderingContext2D Js_of_ocaml.Js.t

  (** Takes the pixels in this rendering context and loads it into an image. *)
  val to_image : t -> on_load:(Image.t -> unit) -> unit

  val set_fill_style : t -> string -> unit
  val fill_rect : t -> x:float -> y:float -> w:float -> h:float -> unit
  val set_global_composite_operation : t -> string -> unit
  val set_filter : t -> string -> unit

  (** draws another canvas onto this context
      - [sx], [sy] specify the "source" x and y position; defaults to 0
      - [sw], [sh] specify the "source" width and height
        defaults to the width and height of the canvas
      - [w], [h] specify the width and height of the rectangle to be drawn
        defaults to the width and height of the canvas
        if [w, h] don't match [sw, sh], then the image will be squished
      - [x], [y] specify the top left corner where the image will be drawn *)
  val draw_canvas
    :  ?sx:float
    -> ?sy:float
    -> ?sw:float
    -> ?sh:float
    -> ?w:float
    -> ?h:float
    -> t
    -> Canvas.t
    -> x:float
    -> y:float
    -> unit

  (** draws an image onto this context
      - [sx], [sy] specify the "source" x and y position; defaults to 0
      - [sw], [sh] specify the "source" width and height
        defaults to the width and height of the canvas
      - [w], [h] specify the width and height of the rectangle to be drawn
        defaults to the width and height of the canvas
        if [w, h] don't match [sw, sh], then the image will be squished
      - [x], [y] specify the top left corner where the image will be drawn *)
  val draw_image
    :  ?sx:float
    -> ?sy:float
    -> ?sw:float
    -> ?sh:float
    -> ?w:float
    -> ?h:float
    -> t
    -> Image.t
    -> x:float
    -> y:float
    -> unit

  (** Retrieves the image data in the rectangle defined by [x, y, w, h].
      - [x] and [y] default to 0
      - [w] defaults to the width of the canvas minus [x]
      - [h] defaults to the width of the canvas minus [y] *)
  val get_image_data : ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> Image_data.t

  (** Copies the pixel data from an [Image_data.t] directly into this context.
      - [x] and [y] define the position offset into the destination context.

      The "dirty" optional parameters define the region _within the Image_data.t_
      that will be copied. [dirty_x] and [dirty_y] do _not_ adjust the position
      that the image will be drawn at.

      - [dirty_x] and [dirty_y] default to 0
      - [dirty_w] defaults to the width of the canvas minus [dirty_x]
      - [dirty_h] defaults to the width of the canvas minus [dirty_y] *)
  val put_image_data
    :  ?dirty_x:int
    -> ?dirty_y:int
    -> ?dirty_w:int
    -> ?dirty_h:int
    -> t
    -> Image_data.t
    -> x:int
    -> y:int
    -> unit
end

and Image_data : sig
  type t

  (** returns the width of the pixel grid in this image-data *)
  val width : t -> int

  (** returns the height of the pixel grid in this image-data *)
  val height : t -> int

  (** pixel rgb getters *)
  val get_rgba : t -> x:int -> y:int -> int * int * int * int

  (** write rgba values into an array *)
  val get_rgba' : t -> x:int -> y:int -> into:int array -> unit

  val get_r : t -> x:int -> y:int -> int
  val get_g : t -> x:int -> y:int -> int
  val get_b : t -> x:int -> y:int -> int
  val get_a : t -> x:int -> y:int -> int

  (** pixel rgb getters *)
  val set : t -> x:int -> y:int -> r:int -> g:int -> b:int -> a:int -> unit

  val set_r : t -> x:int -> y:int -> int -> unit
  val set_g : t -> x:int -> y:int -> int -> unit
  val set_b : t -> x:int -> y:int -> int -> unit
  val set_a : t -> x:int -> y:int -> int -> unit

  (** lower-level data access *)
  val data : t -> Pixel_array.t
end

and Pixel_array : sig
  (** an array of bytes that represent the pixels in an [Image_data.t].
      - The pixels are in row-major order
      - The bytes are in [r; g; b; a] order *)
  type t

  (** returns the number of bytes in this pixel array, [w * h * 4] *)
  val length : t -> int

  (** gets a byte from the array *)
  val get : t -> int -> int

  (** sets a byte into the array *)
  val set : t -> int -> int -> unit

  type bytes_bigarray :=
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t

  (** view this pixel array as an ocaml bigarray *)
  val as_bigarray : t -> bytes_bigarray
end

and Image : sig
  (** A 2d image *)
  type t

  val width : t -> int
  val height : t -> int

  (** loads an image from the given url.  The image will be passed to [on_load]
      when it finishes loading. *)
  val of_url : string -> on_load:(t -> unit) -> unit

  (** Creates an image from a canvas2d context.  The image will be passed to
      [on_load] when it finishes loading. *)
  val of_ctx : Ctx2d.t -> on_load:(t -> unit) -> unit

  (** converts the image into a png-encoded data-url *)
  val to_data_url : t -> string

  (** Returns the dom element underlying this image object *)
  val dom_element : t -> Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t

  val add_padding
    :  ?left:int
    -> ?right:int
    -> ?top:int
    -> ?bottom:int
    -> t
    -> fill_color:string
    -> on_load:(Image.t -> unit)
    -> unit
end
