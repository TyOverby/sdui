open Js_of_ocaml
open Gen_js_api

module Dom_html_element = struct
  type t = Dom_html.element Js.t

  let t_to_js : t -> Ojs.t = Obj.magic
  let t_of_js : Ojs.t -> t = Obj.magic
end

module Callback = struct
  type 'a t = 'a Js.callback

  let t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t = fun _ -> Obj.magic
  let t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t = fun _ -> Obj.magic
end

module With_conversion = struct
  type 'a t = 'a * ('a -> Ojs.t)

  let create ~t_to_js x = x, t_to_js
  let t_to_js _ (x, t_to_js) = t_to_js x
end
