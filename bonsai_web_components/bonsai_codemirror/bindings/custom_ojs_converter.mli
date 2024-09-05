open Js_of_ocaml
open Gen_js_api

module Dom_html_element : sig
  type t = Dom_html.element Js.t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
end

module Callback : sig
  type 'a t = 'a Js.callback

  val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t
end

module With_conversion : sig
  type 'a t

  val create : t_to_js:('a -> Ojs.t) -> 'a -> 'a t
  val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t
end
