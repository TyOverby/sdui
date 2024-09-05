[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]

open! Core
open! Import
open Gen_js_api

type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let rec t_of_js : Ojs.t -> t =
  fun (x2 : Ojs.t) ->
  { x = Ojs.int_of_js (Ojs.get_prop_ascii x2 "x")
  ; y = Ojs.int_of_js (Ojs.get_prop_ascii x2 "y")
  ; w = Ojs.int_of_js (Ojs.get_prop_ascii x2 "w")
  ; h = Ojs.int_of_js (Ojs.get_prop_ascii x2 "h")
  }

and t_to_js : t -> Ojs.t =
  fun (x1 : t) ->
  Ojs.obj
    [| "x", Ojs.int_to_js x1.x
     ; "y", Ojs.int_to_js x1.y
     ; "w", Ojs.int_to_js x1.w
     ; "h", Ojs.int_to_js x1.h
    |]
;;
