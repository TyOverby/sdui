open! Core

type ('k, 'v) t =
  | Empty
  | Branch of
      { key : 'k
      ; data : 'v
      ; children : ('k, 'v) t list
      }

let rec sexp_of_t sexp_of_k sexp_of_v = function
  | Empty -> Sexp.List []
  | Branch { key; data; children } ->
    List [ sexp_of_k key; sexp_of_v data; [%sexp_of: (k, v) t list] children ]
;;

let empty = Empty
let branch ~key ~data ~children = Branch { key; data; children }

open Bonsai_web

let rec render t ~f =
  match t with
  | Empty -> Vdom.Node.none
  | Branch { key; data; children = [] } -> f key data
  | Branch { key; data; children = [ next ] } -> View.hbox [ f key data; render next ~f ]
  | Branch { key; data; children } ->
    View.hbox [ f key data; View.vbox (List.map children ~f:(render ~f)) ]
;;
