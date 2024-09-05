[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]

open! Core
open! Import
open Gen_js_api

module Series = struct
  type t =
    { dashHTML : Raw_html.t
    ; label : string
    ; labelHTML : Raw_html.t
    ; isVisible : bool
    ; isHighlighted : bool option
    ; color : string option
    ; y : float option
    ; yHTML : Raw_html.t option
    }
  [@@deriving equal, sexp]

  let rec t_of_js : Ojs.t -> t =
    fun (x6 : Ojs.t) ->
    { dashHTML = Raw_html.t_of_js (Ojs.get_prop_ascii x6 "dashHTML")
    ; label = Ojs.string_of_js (Ojs.get_prop_ascii x6 "label")
    ; labelHTML = Raw_html.t_of_js (Ojs.get_prop_ascii x6 "labelHTML")
    ; isVisible = Ojs.bool_of_js (Ojs.get_prop_ascii x6 "isVisible")
    ; isHighlighted =
        Ojs.option_of_js Ojs.bool_of_js (Ojs.get_prop_ascii x6 "isHighlighted")
    ; color = Ojs.option_of_js Ojs.string_of_js (Ojs.get_prop_ascii x6 "color")
    ; y = Ojs.option_of_js Ojs.float_of_js (Ojs.get_prop_ascii x6 "y")
    ; yHTML = Ojs.option_of_js Raw_html.t_of_js (Ojs.get_prop_ascii x6 "yHTML")
    }

  and t_to_js : t -> Ojs.t =
    fun (x1 : t) ->
    Ojs.obj
      [| "dashHTML", Raw_html.t_to_js x1.dashHTML
       ; "label", Ojs.string_to_js x1.label
       ; "labelHTML", Raw_html.t_to_js x1.labelHTML
       ; "isVisible", Ojs.bool_to_js x1.isVisible
       ; "isHighlighted", Ojs.option_to_js Ojs.bool_to_js x1.isHighlighted
       ; "color", Ojs.option_to_js Ojs.string_to_js x1.color
       ; "y", Ojs.option_to_js Ojs.float_to_js x1.y
       ; "yHTML", Ojs.option_to_js Raw_html.t_to_js x1.yHTML
      |]
  ;;
end

type t =
  { x : float option
  ; xHTML : Html_or_number.t option
  ; series : Series.t list
  }
[@@deriving equal, sexp]

let rec t_of_js : Ojs.t -> t =
  fun (x15 : Ojs.t) ->
  { x = Ojs.option_of_js Ojs.float_of_js (Ojs.get_prop_ascii x15 "x")
  ; xHTML = Ojs.option_of_js Html_or_number.t_of_js (Ojs.get_prop_ascii x15 "xHTML")
  ; series = Ojs.list_of_js Series.t_of_js (Ojs.get_prop_ascii x15 "series")
  }

and t_to_js : t -> Ojs.t =
  fun (x11 : t) ->
  Ojs.obj
    [| "x", Ojs.option_to_js Ojs.float_to_js x11.x
     ; "xHTML", Ojs.option_to_js Html_or_number.t_to_js x11.xHTML
     ; "series", Ojs.list_to_js Series.t_to_js x11.series
    |]
;;
