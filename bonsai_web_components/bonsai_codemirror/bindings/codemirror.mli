include module type of Custom_ojs_converter
include module type of For_ppx

(* These extra modules are just here to provide a more intuitive interface for the
   raw CodeMirror bindings. *)

module State : sig
  include module type of For_ppx.State

  module Range_set_update_spec : sig
    include module type of Range_set_update_spec

    module Filter_spec : sig
      (* Filters the ranges already in the set between [from] and [to_]. Only ranges
           for which [f] returns true are kept. *)
      type 'v t =
        { f : from:int -> to_:int -> value:'v -> bool
        ; filter_from : int
        ; filter_to : int
        }
    end

    val create
      :  add:'v Range.t list
      -> sort:bool
      -> filter:'v Filter_spec.t option
      -> 'v t
  end

  module Range_set : sig
    include module type of Range_set

    val between
      :  'v t
      -> from:int
      -> to_:int
      -> f:(from:int -> to_:int -> value:'v -> [ `Stop | `Continue ])
      -> unit
  end
end
