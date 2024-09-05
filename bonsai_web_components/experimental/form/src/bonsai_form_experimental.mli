module Combine = Combine
module Product = Product
module Validated = Validated
open! Core
open Bonsai_web

type ('input, 'result, 'parsed) t =
  default:'parsed
  -> ( 'input
       , ('result Or_error.t Product.With_view.t, 'parsed) Product.t )
       Arrow_deprecated.Bonsai.t

val text_input : default:string -> (_, string Product.Same.t) Arrow_deprecated.Bonsai.t

val textarea_input
  :  default:string
  -> (_, string Product.Same.t) Arrow_deprecated.Bonsai.t

val checkbox_input
  :  ?label:string
  -> default:bool
  -> unit
  -> (_, bool Product.Same.t) Arrow_deprecated.Bonsai.t

val date_picker_with_bad_user_experience
  :  default:Date.t
  -> (_, Date.t Product.Same.t) Arrow_deprecated.Bonsai.t

val date_picker
  :  default:Date.t option
  -> (_, Date.t option Product.Same.t) Arrow_deprecated.Bonsai.t

module Dropdown : sig
  module type Equal = sig
    type t [@@deriving equal, sexp]

    val to_string : t -> string
  end

  (** Creates a dropdown that updates when the model changes and updates the model when
      the user selects a different item. *)
  val of_input
    :  (module Equal with type t = 'a)
    -> default:'a
    -> ('a list, 'a Product.Same.t) Arrow_deprecated.Bonsai.t

  (** Same as [of_input], but includes a blank first entry to represent [None]. *)
  val of_input_opt
    :  (module Equal with type t = 'a)
    -> default:'a option
    -> ('a list, 'a option Product.Same.t) Arrow_deprecated.Bonsai.t

  module type Enum = sig
    type t [@@deriving enumerate, equal, sexp]

    val to_string : t -> string
  end

  (** Same as [of_input], but uses [all] from the module to determine the items and order. *)
  val of_enum
    :  (module Enum with type t = 'a)
    -> default:'a
    -> (_, 'a Product.Same.t) Arrow_deprecated.Bonsai.t

  (** Same as [of_enum] but takes a dynamically computed [default]. *)
  val of_enum_dynamic_model
    :  (module Enum with type t = 't)
    -> default:'t Bonsai.t
    -> Bonsai.graph
    -> ('t Product.With_view.t, 't) Product.t Bonsai.t

  (** Same as [of_enum], but includes a blank first entry to represent [None]. *)
  val of_enum_opt
    :  (module Enum with type t = 'a)
    -> default:'a option
    -> (_, 'a option Product.Same.t) Arrow_deprecated.Bonsai.t
end
