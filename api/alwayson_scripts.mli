module Ctrlnet : sig
  module Query : sig
    type t =
      { image : Image.t
      ; module_ : string option
      ; model : string
      ; weight : float
      ; guidance_start : float
      ; guidance_end : float
      }
    [@@deriving equal, sexp]
  end
end

module Regional_prompter : sig
  module Query : sig
    type t =
      { ratios : string
      ; matrix_mode : [ `Columns | `Horizontal | `Rows | `Vertical ]
      }
    [@@deriving equal, sexp]
  end
end

type t [@@deriving yojson_of, sexp]

val create
  :  ctrlnet:Ctrlnet.Query.t option
  -> regional_prompter:Regional_prompter.Query.t option
  -> unit
  -> t option
