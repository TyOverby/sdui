open! Core

type t [@@deriving sexp_of]

module Checkpoint_data : sig
  type 'decoration t

  val get_decorations_since_last_checkpoint : 'decoration t -> 'decoration list
end

module Highlight_accumulator : sig
  type 'decoration t
end

module Highlight_result : sig
  type 'decoration t

  val get_all_checkpoints : 'decoration t -> 'decoration Checkpoint_data.t Checkpoints.t
end

module type Text = sig
  type t

  val foldi : t -> init:'acc -> f:(int -> 'acc -> char -> 'acc) -> 'acc
  val length : t -> int
  val suffix : t -> int -> t
end

val highlight
  :  (module Text with type t = 'text)
  -> 'text
  -> checkpoints:'decoration Checkpoint_data.t Checkpoints.t
  -> combine:
       (index:int -> nesting:int -> decorations:'decoration list -> 'decoration list)
  -> 'decoration Highlight_result.t
