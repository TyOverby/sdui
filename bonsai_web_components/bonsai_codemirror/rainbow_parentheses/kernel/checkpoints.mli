open! Core

type 'a t [@@deriving sexp_of]

val to_string : 'a t -> ('a -> string) -> string
val empty : 'a t

val add_checkpoint
  :  'a t
  -> pos:int
  -> data:'a
  -> ?at_end_of_parse:bool
  -> unit
  -> 'a t Or_error.t

(* Gets the last checkpoint less than or equal to `before`. `before`
   defaults to Int.max_value. Does not include the end-of-parse checkpoint. *)
val get_last_checkpoint : 'a t -> ?before:int -> unit -> (int * 'a) option

(* Discards checkpoints greater than or equal to `after` and
   discards the end-of-parse checkpoint, if present. *)
val discard_checkpoints : 'a t -> after:int -> 'a t

(* Gets checkpoints starting from the last checkpoint before `min` and
   extending to the first checkpoint after `max`. Includes the end-of-parse
   checkpoint if it exists and is in/around this range. *)
val get_checkpoints_around_range : 'a t -> min:int -> max:int -> (int * 'a) list
