open! Core

module Colors : sig
  type t

  (** The colors are based on classes. The parentheses will cycle
      through the colors in the list based on their nesting. *)
  val create : classes:string Nonempty_list.t -> t

  val default : t lazy_t
end

(** Creates a rainbow parentheses codemirror extension for S-expressions. *)
val extension : ?colors:Colors.t -> unit -> Codemirror.State.Extension.t
