open! Core
open! Bonsai_web

type t [@@deriving sexp]

(** Computes an identifier that changes every time the model gets reset (i.e.
    by [with_model_resetter]. This is useful for widgets, which may have reason
    to witness when a model gets reset. By putting a [t] in the input of such a
    widget, the input will change every time the model gets reset, causing the
    [update] lifecycle function to be called.

    If [reset_when_deactivated] is true, then the result will go back to its
    initial value if it is de-activated. This may be useful to ensure that
    putting this computation inside an [assoc] does not contribute to a
    memory-leak. *)
val model_resetter_generation : Bonsai.graph -> t Bonsai.t

(** Basically the opposite of [equal]. The point of a [t] is to notice when it
    is different, not when it is the same, as something else. This function
    might sometimes return false when the two values are actually distinct, but
    never on consecutive incremental frames. *)
val distinct : t -> t -> bool
