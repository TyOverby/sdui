open! Core
open! Bonsai_web.Cont

module Item_id : sig
  type t [@@deriving of_sexp]
end

type ('a, 'spec) t =
  { push_back : 'spec -> 'a -> Item_id.t Ui_effect.t
  ; pop_front : 'spec -> 'a Ui_effect.t
  ; remove : Item_id.t -> unit Ui_effect.t
  ; debug : Sexp.t Lazy.t
  }

val pipe
  :  ?sexp_of_a:('a -> Sexp.t)
  -> ?sexp_of_spec:('spec -> Sexp.t)
  -> compare:('spec -> 'spec -> int)
  -> Bonsai.graph
  -> ('a, 'spec) t Bonsai.t
