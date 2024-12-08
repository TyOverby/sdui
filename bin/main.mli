module Tab : sig
  type t =
    | Gen
    | Comp
    | Evolve

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val all : t list
  val equal : t -> t -> bool
  val default : t
end

val ui : local_ Bonsai.graph -> Virtual_dom.Vdom.Node.t Bonsai.t
