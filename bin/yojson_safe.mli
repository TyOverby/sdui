open! Core

type t = Yojson.Safe.t [@@deriving sexp_of, yojson]

include module type of Yojson.Safe with type t := t
