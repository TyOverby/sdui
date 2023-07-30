open! Core
include Yojson.Safe

let rec sexp_of_t (t : t) =
  match t with
  | `Tuple l -> Sexp.List (List.map l ~f:sexp_of_t)
  | `Bool b -> sexp_of_bool b
  | `Intlit s -> sexp_of_string s
  | `Null -> Atom "null"
  | `Variant (n, p) -> List (Atom n :: (Option.to_list p |> List.map ~f:sexp_of_t))
  | `Assoc kvp -> List (List.map kvp ~f:(fun (k, v) -> Sexp.List [ Atom k; sexp_of_t v ]))
  | `List l -> List (List.map l ~f:sexp_of_t)
  | `Float f -> sexp_of_float f
  | `String s -> sexp_of_string s
  | `Int i -> sexp_of_int i
;;

let t_of_yojson = Fn.id
let yojson_of_t = Fn.id
