open! Core

let any_at_lines s =
  List.exists (String.split_lines s) ~f:(fun line ->
    match String.get line 0 with
    | '@' -> true
    | _ | (exception _) -> false)
;;

let strip_prompt s =
  let any_at_lines = any_at_lines s in
  String.split_lines s
  |> List.filter_map ~f:(fun line ->
    match String.get line 0 with
    | '#' -> None
    | '!' -> Some (String.drop_prefix line 1)
    | '@' -> Some (String.drop_prefix line 1)
    | _ | (exception _) -> if any_at_lines then None else Some line)
  |> List.map ~f:String.strip
  |> String.concat_lines
;;
