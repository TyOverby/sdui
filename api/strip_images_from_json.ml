open! Core
open Js_of_ocaml

let f =
  Js.Unsafe.pure_js_expr
    {|
(function(s){
  let o = JSON.parse(s);
  let images = o.images;
  
  o.images = [];
  o.init_images = [];
  
  if (o.parameters) {
    o.parameters.init_images = [];
    o.parameters.mask = [];
  }
  
  return [JSON.stringify(o), images];
})
      |}
;;

let f json =
  let json_and_images = Js.Unsafe.fun_call f [| Js.Unsafe.inject (Js.string json) |] in
  let json =
    Js.Optdef.get (Js.array_get json_and_images 0) (fun () -> assert false)
    |> Js.Unsafe.coerce
    |> Js.to_string
  in
  let images =
    Js.Optdef.get (Js.array_get json_and_images 1) (fun () -> assert false)
    |> Js.Unsafe.coerce
    |> Js.to_array
    |> Array.to_list
    |> List.map ~f:Js.to_string
  in
  json, images
;;
