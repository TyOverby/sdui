open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Form_manual
module Elements = Elements_manual

module Optional = struct
  let dropdown
    (type a view)
    ?(some_label = "Some")
    ?(none_label = "None")
    ?extra_attrs
    (form : Bonsai.graph -> (a, view) Form.t Bonsai.t)
    : Bonsai.graph -> (a option, Vdom.Node.t * view option) Form.t Bonsai.t
    =
    fun graph ->
    let module M = struct
      type t =
        | None
        | Some of a
      [@@deriving typed_variants]

      let to_option : t -> a option = function
        | None -> None
        | Some a -> Some a
      ;;

      let of_option : a option -> t = function
        | None -> None
        | Some a -> Some a
      ;;
    end
    in
    let%map.Bonsai form =
      Typed_manual.Variant.make
        (module struct
          module Typed_variant = M.Typed_variant

          type picker_view = Vdom.Node.t
          type variant_view = view option
          type resulting_view = Vdom.Node.t * view option

          let form_for_variant
            : type a.
              a Typed_variant.t -> Bonsai.graph -> (a, view option) Form.t Bonsai.t
            =
            fun typed_variant graph ->
            match typed_variant with
            | None -> Bonsai.return (Form.return () |> Form.map_view ~f:(fun () -> None))
            | Some ->
              let form = form graph in
              let%arr form = form in
              Form.map_view ~f:Option.some form
          ;;

          let form_for_picker =
            Elements.Dropdown.enumerable
              ?extra_attrs
              (module M.Typed_variant.Packed)
              ~to_string:(function
                | { M.Typed_variant.Packed.f = T None } -> none_label
                | { M.Typed_variant.Packed.f = T Some } -> some_label)
          ;;

          let finalize_view picker_view inner _graph =
            match%sub inner with
            | Ok (_, inner) ->
              let%arr inner = inner
              and picker_view = picker_view in
              picker_view, Form.view inner
            | Error _ ->
              let%arr picker_view = picker_view in
              picker_view, None
          ;;
        end)
        graph
    in
    Form.project form ~parse_exn:M.to_option ~unparse:M.of_option
  ;;
end
