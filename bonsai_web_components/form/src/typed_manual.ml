open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Form_manual
module Elements = Elements_manual

let sexp_to_pretty_string sexp_of_t t =
  t
  |> sexp_of_t
  |> Sexp.to_string_mach
  |> String.lowercase
  |> String.map ~f:(function
    | '(' | ')' | '-' | '_' -> ' '
    | o -> o)
;;

module Record = struct
  module type S = sig
    module Typed_field : Typed_fields_lib.S

    type field_view
    type resulting_view

    val form_for_field
      :  'a Typed_field.t
      -> Bonsai.graph
      -> ('a, field_view) Form.t Bonsai.t

    type form_of_field_fn =
      { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Bonsai.t }

    val finalize_view : form_of_field_fn -> Bonsai.graph -> resulting_view Bonsai.t
  end

  module type S' = sig
    include S

    val augment_error : 'a Typed_field.t -> Error.t -> Error.t
  end

  module type S_for_table = sig
    module Typed_field : Typed_fields_lib.S

    val label_for_field
      : [ `Inferred
        | `Computed of 'a Typed_field.t -> string
        | `Dynamic of (Typed_field.Packed.t -> string) Bonsai.t
        ]

    val form_for_field
      :  'a Typed_field.t
      -> Bonsai.graph
      -> ('a, Vdom.Node.t) Form.t Bonsai.t
  end

  let make_shared
    (type a field_view resulting_view)
    (module M : S'
      with type Typed_field.derived_on = a
       and type resulting_view = resulting_view
       and type field_view = field_view)
    graph
    =
    let module Form_value = struct
      type 'a t = Bonsai.graph -> ('a, field_view) Form.t Bonsai.t
    end
    in
    let module App = struct
      type 'a s = ('a, field_view) Form.t
      type 'a t = Bonsai.graph -> 'a Bonsai.t

      let map a ~f graph = Bonsai.map (a graph) ~f
      let all x graph = Bonsai.all (List.map x ~f:(fun x -> x graph))
      let translate = Fn.id
    end
    in
    let module Form' = struct
      type 'a t = ('a, field_view) Form.t
    end
    in
    let module The_form_values = Typed_field_map.Make (M.Typed_field) (Form_value) in
    let module The_forms = Typed_field_map.Make (M.Typed_field) (Form') in
    let module The_results = Typed_field_map.Make (M.Typed_field) (Or_error) in
    let module To_forms = The_form_values.As_applicative.To_other_map (App) (The_forms) in
    let form_values_per_field =
      let f field graph =
        let subform = M.form_for_field field graph in
        let%arr subform = subform in
        Form.map_error subform ~f:(M.augment_error field)
      in
      The_form_values.create { f }
    in
    let forms_per_field = To_forms.run form_values_per_field graph in
    let lookup field =
      let%map forms_per_field = forms_per_field in
      The_forms.find forms_per_field field
    in
    let view = M.finalize_view { f = lookup } graph in
    let%arr forms_per_field = forms_per_field
    and view = view in
    let value =
      let f field = Form.value (The_forms.find forms_per_field field) in
      The_results.As_applicative.transpose
        (module Or_error)
        ~create:(fun { f } -> M.Typed_field.create { f })
        (The_results.create { f })
    in
    let set r =
      M.Typed_field.Packed.all
      |> List.map ~f:(fun { f = T field } ->
        Form.set (The_forms.find forms_per_field field) (M.Typed_field.get field r))
      |> Vdom.Effect.Many
    in
    { Form.view; value; set }
  ;;

  let make
    (type a field_view resulting_view)
    (module M : S
      with type Typed_field.derived_on = a
       and type resulting_view = resulting_view
       and type field_view = field_view)
    =
    make_shared
      (module struct
        include M

        let augment_error field error =
          Error.tag error ~tag:("in field " ^ M.Typed_field.name field)
        ;;
      end)
  ;;

  let make_without_tagging_errors
    (type a field_view resulting_view)
    (module M : S
      with type Typed_field.derived_on = a
       and type resulting_view = resulting_view
       and type field_view = field_view)
    =
    make_shared
      (module struct
        include M

        (* Don't augment the error *)
        let augment_error _ error = error
      end)
  ;;

  let make_table
    (type a)
    (module M : S_for_table with type Typed_field.derived_on = a)
    graph
    =
    let module View_value_by_field =
      Typed_field_map.Make
        (M.Typed_field)
        (struct
          type 'a t = Vdom.Node.t Bonsai.t
        end)
    in
    let module View_by_field =
      Typed_field_map.Make
        (M.Typed_field)
        (struct
          type 'a t = Vdom.Node.t
        end)
    in
    let module App = struct
      type 'a s = Vdom.Node.t
      type 'a t = 'a Bonsai.t

      let map a ~f = Bonsai.map a ~f
      let all x = Bonsai.all x
      let translate = Fn.id
    end
    in
    let module To_views =
      View_value_by_field.As_applicative.To_other_map (App) (View_by_field)
    in
    let record =
      make_without_tagging_errors
        (module struct
          include M

          type field_view = Vdom.Node.t
          type resulting_view = View_by_field.t

          type form_of_field_fn =
            { f : 'a. 'a Typed_field.t -> ('a, field_view) Form.t Bonsai.t }

          let finalize_view { f } _graph =
            let f : type a. a Typed_field.t -> Vdom.Node.t Bonsai.t =
              fun field ->
              let%map form = f field in
              Form.view form
            in
            To_views.run (View_value_by_field.create { f })
          ;;
        end)
    in
    let many_records = Elements.Multiple.list record graph in
    let render_table =
      let to_string =
        match M.label_for_field with
        | `Inferred ->
          Bonsai.return (fun t -> sexp_to_pretty_string M.Typed_field.Packed.sexp_of_t t)
        | `Computed field_to_string ->
          Bonsai.return (fun ({ f = T field } : M.Typed_field.Packed.t) ->
            field_to_string field)
        | `Dynamic field_to_string -> field_to_string
      in
      let cols =
        let%arr to_string = to_string in
        let remove_column =
          View.Table.Col.make
            "Remove"
            ~get:(fun (_map, remove) -> remove)
            ~render:(fun theme remove ->
              View.hbox
                ~main_axis_alignment:Center
                [ View.button theme "X" ~on_click:remove ])
        in
        (M.Typed_field.Packed.all
         |> List.map ~f:(fun ({ f = T field } as packed) ->
           View.Table.Col.make
             (to_string packed)
             ~get:(fun (map, _remove) -> View_by_field.find map field)
             ~render:(fun _theme vdom -> vdom)))
        @ [ remove_column ]
      in
      let theme = View.Theme.current graph in
      let%arr cols = cols
      and theme = theme in
      fun ({ items; add_element } : (a, View_by_field.t) Elements.Multiple.t) ->
        let items = List.map items ~f:(fun { form; remove } -> Form.view form, remove) in
        View.vbox
          [ View.Table.render theme cols items
          ; View.button theme "+" ~on_click:add_element
          ]
    in
    let%arr many_records = many_records
    and render_table = render_table in
    Form.map_view many_records ~f:render_table
  ;;
end

module Variant = struct
  module type S = sig
    (**  This module should be generated by deriving [typed_variants] on a
         sum type. *)
    module Typed_variant : Typed_variants_lib.S

    type picker_view
    type variant_view
    type resulting_view

    val form_for_picker
      :  Bonsai.graph
      -> (Typed_variant.Packed.t, picker_view) Form.t Bonsai.t

    val form_for_variant
      :  'a Typed_variant.t
      -> Bonsai.graph
      -> ('a, variant_view) Form.t Bonsai.t

    val finalize_view
      :  picker_view Bonsai.t
      -> ('a Typed_variant.t * ('a, variant_view) Form.t) Or_error.t Bonsai.t
      -> Bonsai.graph
      -> resulting_view Bonsai.t
  end

  module type S_opt = sig
    (**  This module should be generated by deriving [typed_variants] on a
           sum type. *)
    module Typed_variant : Typed_variants_lib.S

    type picker_view
    type variant_view
    type resulting_view

    val form_for_picker
      :  Bonsai.graph
      -> (Typed_variant.Packed.t option, picker_view) Form.t Bonsai.t

    val form_for_variant
      :  'a Typed_variant.t
      -> Bonsai.graph
      -> ('a, variant_view) Form.t Bonsai.t

    val finalize_view
      :  picker_view Bonsai.t
      -> ('a Typed_variant.t * ('a, variant_view) Form.t) option Or_error.t Bonsai.t
      -> Bonsai.graph
      -> resulting_view Bonsai.t
  end

  module Packed_set_form = struct
    type ('a, 'view) t =
      | T :
          { form : (('a, 'cmp) Set.t, 'view) Form.t
          ; comparator : ('a, 'cmp) Bonsai.comparator
          }
          -> ('a, 'view) t
  end

  module type S_set = sig
    include Comparator.S

    (**  This module should be generated by deriving [typed_variants] on a
         sum type. *)
    module Typed_variant : Typed_variants_lib.S with type derived_on = t

    type variant_view
    type resulting_view

    val sexp_of_variant_argument
      : [ `Use_sexp_of_variant | `Custom of 'a Typed_variant.t -> 'a -> Sexp.t ]

    val form_for_variant
      :  'a Typed_variant.t
      -> ('a, 'cmp) Bonsai.comparator
      -> Bonsai.graph
      -> (('a, 'cmp) Set.t, variant_view) Form.t Bonsai.t

    type form_of_variant_fn =
      { f : 'a 'cmp. 'a Typed_variant.t -> ('a, variant_view) Packed_set_form.t Bonsai.t }

    val finalize_view : form_of_variant_fn -> Bonsai.graph -> resulting_view Bonsai.t
  end

  module type S_set' = sig
    include S_set

    val augment_error : 'a Typed_variant.t -> Error.t -> Error.t
  end

  let make
    (type a view picker_view variant_view)
    (module M : S
      with type Typed_variant.derived_on = a
       and type resulting_view = view
       and type variant_view = variant_view
       and type picker_view = picker_view)
    graph
    =
    let%sub { Form.value = picker_value; set = set_picker_value; view = picker_view } =
      M.form_for_picker graph
    in
    let module Packed_with_form = struct
      type t =
        | T :
            { variant : 'a M.Typed_variant.t
            ; form : ('a, M.variant_view) Form.t
            ; finalized_view : M.resulting_view
            }
            -> t
    end
    in
    let%sub view, inner =
      match%sub picker_value with
      | Error e ->
        let picker_error =
          let%arr e = e in
          Error e
        in
        let view = M.finalize_view picker_view picker_error graph in
        let%arr view = view
        and e = e in
        view, Error e
      | Ok picker_value ->
        let inner =
          Bonsai.enum
            (module M.Typed_variant.Packed)
            ~match_:picker_value
            ~with_:(function
              | { f = T variant } ->
                fun graph ->
                  let form = M.form_for_variant variant graph in
                  let variant_and_form =
                    let%arr form = form in
                    Ok (variant, form)
                  in
                  let finalized_view =
                    M.finalize_view picker_view variant_and_form graph
                  in
                  let%arr form = form
                  and finalized_view = finalized_view in
                  Packed_with_form.T { variant; form; finalized_view })
            graph
        in
        let%arr (T { variant = picker_value; form = inner; finalized_view }) = inner in
        let projected =
          let parse_exn content = M.Typed_variant.create picker_value content in
          let unparse kind =
            match M.Typed_variant.get picker_value kind with
            | None ->
              let expected = M.Typed_variant.Packed.pack picker_value in
              let found = M.Typed_variant.which kind in
              raise_s
                [%message
                  "BUG"
                    [%here]
                    (expected : M.Typed_variant.Packed.t)
                    (found : M.Typed_variant.Packed.t)]
            | Some v -> v
          in
          Form.project inner ~parse_exn ~unparse
        in
        finalized_view, Ok projected
    in
    let get_inner_form = Bonsai.peek inner graph in
    let%arr inner = inner
    and set_picker_value = set_picker_value
    and get_inner_form = get_inner_form
    and view = view in
    let set value =
      let constructor = M.Typed_variant.which value in
      let open Ui_effect.Let_syntax in
      (* sequence this so that the result of evaluating the picker is visible
         when setting the innermost form *)
      let%bind () = set_picker_value constructor in
      match%bind.Effect get_inner_form with
      | Active (Ok inner) -> Form.set inner value
      | Active (Error e) -> Effect.print_s [%sexp "BUG", [%here], (e : Error.t)]
      | Inactive -> Effect.never
    in
    let value =
      match inner with
      | Error e -> Error e
      | Ok form -> form.value
    in
    { Form.view; value; set }
  ;;

  let make_optional
    (type a view picker_view variant_view)
    (module M : S_opt
      with type Typed_variant.derived_on = a
       and type resulting_view = view
       and type variant_view = variant_view
       and type picker_view = picker_view)
    graph
    =
    let module Transformed = struct
      module Original = struct
        type t = M.Typed_variant.derived_on

        module Typed_variant = M.Typed_variant
      end

      (* This type is effectively an option, however, we use [N] and [S] instead of [None]
         and [Some] to make it clear which type we're interacting with at any given time.
         This also avoids the need for some random-looking type annotations. *)
      type t =
        | N
        | S of Original.t [@subvariant]
      [@@deriving typed_variants, variants]

      let of_option (opt : Original.t option) : t =
        match opt with
        | None -> N
        | Some x -> S x
      ;;

      let to_option (opt : t) : Original.t option =
        match opt with
        | N -> None
        | S x -> Some x
      ;;

      type picker_view = M.picker_view
      type variant_view = M.variant_view option
      type resulting_view = M.resulting_view

      let form_for_picker
        : Bonsai.graph -> (Typed_variant.Packed.t, picker_view) Form.t Bonsai.t
        =
        fun graph ->
        let picker_form = M.form_for_picker graph in
        let%arr picker_form = picker_form in
        Form.project
          picker_form
          ~parse_exn:(function
            | None -> ({ f = T Typed_variant.N } : Typed_variant.Packed.t)
            | Some { f = T variant } -> { f = T (S variant) })
          ~unparse:(function
            | { f = T Typed_variant.N } -> None
            | { f = T (S variant) } -> Some { f = T variant })
      ;;

      let form_for_variant
        : type a. a Typed_variant.t -> Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
        =
        fun typed_field graph ->
        match typed_field with
        | N ->
          Bonsai.return
            (Form.return () |> Form.map_view ~f:(fun () : variant_view -> None))
        | S variant ->
          let%map.Bonsai form = M.form_for_variant variant graph in
          Form.map_view form ~f:Option.some
      ;;

      let finalize_view
        (type a)
        (picker_view : picker_view Bonsai.t)
        (selected_clause_view :
          (a Typed_variant.t * (a, variant_view) Form.t) Or_error.t Bonsai.t)
        graph
        =
        match%sub selected_clause_view with
        | Error e ->
          let error =
            let%arr e = e in
            Error e
          in
          M.finalize_view picker_view error graph
        | Ok (N, _) -> M.finalize_view picker_view (Bonsai.return (Ok None)) graph
        | Ok (S selected_clause, form) ->
          let variant_and_form =
            let%arr selected_clause = selected_clause
            and form = form in
            Ok
              (Some
                 ( selected_clause
                 , Form.map_view form ~f:(function
                     | None ->
                       raise_s [%message "Unexpected form view in [make_optional]"]
                     | Some view -> view) ))
          in
          M.finalize_view picker_view variant_and_form graph
      ;;
    end
    in
    make (module Transformed) graph
    |> Bonsai.map
         ~f:(Form.project ~parse_exn:Transformed.to_option ~unparse:Transformed.of_option)
  ;;

  let make_set_shared
    (type a cmp resulting_view)
    (module M : S_set'
      with type t = a
       and type comparator_witness = cmp
       and type resulting_view = resulting_view)
    : Bonsai.graph -> ((a, cmp) Set.t, resulting_view) Form.t Bonsai.t
    =
    fun graph ->
    let module Packed = struct
      include M.Typed_variant.Packed
      include Comparable.Make_plain (M.Typed_variant.Packed)
    end
    in
    let module Form_with_comparator = struct
      type 'a t =
        | T :
            { cmp : ('a, 'cmp) Bonsai.comparator
            ; form : (('a, 'cmp) Set.t, M.variant_view) Form.t
            ; wrap : ('a, 'cmp) Set.t -> (a, cmp) Set.t
            ; unwrap_exn : (a, cmp) Set.t -> ('a, 'cmp) Set.t
            }
            -> 'a t
    end
    in
    let module Form_computations =
      Typed_field_map.Make
        (M.Typed_variant)
        (struct
          type 'a t = Bonsai.graph -> 'a Form_with_comparator.t Bonsai.t
        end)
    in
    let module Forms = Typed_field_map.Make (M.Typed_variant) (Form_with_comparator) in
    let module To_forms =
      Form_computations.As_applicative.To_other_map
        (struct
          type 'a s = 'a Form_with_comparator.t
          type 'a t = Bonsai.graph -> 'a Bonsai.t

          let map a ~f graph = Bonsai.map (a graph) ~f
          let all x graph = Bonsai.all (List.map x ~f:(fun x -> x graph))
          let translate = Fn.id
        end)
        (Forms)
    in
    let create_form_with_comparator (type a) (variant : a M.Typed_variant.t) graph =
      let wrap = M.Typed_variant.create variant in
      (* This is safe because this function only ever gets types of this argument. *)
      let unwrap_exn v = M.Typed_variant.get variant v |> Option.value_exn in
      let module Comparator = struct
        module T = struct
          type t = a

          let sexp_of_t =
            match M.sexp_of_variant_argument with
            | `Use_sexp_of_variant -> fun a -> M.comparator.sexp_of_t (wrap a)
            | `Custom f -> f variant
          ;;

          let compare = Comparable.lift ~f:wrap M.comparator.compare
        end

        include T
        include Comparator.Make (T)
      end
      in
      let form = M.form_for_variant variant (module Comparator) graph in
      let%arr form = form in
      let form = Form.map_error form ~f:(M.augment_error variant) in
      Form_with_comparator.T
        { cmp = (module Comparator)
        ; wrap = Set.map (module M) ~f:wrap
        ; form
        ; unwrap_exn = Set.map (module Comparator) ~f:unwrap_exn
        }
    in
    let form_computations =
      Form_computations.create { f = create_form_with_comparator }
    in
    let forms = To_forms.run form_computations graph in
    let view =
      let lookup variant =
        let%map forms = forms in
        let (Form_with_comparator.T { form; cmp; _ }) = Forms.find forms variant in
        Packed_set_form.T { form; comparator = cmp }
      in
      M.finalize_view { f = lookup } graph
    in
    let%arr forms = forms
    and view = view in
    let value =
      List.map Packed.all ~f:(fun { Packed.f = T variant } ->
        let (Form_with_comparator.T { form; wrap; _ }) = Forms.find forms variant in
        Form.value form |> Or_error.map ~f:wrap)
      |> Or_error.all
      |> Or_error.map ~f:(Set.union_list (module M))
    in
    let set set =
      let values =
        Set.fold set ~init:Packed.Map.empty ~f:(fun map value ->
          Map.update map (M.Typed_variant.which value) ~f:(function
            | None -> Set.singleton (module M) value
            | Some set -> Set.add set value))
      in
      Map.to_alist values
      |> List.map ~f:(fun ({ f = T variant }, values) ->
        let (Form_with_comparator.T { form; unwrap_exn; _ }) = Forms.find forms variant in
        Form.set form (unwrap_exn values))
      |> Ui_effect.all_unit
    in
    { Form.view; value; set }
  ;;

  let make_set_without_tagging_errors
    (type a cmp variant_view resulting_view)
    (module M : S_set
      with type t = a
       and type comparator_witness = cmp
       and type resulting_view = resulting_view
       and type variant_view = variant_view)
    =
    make_set_shared
      (module struct
        include M

        (* Don't augment the error *)
        let augment_error _ error = error
      end)
  ;;

  let make_set
    (type a cmp variant_view resulting_view)
    (module M : S_set
      with type t = a
       and type comparator_witness = cmp
       and type resulting_view = resulting_view
       and type variant_view = variant_view)
    =
    make_set_shared
      (module struct
        include M

        let augment_error field error =
          Error.tag error ~tag:("in field " ^ M.Typed_variant.name field)
        ;;
      end)
  ;;
end
