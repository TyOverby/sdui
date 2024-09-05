open! Core
open! Codemirror
open Codemirror_rainbow_parentheses_kernel

module Colors = struct
  (* An array of classes *)
  type t = string Iarray.t

  let create ~classes : t = Nonempty_list.to_list classes |> Iarray.of_list

  let default =
    (* Colors were picked to look good on light and dark backgrounds as well as being
       color-blind friendly. *)
    lazy
      (let module Sheet =
         [%css
         stylesheet
           {|
             .rainbow-colors-0 {
               color: #e69f00;
             }
             .rainbow-colors-1 {
               color: #56b4e9;
             }
             .rainbow-colors-2 {
               color: #009e73;
             }
             .rainbow-colors-3 {
               color: #d0c100;
             }
             .rainbow-colors-4 {
               color: #0072b2;
             }
             .rainbow-colors-5 {
               color: #d55e00;
             }
             .rainbow-colors-6 {
               color: #cc79a7;
             }
             |}]
       in
      create
        ~classes:
          Sheet.For_referencing.
            [ rainbow_colors_0
            ; rainbow_colors_1
            ; rainbow_colors_2
            ; rainbow_colors_3
            ; rainbow_colors_4
            ; rainbow_colors_5
            ; rainbow_colors_6
            ])
  ;;
end

module Extension_state = struct
  type t =
    { mutable checkpoints :
        View.Decoration.t State.Range.t Symbolic_automaton_based.Checkpoint_data.t
          Checkpoints.t
    ; mutable visible_decoration_set : View.Decoration_set.t
    }

  let new_state () =
    { visible_decoration_set = State.Range_set.empty; checkpoints = Checkpoints.empty }
  ;;
end

module Text_functions : Symbolic_automaton_based.Text with type t = Text.Text.t = struct
  type t = Text.Text.t

  let fold_text text ~init ~f =
    let iter = Text.Text.iter text () in
    let rec loop iterator ~acc =
      if Text.Text_iterator.done_ iter
      then acc
      else (
        let acc = String.fold ~f ~init:acc (Text.Text_iterator.value iterator) in
        loop (Text.Text_iterator.next iterator ()) ~acc)
    in
    loop iter ~acc:init
  ;;

  let foldi text ~init ~f =
    fold_text text ~init:(0, init) ~f:(fun (i, acc) char -> i + 1, f i acc char) |> snd
  ;;

  let length = Text.Text.length
  let suffix text len = Text.Text.slice text ~from:(length text - len) ()
end

(* The viewport extends beyond the visible range by around 80% (according to some manual
   testing); these do not get the index of the first or last visible character. *)
let get_viewport_start view =
  View.Editor_view.viewport view |> View.Editor_view.Viewport.from
;;

let get_viewport_end view =
  View.Editor_view.viewport view |> View.Editor_view.Viewport.to_
;;

let highlight text_slice ~(state : Extension_state.t) ~start_of_edit ~colors =
  let checkpoints_before_edit =
    Checkpoints.discard_checkpoints state.checkpoints ~after:start_of_edit
  in
  let res =
    Symbolic_automaton_based.highlight
      (module Text_functions)
      text_slice
      ~checkpoints:checkpoints_before_edit
      ~combine:(fun ~index ~nesting ~decorations ->
        let nesting = nesting % Iarray.length colors in
        View.Decoration.(
          range
            ~from:index
            ~to_:(index + 1)
            (mark (Mark_spec.create ~class_:(Iarray.get colors nesting) ())))
        :: decorations)
  in
  state.checkpoints <- Symbolic_automaton_based.Highlight_result.get_all_checkpoints res
;;

let get_visible_decorations (state : Extension_state.t) ~view_start ~view_end =
  Checkpoints.get_checkpoints_around_range state.checkpoints ~min:view_start ~max:view_end
  |> List.concat_map ~f:(fun (_, checkpoint) ->
    Symbolic_automaton_based.Checkpoint_data.get_decorations_since_last_checkpoint
      checkpoint)
  |> View.Decoration.set ~sort:true
;;

let decorate_view view ~state ~start_of_edit ~colors =
  let view_start = get_viewport_start view in
  let view_end = get_viewport_end view in
  View.Editor_view.state view
  |> State.Editor_state.doc
  |> (fun t -> Text.Text.slice t ~from:0 ~to_:view_end ())
  |> highlight ~state ~start_of_edit ~colors;
  state.visible_decoration_set <- get_visible_decorations state ~view_start ~view_end
;;

let update_view (state : Extension_state.t) update ~colors =
  let doc_changed = View.View_update.doc_changed update in
  let viewport_changed = View.View_update.viewport_changed update in
  match doc_changed || viewport_changed with
  | true ->
    let start_of_edit = ref Int.max_value in
    State.Change_set.iter_changes
      (View.View_update.changes update)
      ~f:(fun ~from_a ~to_a:_ ~from_b:_ ~to_b:_ ~inserted:_ ->
        start_of_edit := Int.min !start_of_edit from_a);
    decorate_view
      (View.View_update.view update)
      ~state
      ~start_of_edit:!start_of_edit
      ~colors
  | false -> ()
;;

let create_plugin view ~colors : Extension_state.t View.Plugin_value.t =
  let ext_state = Extension_state.new_state () in
  decorate_view view ~state:ext_state ~start_of_edit:0 ~colors;
  { update = update_view ext_state ~colors |> Some; custom_state = ext_state }
;;

let get_decorations (plugin : Extension_state.t View.Plugin_value.t) =
  plugin.custom_state.visible_decoration_set
;;

let extension ?(colors = Lazy.force Colors.default) () =
  View.View_plugin.define
    ~create:(create_plugin ~colors)
    ~spec:(View.Plugin_spec.create ~decorations:(Some get_decorations))
    ()
  |> View.View_plugin.extension
;;
