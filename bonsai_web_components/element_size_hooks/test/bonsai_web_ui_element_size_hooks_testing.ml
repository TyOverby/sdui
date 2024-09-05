open! Core
open Bonsai_web_test

module Bulk_size_tracker = struct
  open Bonsai_web_ui_element_size_hooks

  type change =
    { selector : string
    ; width : float
    ; height : float
    }

  let change_sizes handle ~get_vdom changes =
    Bulk_size_tracker.For_testing.change_sizes
      (List.map changes ~f:(fun { selector; height; width } ->
         ( Handle.get_hook_value
             handle
             ~get_vdom
             ~selector
             ~name:Bulk_size_tracker.For_testing.hook_name
             Bulk_size_tracker.For_testing.type_id
         , { Bulk_size_tracker.Dimensions.width; height } )))
  ;;
end

module Position_tracker = struct
  open Bonsai_web_ui_element_size_hooks

  type change =
    { selector : string
    ; top : int
    ; left : int
    ; width : int
    ; height : int
    }

  let change_positions handle ~get_vdom changes =
    Position_tracker.For_testing.change_positions
      (List.map changes ~f:(fun { selector; top; left; height; width } ->
         ( Handle.get_hook_value
             handle
             ~get_vdom
             ~selector
             ~name:Position_tracker.For_testing.hook_name
             Position_tracker.For_testing.type_id
         , { Position_tracker.Position.top; left; height; width } )))
  ;;
end
