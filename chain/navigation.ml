open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let component ~pool ~hosts_view ~lease_pool_view graph =
  let init_path = Bonsai.path graph in
  let first_image, first_image_view = First_image.component graph in
  let no_prev_params = Bonsai.return (Error (Error.of_string "no previous params")) in
  let selected, set_selected = Bonsai.state_opt graph in
  let%sub route, rest =
    match%sub first_image with
    | None ->
      let { Single.image; gallery_view; form_view; form } =
        Chain.img2img_impl
          ~direction:`Vertical
          ~pool
          ~prev:(Bonsai.return None)
          ~prev_params:no_prev_params
          ~mask:(Bonsai.return None)
          graph
      in
      let rest =
        Chain.component
          ~pool
          ~index:(Bonsai.return 1)
          ~prev:image
          ~prev_params:(form >>| Form.value)
          graph
      in
      let%arr form_view = form_view
      and gallery = gallery_view
      and _, route, rest = rest
      and init_path = init_path
      and first_image_view = first_image_view in
      let route = Route.branch ~key:init_path ~data:() ~children:[ route ] in
      let rest =
        Map.set
          rest
          ~key:init_path
          ~data:(Workspace.for_first_node ~first_image_view ~form_view ~gallery)
      in
      route, rest
    | Some image ->
      let rest =
        Chain.component
          ~pool
          ~index:(Bonsai.return 1)
          ~prev:image
          ~prev_params:no_prev_params
          graph
      in
      let%arr _, route, rest = rest
      and first_image_view = first_image_view
      and init_path = init_path in
      let route = Route.branch ~key:init_path ~data:() ~children:[ route ] in
      let rest =
        Map.set
          rest
          ~key:init_path
          ~data:
            (Workspace.for_first_node
               ~first_image_view
               ~form_view:Vdom.Node.none
               ~gallery:Vdom.Node.none)
      in
      route, rest
  in
  let selected =
    let%arr selected = selected
    and init_path = init_path
    and rest = rest in
    match selected with
    | None -> init_path
    | Some selected -> if Map.mem rest selected then selected else init_path
  in
  Bonsai.Edge.on_change'
    rest
    ~equal:(Bonsai.Path.Map.equal (fun _ _ -> true))
    graph
    ~callback:
      (let%arr selected = selected
       and set_selected = set_selected in
       fun prev next ->
         match prev with
         | None -> Effect.Ignore
         | Some prev ->
           Map.fold_symmetric_diff
             ~data_equal:(fun _ _ -> true)
             prev
             next
             ~init:Effect.Ignore
             ~f:(fun acc ->
               function
               | key, `Right (_ : Workspace.t) ->
                 if [%compare: Bonsai.Path.t] selected key <= 0
                 then set_selected (Some key)
                 else acc
               | _ -> acc));
  let route_view =
    let%arr route = route
    and selected = selected
    and set_selected = set_selected in
    Route.render route ~f:(fun key () ->
      let background =
        if [%equal: Bonsai.Path.t] selected key then "green" else "white"
      in
      Vdom.Node.div
        ~attrs:
          [ {%css| margin: 5px; border-radius:10px; width: 20px; height:20px; background: %{background}; |}
          ; Vdom.Attr.on_click (fun _ -> set_selected (Some key))
          ]
        [])
  in
  let rest =
    Bonsai.assoc
      (module Bonsai.Path)
      rest
      graph
      ~f:(fun path data _graph ->
        let%arr selected = selected
        and path = path
        and data = data
        and hosts_view = hosts_view
        and route_view = route_view
        and lease_pool_view = lease_pool_view in
        let maybe_visible =
          if [%equal: Bonsai.Path.t] selected path
          then {%css||}
          else {%css| display: none; |}
        in
        Vdom.Node.div
          ~attrs:[ maybe_visible ]
          [ Workspace.finalize
              data
              ~hosts:hosts_view
              ~queue:lease_pool_view
              ~route:route_view
          ])
  in
  let%arr rest = rest in
  Vdom_node_with_map_children.make ~tag:"div" rest
;;
