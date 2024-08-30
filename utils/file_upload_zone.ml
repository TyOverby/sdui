open! Core
open! Bonsai_web.Cont
open Js_of_ocaml

module Bindings = struct
  class type data_transfer_item = object
    method kind : Js.js_string Js.t Js.readonly_prop
    method name : Js.js_string Js.t Js.readonly_prop
    method getAsFile : File.file Js.t Js.meth
  end

  class type data_transfer = object
    inherit Dom_html.dataTransfer
    method items : data_transfer_item Js.t Js.js_array Js.t Js.optdef Js.readonly_prop
  end
end

let attr ?mime_types ~on_file_upload () =
  let drag_over = Vdom.Attr.on_dragover (fun _event -> Effect.Prevent_default) in
  let drop =
    Vdom.Attr.on_drop (fun event ->
      let data_transfer : Bindings.data_transfer Js.t = Obj.magic event##.dataTransfer in
      let respond_with =
        match Js.Optdef.to_option data_transfer##.items with
        | Some items ->
          let out = Js.array [||] in
          for i = 0 to items##.length - 1 do
            match Js.Optdef.to_option (Js.array_get items i) with
            | Some item ->
              if phys_equal item##.kind (Js.string "file")
              then ignore (out##push item##getAsFile)
              else ()
            | None -> ()
          done;
          Array.to_list (Js.to_array out)
        | None ->
          let items = data_transfer##.files in
          let out = Js.array [||] in
          for i = 0 to items##.length - 1 do
            match Js.Opt.to_option (items##item i) with
            | Some item -> ignore (out##push item)
            | None -> ()
          done;
          Array.to_list (Js.to_array out)
      in
      let respond_with =
        match mime_types with
        | None -> respond_with
        | Some mime_types ->
          List.filter respond_with ~f:(fun file ->
            List.mem mime_types ~equal:String.equal (Js.to_string file##._type))
      in
      Effect.Many [ on_file_upload respond_with; Effect.Prevent_default ])
  in
  Vdom.Attr.many [ drag_over; drop ]
;;
