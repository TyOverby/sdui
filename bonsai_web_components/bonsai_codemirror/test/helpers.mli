open! Core
open Bonsai_web
open Bonsai_web_test

(** Sends a transaction to the codemirror editor corresponding to the specified name. *)
val send_transaction
  :  ('result, 'incoming) Handle.t
  -> get_vdom:('result -> Vdom.Node.t)
  -> name:string
  -> Bonsai_web_ui_codemirror.Transaction.t
  -> unit
