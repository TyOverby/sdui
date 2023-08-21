open! Core
open! Bonsai_web

type history_kind :=
  [ `Instruction
  | `Response
  ]

type kind :=
  [ history_kind
  | `Continue
  ]

type t

val make
  :  ?length_penalty:float
  -> ?early_stopping:bool
  -> ?max_new_tokens:int
  -> context:string
  -> history:(history_kind * string) list
  -> kind
  -> t

val send : on_response:(string -> unit Effect.t) -> t -> unit Or_error.t Effect.t
val count_tokens : t -> int Or_error.t Effect.t
