open! Core
open! Async_kernel

type t = private
  { reader : string Pipe.Reader.t
  ; writer : string Pipe.Writer.t
  ; close : unit -> unit
  }

val connect : Uri.t -> t Deferred.Or_error.t
