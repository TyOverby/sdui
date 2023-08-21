open! Core
open! Bonsai_web
open! Async_kernel

module Style = [%css stylesheet {|
  body {
      font-size: 1.1em;
  }

|}]

let component = Tree.component
