open! Core
open! Async_kernel
open! Bonsai_web.Cont

let dispatch ~host_and_port =
  Deferred.Or_error.try_with (fun () ->
    let%bind.Deferred _ =
      Async_js.Http.request
        ~response_type:Default
        ~headers:[ "Content-Type", "application/json" ]
        (Post None)
        ~url:(sprintf "%s/sdapi/v1/skip" host_and_port)
      |> Deferred.Or_error.ok_exn
    in
    return ())
;;

let dispatch_effect ~host =
  Effect.of_deferred_fun
    (fun host -> dispatch ~host_and_port:((host : Sd.Hosts.Host.t) :> string))
    host
;;
