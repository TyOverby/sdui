open! Core
open! Bonsai_web

val img2img_impl
  :  direction:[ `Horizontal | `Vertical ]
  -> pool:(Sd.Hosts.Host.t, 'a, 'b) Lease_pool.t
  -> prev:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> mask:Sd.Image.t Inc.Or_error_or_stale.t option Bonsai.t
  -> prev_params:(Parameters.t, 'c) result Bonsai.t
  -> local_ Bonsai.graph
  -> Single.t

val component
  :  pool:(Sd.Hosts.Host.t, 'a, 'b) Lease_pool.t
  -> index:int Bonsai.t
  -> prev:Sd.Image.t Inc.Or_error_or_stale.t Bonsai.t
  -> prev_params:(Parameters.t, Error.t) result Bonsai.t
  -> local_ Bonsai.graph
  -> (Sd.Image.t Inc.Or_error_or_stale.t
     * (Bonsai.Path.t, unit) Route.t
     * (Bonsai.Path.t, Workspace.t, Bonsai.Path.comparator_witness) Map_intf.Map.t)
       Bonsai.t
