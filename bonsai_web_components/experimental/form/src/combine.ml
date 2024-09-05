open! Core
open! Import

let lift ~f component = Arrow_deprecated.Bonsai.map component ~f:(Product.lift ~f)

module T = struct
  type ('result, 'input, 'parsed) t =
    ('input, ('result, 'parsed) Product.t) Arrow_deprecated.Bonsai.t

  include Applicative.Make3_using_map2 (struct
      type nonrec ('result, 'input, 'parsed) t = ('result, 'input, 'parsed) t

      let return value =
        Product.Fields.create ~value ~set:(const Ui_effect.Ignore)
        |> Arrow_deprecated.Bonsai.const
      ;;

      let map2 a b ~f =
        let open Arrow_deprecated.Bonsai.Let_syntax in
        let%map_open a = a
        and b = b in
        let value = f (Product.value a) (Product.value b) in
        let set parsed =
          Vdom.Effect.Many [ Product.set a parsed; Product.set b parsed ]
        in
        Product.Fields.create ~value ~set
      ;;

      let map = `Define_using_map2
    end)
end

include T

module Open_on_rhs_intf = struct
  module type S = sig
    val lift
      :  f:('parsed2 -> 'parsed1)
      -> ('input, ('result, 'parsed1) Product.t) Arrow_deprecated.Bonsai.t
      -> ('input, ('result, 'parsed2) Product.t) Arrow_deprecated.Bonsai.t
  end
end

include
  Applicative.Make_let_syntax3 (T) (Open_on_rhs_intf)
    (struct
      let lift = lift
    end)
