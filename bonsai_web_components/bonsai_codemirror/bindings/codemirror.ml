include Custom_ojs_converter
include For_ppx

module State = struct
  include For_ppx.State

  module Range_set_update_spec = struct
    include Range_set_update_spec

    module Filter_spec = struct
      type 'v t =
        { f : from:int -> to_:int -> value:'v -> bool
        ; filter_from : int
        ; filter_to : int
        }
    end

    let create ~add ~sort ~(filter : 'v Filter_spec.t option) =
      match filter with
      | Some filter ->
        create
          ~add
          ~sort
          ~filter:(Some filter.f)
          ~filter_from:(Some filter.filter_from)
          ~filter_to:(Some filter.filter_to)
      | None -> create ~add ~sort ~filter:None ~filter_from:None ~filter_to:None
    ;;
  end

  module Range_set = struct
    include Range_set

    let between t ~from ~to_ ~f =
      between t ~from ~to_ ~f:(fun ~from ~to_ ~value ->
        match f ~from ~to_ ~value with
        | `Stop -> Some false
        | `Continue -> None)
    ;;
  end
end
