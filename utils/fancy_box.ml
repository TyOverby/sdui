open! Core
open! Bonsai_web

let test_box ~inner ~outer ~opacity ~gap ~blur contents =
  let ~color:inner_color, ~radius:inner_radius, ~border:inner_border = inner in
  let ~color:outer_color, ~radius:outer_radius, ~border:outer_border = outer in
  let gap_px = sprintf "%dpx" gap in
  let blur_px = sprintf "%dpx" blur in
  let gap_minus_one_px = sprintf "%dpx" (gap - 1) in
  let inner_radius_px = sprintf "%dpx" inner_radius in
  let outer_radius_px = sprintf "%dpx" outer_radius in
  {%html| 
      <div style="display: grid; grid-template-rows: auto; grid-template-columns: auto; ">
        <div style="width:100%; height:100%; grid-area: 1/1/1/1; border-radius:%{outer_radius_px}; overflow:clip; backdrop-filter:blur(%{blur_px})" > 
          <div style="width:100%; height:100%; background:%{outer_color}; border-radius:%{outer_radius_px}; padding:%{gap_px}; contain:paint; opacity:%{Virtual_dom.Dom_float.to_string opacity};" > 
            <div style="width:100%; height:100%; background:%{inner_color}; border-radius:%{inner_radius_px}"> </div>
          </div>
        </div>
        <div style="width:100%; height:100%; border-radius:%{outer_radius_px}; border: %{outer_border}; grid-area: 1/1/1/1; padding: %{gap_minus_one_px}; contain:paint;"> 
          <div style="width:100%; height: 100%; border-radius:%{inner_radius_px}; border:%{inner_border}"> 
            *{contents}
          </div>
        </div>
      </div>
    |}
;;
