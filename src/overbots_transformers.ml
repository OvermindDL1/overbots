
open Overbots_types
open Overbots_flags
open Overbots_resource



module BaseSolarGeneration : Transformer = struct
  type model = Overbots_types.model
  let name _model = "Sunlight"
  let enabled _model = true
  let transformers model =
    [ Generate (Energy, float_flag_value BasicSolarPanelSelfGeneration model) ]
end

module DrillEnabled : Transformer = struct
  type model = Overbots_types.model
  let name _model = "Internal Drilling"
  let enabled model = bool_flag_exists DrillDeployed model
  let transformers _model = [
    Consume (Energy, 0.5);
    Generate (IronOxide, 0.2);
    Generate (RawSilicon, 0.1);
  ]
end


(* In processing order *)
let all_transformers = [
  (module BaseSolarGeneration : Transformer);
  (module DrillEnabled);
]

let enabled_transformers model =
  all_transformers
  |> List.filter (fun (module T : Transformer) ->
      T.enabled model
    )


let transformer_delta = function
  | Generate (rid, delta) -> (rid, delta)
  | Consume (rid, ndelta) -> (rid, -.ndelta)


let calculate_resource_delta model map (module T : Transformer) =
  List.fold_left (fun map transformation ->
      let rid, delta = transformer_delta transformation in
      let delta = delta +. ResourceMap.find rid map in
      ResourceMap.add rid delta map
    ) map (T.transformers model)

let calculate_resource_deltas model transformers =
  List.fold_left (calculate_resource_delta model) init_resources_values transformers


let calculate_delta_to_next_filled model rid delta old_time =
    if delta = 0.0 then old_time else
    let value = ResourceMap.find rid model.resource_values in
    let module R = (val get_resource_module rid) in
    let rmin, rmax = R.get_value_range model in
    if value >= rmax || value <= rmin then old_time else
    let at_time = if delta > 0.0 then (rmax-.value) /. delta else (value-.rmin) /. delta in
    if at_time > 0.0 && at_time < old_time
    then at_time
    else old_time

let calculate_deltas_to_next_filled model resource_deltas =
  ResourceMap.fold (calculate_delta_to_next_filled model) resource_deltas max_float


let apply_resource_deltas model resource_deltas cur_time =
  let time_delta = cur_time -. model.gametime in
  ResourceMap.fold (fun rid delta model ->
      let delta = delta *. time_delta in
      match add_resource_value rid delta model with
      | ValueTooLow -> model
      | ValueTooHigh (model, _overrage) -> model
      | ValueSuccess model -> model
    ) resource_deltas model


let rec update_transformations model new_time =
  let transformers, resource_deltas =
    if model.cache.transformers == []
    then
      let transformers = enabled_transformers model in
      transformers, calculate_resource_deltas model transformers
    else model.cache.transformers, model.cache.resource_deltas in
  let time_to_next_filled = model.gametime +. calculate_deltas_to_next_filled model resource_deltas in
  let time_slice = min time_to_next_filled new_time in
  let model = apply_resource_deltas model resource_deltas time_slice in
  if time_slice >= new_time
  then
    let gametime = new_time in
    let cache = {model.cache with transformers; resource_deltas} in
    let model = {model with gametime; cache} in
    (model, Tea.Cmd.none)
  else
    let gametime = time_slice in
    let cache = {model.cache with transformers = []; resource_deltas = Overbots_resource.init_resources_values} in
    let model = {model with gametime; cache} in
    update_transformations model new_time
