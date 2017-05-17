
open Overbots_types
open Overbots_flags

module type Resource = sig
  val id : resource_flag
  val shown : model -> bool
  val get_value_range : model -> resource_value * resource_value
end


module Energy : Resource = struct
  let id = Energy
  let shown model = bool_flag_exists InternalPowerEnabled model
  let get_value_range _model = 0.0, 100.0
end

module IronOxide : Resource = struct
  let id = IronOxide
  let shown model = bool_flag_exists DrillDeployed model
  let get_value_range _model = 0.0, 10.0
end

module RawSilicon : Resource = struct
  let id = RawSilicon
  let shown model = bool_flag_exists DrillDeployed model
  let get_value_range _model = 0.0, 2.0
end



let all_resources =
  let open ResourceMap in
  empty
  |> add Energy (module Energy : Resource)
  |> add IronOxide (module IronOxide : Resource)
  |> add RawSilicon (module RawSilicon : Resource)


let displayed_resources = [
  ("", "global", [
      Energy, "Energy", "energy";
    ]);
  ("Raw", "raw", [
      IronOxide, "Iron Oxide", "ironoxide";
      RawSilicon, "Raw Silicon", "rawsilicon";
    ]);
]



let get_resource_module rid =
  ResourceMap.find rid all_resources


let get_resource_value rid model =
  ResourceMap.find rid model.resource_values


type resource_value_state =
  | ValueTooLow
  | ValueTooHigh of model * resource_value
  | ValueSuccess of model

let set_resource_value rid value model =
  let module R = (val get_resource_module rid) in
  let rmin, rmax = R.get_value_range model in
  if value < rmin then ValueTooLow else
  if value > rmax then
    let resource_values = ResourceMap.add rid rmax model.resource_values in
    ValueTooHigh ({model with resource_values}, value-.rmax)
  else
    let resource_values = ResourceMap.add rid value model.resource_values in
    ValueSuccess {model with resource_values}

let add_resource_value rid delta model =
  let value = delta +. get_resource_value rid model in
  set_resource_value rid value model


let cost_resource rid delta model =
  match add_resource_value rid (-.delta) model with
  | ValueTooLow -> None
  | ValueTooHigh _ -> None
  | ValueSuccess model -> Some model

let cost_resources resources model =
  let aux model (rid, amt) = match model with
    | None -> None
    | Some model -> cost_resource rid amt model
  in List.fold_left aux (Some model) resources


let init_resources_values =
  let resource_folder rid _r acc =
    ResourceMap.add rid 0.0 acc in
  ResourceMap.fold resource_folder all_resources ResourceMap.empty
