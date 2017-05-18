
open Overbots_types
open Overbots_flags

module type Resource = sig
  val id : resource_flag
  val shown : model -> bool
  val get_value_range : model -> resource_value * resource_value
  val idname : string
  val name : model -> string
end


module Energy : Resource = struct
  let id = Energy
  let shown model = bool_flag_exists InternalPowerEnabled model
  let get_value_range _model = 0.0, 100.0
  let idname = "energy"
  let name _model = "Energy"
end

module IronOxide : Resource = struct
  let id = IronOxide
  let shown model = bool_flag_exists DrillDeployed model
  let get_value_range _model = 0.0, 10.0
  let idname = "ironoxide"
  let name _model = "Iron Oxide"
end

module RawSilicon : Resource = struct
  let id = RawSilicon
  let shown model = bool_flag_exists DrillDeployed model
  let get_value_range _model = 0.0, 2.0
  let idname = "rawsilicon"
  let name _model = "RawSilicon"
end



let all_resources = [
  (module Energy : Resource);
  (module IronOxide);
  (module RawSilicon);
]

let id_resource_mapping =
  List.fold_left (fun map r ->
      let module R = (val r : Resource) in
      ResourceMap.add R.id r map
    ) ResourceMap.empty all_resources

module StringMap = Map.Make(String)
let idname_resource_mapping =
  List.fold_left (fun map r ->
      let module R = (val r : Resource) in
      StringMap.add R.idname r map
    ) StringMap.empty all_resources


let get_resource_module rid =
  ResourceMap.find rid id_resource_mapping

let get_resource_module_by_idname idname =
  if StringMap.mem idname idname_resource_mapping
  then Some (StringMap.find idname idname_resource_mapping)
  else None

let displayed_resources = [
  ("", "global", [
      get_resource_module Energy;
    ]);
  ("Raw", "raw", [
      get_resource_module IronOxide;
      get_resource_module RawSilicon;
    ]);
]




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
  let resource_folder acc r =
    let module R = (val r : Resource) in
    ResourceMap.add R.id 0.0 acc in
  List.fold_left resource_folder ResourceMap.empty all_resources


let init_cache = {
  transformers = [];
  resource_deltas = init_resources_values;
}

let reset_cache model =
  let cache = init_cache in
  {model with cache}
