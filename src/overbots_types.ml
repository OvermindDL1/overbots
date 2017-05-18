


(* Message Types *)
type game_msg =
  | TimeMsg of Tea.Time.t * string
[@@bs.deriving {accessors}]


(* Resource Storage *)
type resource_flag =
  | Energy
  | IronOxide
  | RawSilicon
[@@bs.deriving {accessors}]
module ResourceMap = Map.Make(struct type t = resource_flag let compare = compare end)
type resource_value = float


(* Tagged Data Storage *)
type bool_flag =
  | InternalPowerEnabled
  | SolarPanelsReadyToUnfold
  | SolarPanelsGenerating
  | DrillDeployed
[@@bs.deriving {accessors}]
module BoolFlagSet = Set.Make(struct type t = bool_flag let compare = compare end)
type bool_flags = BoolFlagSet.t
let init_bool_flags = BoolFlagSet.empty



type int_flag =
  | TimeActionIdx
[@@bs.deriving {accessors}]
module IntFlagMap = Map.Make(struct type t = int_flag let compare = compare end)
type int_flags = int IntFlagMap.t
let init_int_flags =
  let open IntFlagMap in
  empty
  |> add TimeActionIdx 0



type float_flag =
  | BasicSolarPanelSelfGeneration
module FloatFlagMap = Map.Make(struct type t = float_flag let compare = compare end)
type float_flags = float FloatFlagMap.t
let init_float_flags =
  let open FloatFlagMap in
  empty
  |> add BasicSolarPanelSelfGeneration 0.0


type button_id =
  | UnfoldSolarPanels
  | DeployDrill


type resource_transformation =
  | Generate of resource_flag * resource_value
  | Consume of resource_flag * resource_value
type resource_transformations = resource_transformation list
type cache_resource_transformation = string * resource_transformations

module type UTransformer = sig
  type model
  val name : model -> string
  val enabled : model -> bool
  val transformers : model -> resource_transformations
end


type msg =
  | UpdateFrame of Tea.AnimationFrame.t
  | ActionButtonClicked of button_id
  | LoadData of string
  | SaveData
[@@bs.deriving {accessors}]


type cache = {
  transformers : (module UTransformer with type model = model) list;
  resource_deltas : resource_value ResourceMap.t;
}
and model = {
  start_realtime : Tea.Time.t;
  current_realtime : Tea.Time.t;
  gametime : Tea.Time.t;
  msgs : game_msg list;
  resource_values : resource_value ResourceMap.t;
  bool_flags : bool_flags;
  int_flags : int_flags;
  float_flags : float_flags;
  cache : cache;
}
[@@bs.deriving {accessors}]

module type Transformer = UTransformer with type model = model
