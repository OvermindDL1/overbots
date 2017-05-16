

type msg =
  | NothingYet
[@@bs.deriving {accessors}]

(* Message Types *)
type game_msg =
  | TimeMsg of Tea.Time.t * string


(* Resource Storage *)
type resource_flag =
  | Energy
  | IronOxide
  | RawSilicon
module ResourceMap = Map.Make(struct type t = resource_flag let compare = compare end)
type resource_value = float


(* Tagged Data Storage *)
type bool_flag =
  | SolarPanelsReadyToUnfold
  | SolarPanelsGenerating
  | DrillDeployed
module BoolFlagSet = Set.Make(struct type t = bool_flag let compare = compare end)
type bool_flags = BoolFlagSet.t
let init_bool_flags = BoolFlagSet.empty



type int_flag =
  | NoIntFlagsYet
module IntFlagMap = Map.Make(struct type t = int_flag let compare = compare end)
type int_flags = int IntFlagMap.t
let init_int_flags =
  let open IntFlagMap in
  empty
  |> add NoIntFlagsYet 0



type model = {
  msgs : game_msg list;
  resource_values : resource_value ResourceMap.t;
  bool_flags : bool_flags;
  int_flags : int_flags;
}
