open Overbots_types

module E = Tea.Json.Encoder
module D = Tea.Json.Decoder


let json_of_gamemsg = function
  | TimeMsg (time, msg) ->
    E.object_ [
      "time", E.float time;
      "msg", E.string msg;
    ]

let apply_on_tuple2 fun0 fun1 (v0, v1) =
  (fun0 v0, fun1 v1)

let string_of_resource_flag rid =
  let open Overbots_resource in
  let module R = (val get_resource_module rid) in
  R.idname

let string_of_bool_flag = function
  | InternalPowerEnabled -> "InternalPowerEnabled"
  | SolarPanelsReadyToUnfold -> "SolarPanelsReadyToUnfold"
  | SolarPanelsGenerating -> "SolarPanelsGenerating"
  | DrillDeployed -> "DrillDeployed"

let string_of_int_flag = function
  | TimeActionIdx ->  "TimeActionIdx"


let string_of_float_flag = function
  | BasicSolarPanelSelfGeneration -> "BasicSolarPanelSelfGeneration"


let json_string_of_model indent model =
  E.object_ [
    "start_realtime", E.float model.start_realtime;
    "current_realtime", E.float model.current_realtime;
    "gametime", E.float model.gametime;
    "msgs", E.list (List.map json_of_gamemsg model.msgs);
    "resource_values", ResourceMap.bindings model.resource_values |> List.map (apply_on_tuple2 string_of_resource_flag E.float) |> E.object_;
    "bool_flags", BoolFlagSet.elements model.bool_flags |> List.map (fun bf -> bf|>string_of_bool_flag|>E.string) |> E.list;
    "int_flags", IntFlagMap.bindings model.int_flags |> List.map (apply_on_tuple2 string_of_int_flag E.int) |> E.object_;
    "float_flags", FloatFlagMap.bindings model.float_flags |> List.map (apply_on_tuple2 string_of_float_flag E.float) |> E.object_;
    (* "cache" is not serialized *)
  ] |> E.encode indent



let decoder_of_gamemsg =
  D.oneOf [
    D.map2 timeMsg (D.field "time" D.float) (D.field "msg" D.string);
  ]

let decoder_of_resource_values =
  let open Overbots_resource in
  D.keyValuePairs D.float
  |> D.map (List.fold_left (fun map (sid, value) -> match get_resource_module_by_idname sid with
      | None -> map
      | Some r ->
        let module R = (val r : Resource) in
        ResourceMap.add R.id value map
    ) init_resources_values)


let decoder_of_bool_flags =
  D.list (D.string |> D.andThen (function
      | "InternalPowerEnabled" -> D.succeed InternalPowerEnabled
      | "SolarPanelsReadyToUnfold" -> D.succeed SolarPanelsReadyToUnfold
      | "SolarPanelsGenerating" -> D.succeed SolarPanelsGenerating
      | "DrillDeployed" -> D.succeed DrillDeployed
      | str -> D.fail ("Unknown bool_flag of: " ^ str)
    )) |> D.map (List.fold_left (fun set flag -> BoolFlagSet.add flag set) BoolFlagSet.empty )


let decoder_of_int_flags =
  let open Tea.Result in
  D.keyValuePairs D.int
  |> D.andThen (fun lst ->
      match List.fold_left (fun rmap (id, value) ->
          match rmap with
          | Error _ as e -> e
          | Ok map -> match id with
            | "TimeActionIdx" -> Ok (IntFlagMap.add TimeActionIdx value map)
            | unknown -> Error ("Unknown Int Flag: " ^ unknown)
        ) (Ok init_int_flags) lst with
      | Error e -> D.fail e
      | Ok r -> D.succeed r
    )


let decoder_of_float_flags =
  let open Tea.Result in
  D.keyValuePairs D.float
  |> D.andThen (fun lst ->
      match List.fold_left (fun rmap (id, value) ->
          match rmap with
          | Error _ as e -> e
          | Ok map -> match id with
            | "BasicSolarPanelSelfGeneration" -> Ok (FloatFlagMap.add BasicSolarPanelSelfGeneration value map)
            | unknown -> Error ("Unknown Float Flag: " ^ unknown)
        ) (Ok init_float_flags) lst with
      | Error e -> D.fail e
      | Ok r -> D.succeed r
    )


let model_of_json_string json_string =
  let construct_model start_realtime current_realtime gametime msgs resource_values bool_flags int_flags float_flags =
    let cache = Overbots_resource.init_cache in
    {start_realtime; current_realtime; gametime; msgs; resource_values; bool_flags; int_flags; float_flags; cache} in
  let decoder = D.map8 construct_model
    (D.field "start_realtime" D.float)
    (D.field "current_realtime" D.float)
    (D.field "gametime" D.float)
    (D.field "msgs" (D.list decoder_of_gamemsg))
    (D.field "resource_values" decoder_of_resource_values)
    (D.field "bool_flags" decoder_of_bool_flags)
    (D.field "int_flags" decoder_of_int_flags)
    (D.field "float_flags" decoder_of_float_flags)
  in D.decodeString decoder json_string
