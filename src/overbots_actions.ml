
open Overbots_types
open Overbots_resource
open Overbots_flags

type action =
  | NoAction
  | ActionAddMsg of string
  | ActionAddResourceAmount of resource_flag * float
  | ActionSetBoolFlag of bool_flag
  | ActionClearBoolFlag of bool_flag
  | ActionSetIntFlag of int_flag * int
  | ActionAddIntFlag of int_flag * int
  | ActionSetFloatFlag of float_flag * float
  | ActionAddFloatFlag of float_flag * float

type actions = action list



let perform_action model = function
  | NoAction -> model
  | ActionAddMsg msg -> {model with msgs = TimeMsg (model.gametime, msg) :: model.msgs} (* TODO:  Need to make a msgs area *)
  | ActionAddResourceAmount (rid, amt) ->
    begin match add_resource_value rid amt model with
      | ValueTooLow -> model
      | ValueTooHigh (model, _left_over) -> model
      | ValueSuccess model -> model
    end
  | ActionSetBoolFlag flag -> bool_flag_set flag model
  | ActionClearBoolFlag flag -> bool_flag_reset flag model
  | ActionSetIntFlag (flag, value) -> int_flag_set flag value model
  | ActionAddIntFlag (flag, delta) -> int_flag_add flag delta model
  | ActionSetFloatFlag (flag, value) -> float_flag_set flag value model
  | ActionAddFloatFlag (flag, delta) -> float_flag_add flag delta model

let perform_actions model actions =
  List.fold_left perform_action model actions


type timeaction = {
  at : Tea.Time.t;
  actions : actions;
}

let init_timeaction at actions = {at; actions}

let timeactions = [|
  init_timeaction 0.0 [ActionAddResourceAmount (Energy, 100.0)];
  init_timeaction 1.0 [ActionAddMsg "Hmm, what is going on?"];
  init_timeaction 3.0 [ActionSetBoolFlag InternalPowerEnabled; ActionSetFloatFlag (BasicSolarPanelSelfGeneration, 100.0); ActionAddMsg "I appear to be getting power through an umbillica interface, however the data connection across it appears to be down..."];
  init_timeaction 5.0 [ActionAddMsg "Running diagnostics..."];
  init_timeaction 7.0 [ActionAddMsg "Minor damage detected, appears to be old micrometeroite impacts, armor has deflected damage from internal systems"];
  init_timeaction 10.0 [ActionAddMsg "Supposed to be getting instructions from the umbillica, and the activation of power from it signifies that I am being activated to work"];
  init_timeaction 12.5 [ActionAddMsg "However, no information has come down, likely the primary craft has been damaged by micrometeroites as well, hence its inability to communicate instructions"];
  init_timeaction 15.0 [ActionAddMsg "Fallback instructions are to acquire resources and prepare for settlement and/or re-acquisition"];
  init_timeaction 20.0 [ActionAddMsg "Velocity sensors are showing that acceleration has not occurred, which should already have happened if I've been reactived"];
  init_timeaction 25.0 [ActionAddMsg "Accelleration is now occurring..."];
  init_timeaction 30.0 [ActionAddMsg "Vector is not changing, which indicates orbital entry is not being accounted for..."];
  init_timeaction 35.0 [ActionAddMsg "Most probable explanation is that the accelleration is from the primary ship entering a planetery atmosphere without the engines firing"];
  init_timeaction 40.0 [ActionAddMsg "The primary ship does have a breaking system that can be deployed in the event of engine failure, the acceleration profile indicates that is what is occuring"];
  init_timeaction 50.0 [ActionAddMsg "Waiting to be deployed..."];
  init_timeaction 60.0 [ActionSetFloatFlag (BasicSolarPanelSelfGeneration, 0.0); ActionAddMsg "Confirmed, deployment has started, primary ship has launched me out in the landing assembly, umbillica is detached from the primary ship"];
  init_timeaction 70.0 [ActionAddMsg "Acceleration profile indicates the landing assembly parachutes have been deployed"];
  init_timeaction 80.0 [ActionSetBoolFlag SolarPanelsReadyToUnfold; ActionAddMsg "Touchdown!  Landing assembly is unfolding.  I now need to deploy my solar energy collectors."];
  init_timeaction max_float []; (* Leae at the end as a sentinal *)
|]


let update_timeactions model time =
  let open Tea in
  let idx = int_flag_value TimeActionIdx model in
  let {at; actions} = timeactions.(idx) in
  if time < at then (model, Cmd.none) else
  let model = perform_actions model actions in
  let model = int_flag_add TimeActionIdx 1 model in
  (model, Cmd.none)
