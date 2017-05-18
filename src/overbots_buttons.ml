open Overbots_types
open Overbots_actions
open Overbots_flags
open Overbots_resource


let displayed_buttons = [
  ("Perform", "perform", [
    UnfoldSolarPanels, "unfold-solar-panels", "Unfold Solar Panels";
    DeployDrill, "deploy-drill", "Deploy Drill";
    ]);
]


let button_cost _model = function
  | UnfoldSolarPanels -> [Energy, 50.0]
  | DeployDrill -> [Energy, 25.0]


let button_enabled model = function
  | UnfoldSolarPanels -> not (bool_flag_exists SolarPanelsGenerating model) && bool_flag_exists SolarPanelsReadyToUnfold model
  | DeployDrill -> not (bool_flag_exists DrillDeployed model) && bool_flag_exists SolarPanelsGenerating model


let button_temporarily_disabled model = function
  | DeployDrill -> (cost_resources (button_cost model DeployDrill) model) == None
  | button -> (cost_resources (button_cost model button) model) == None


let button_actions _model = function
  | UnfoldSolarPanels -> [ActionSetFloatFlag (BasicSolarPanelSelfGeneration, 1.0); ActionSetBoolFlag SolarPanelsGenerating; ActionClearBoolFlag SolarPanelsReadyToUnfold; ActionAddMsg "Energy is now being generated, now to acquire simple minerals by drilling"]
  | DeployDrill -> [ActionSetBoolFlag DrillDeployed; ActionAddMsg "Now that I've started acquiring resources I need to activate my internal refineries to prepare the resources for use"]


let perform_button orig_model id =
  let open Tea in
  if not (button_enabled orig_model id) || (button_temporarily_disabled orig_model id) then (orig_model, Cmd.none) else
    match cost_resources (button_cost orig_model id) orig_model with
    | None -> (orig_model, Cmd.none)
    | Some model ->
      let model = perform_actions model (button_actions model id) in
      match id with
      | _ -> (model, Cmd.none)
