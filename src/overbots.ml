open Tea
open Overbots_types


let serialized_name = "Overbots"

let init () =
  let model = {
    start_realtime = -1.0;
    current_realtime = -1.0;
    gametime = 0.0;
    msgs = [];
    resource_values = Overbots_resource.init_resources_values;
    bool_flags = init_bool_flags;
    int_flags = init_int_flags;
    float_flags = init_float_flags;
    cache = Overbots_resource.init_cache;
  } in
  let load_data_task = Ex.LocalStorage.getItem serialized_name in
  let open Tea.Result in
  (model, Cmd.batch [
      Tea_task.attemptOpt (function | Ok s -> Some (LoadData s) | Error _e -> None) load_data_task;
    ])

let update model = function
  | UpdateFrame timeinfo ->
    let time = timeinfo.time *. 0.001 in
    let model =
      if model.start_realtime >= 0.0 then
        model
      else
        {model with start_realtime = time; current_realtime = time; gametime = 0.0}
    in Overbots_update.update_state model time
  | ActionButtonClicked bid ->
    Overbots_buttons.perform_button model bid
  | LoadData "" -> (model, Cmd.none)
  | LoadData json_string ->
    let open Tea.Result in
    begin match Overbots_serialization.model_of_json_string json_string with
      | Error _e -> (model, Tea_task.attemptOpt (fun _ -> None) (Ex.LocalStorage.setItem serialized_name ""))
      | Ok model ->
        (model, Cmd.none)
    end
  | SaveData ->
    let json_string = Overbots_serialization.json_string_of_model 0 model in
    (model, Tea_task.attemptOpt (fun _ -> None) (Ex.LocalStorage.setItem serialized_name json_string))

let subscriptions _model =
  Sub.batch [
    AnimationFrame.every updateFrame;
    Time.every (10.0 *. Time.second) (fun _ -> SaveData)
  ]

let main =
  App.standardProgram {
    init;
    update;
    view = Overbots_view.view;
    subscriptions;
  }
