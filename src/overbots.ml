open Tea
open Overbots_types

let init () =
  let model = {
    msgs = [];
    resource_values = Overbots_resource.init_resources_values;
    bool_flags = init_bool_flags;
    int_flags = init_int_flags;
  } in
  (model, Cmd.none)

let update model = function
  | NothingYet -> (model, Cmd.none)

let subscriptions _model =
  Sub.none

let main =
  App.standardProgram {
    init;
    update;
    view = Overbots_view.view;
    subscriptions;
  }
