
open Tea
open Overbots_types


let update_state model new_time =
  let time = new_time -. model.start_realtime in
  let model, ta_cmds = Overbots_actions.update_timeactions model time in
  let model = {model with gametime = time; current_realtime = new_time} in
  (model, Cmd.batch [ta_cmds])
