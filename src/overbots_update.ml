
open Tea
open Overbots_types


let max_msg_count = 20

let pruneOldMsgs msgs =
  let rec aux i acc = function
    | [] -> List.rev acc
    | h :: t -> if i = 0 then List.rev acc
      else aux (i-1) (h :: acc) t  in
  aux max_msg_count [] msgs


let update_state model new_time =
  let time = new_time -. model.start_realtime in
  let model, ta_cmds = Overbots_actions.update_timeactions model time in
  let model, t_cmds = Overbots_transformers.update_transformations model time in
  let msgs = pruneOldMsgs model.msgs in
  let model = {model with gametime = time; current_realtime = new_time; msgs} in
  (model, Cmd.batch [ta_cmds; t_cmds])
