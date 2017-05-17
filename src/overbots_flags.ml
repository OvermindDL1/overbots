
open Overbots_types

let bool_flag_exists fid model =
  BoolFlagSet.mem fid model.bool_flags

let bool_flag_set fid model =
  let bool_flags = BoolFlagSet.add fid model.bool_flags in
  {model with bool_flags}

let bool_flag_reset fid model =
  let bool_flags = BoolFlagSet.remove fid model.bool_flags in
  {model with bool_flags}


let int_flag_value fid model =
  IntFlagMap.find fid model.int_flags

let int_flag_set fid value model =
  let int_flags = IntFlagMap.add fid value model.int_flags in
  {model with int_flags}

let int_flag_add fid delta model =
  let value = delta + int_flag_value fid model in
  int_flag_set fid value model


let float_flag_value fid model =
  FloatFlagMap.find fid model.float_flags

let float_flag_set fid value model =
  let float_flags = FloatFlagMap.add fid value model.float_flags in
  {model with float_flags}

let float_flag_add fid delta model =
  let value = delta +. float_flag_value fid model in
  float_flag_set fid value model
