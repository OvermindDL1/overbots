
open Overbots_types

let bool_flag_exists fid model =
  BoolFlagSet.mem fid model.bool_flags

let int_flag_value fid model =
  IntFlagMap.find fid model.int_flags
