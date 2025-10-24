(** This modules contains constants used by the rest of preprocessing *)

let const_var_name = "_const"
let nondet_var_name = "_nondet"
let ptr_field_name = "_target"
let const_var = Cil.makeVarinfo false false const_var_name Cil_const.voidPtrType

let nondet_var =
  Cil.makeVarinfo false false nondet_var_name Cil_const.voidPtrType
