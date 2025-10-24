(** This modules contains constants used by the rest of preprocessing *)

let nondet_var_name = "$nondet"
let ptr_field_name = "$target"

let nondet_var =
  Cil.makeVarinfo false false nondet_var_name Cil_const.voidPtrType
