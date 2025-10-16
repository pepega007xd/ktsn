open Astral

(** This module implements the transfer function for most of the basic
    instructions defined in [Instruction_type] *)

(** transfer function for [var = var;] *)
let assign (lhs : Formula.var) (rhs : Formula.var) (formula : Formula.t) :
    Formula.t =
  formula |> Formula.substitute_by_fresh lhs |> Formula.add_eq lhs rhs

(** transfer function for [var = var->field;] *)
let assign_rhs_field (lhs : Formula.var) (rhs : Formula.var)
    (rhs_field : Types.field_type) (formula : Formula.t) : Formula.t =
  let rhs_target =
    Formula.get_spatial_target rhs rhs_field formula |> function
    | Some rhs -> rhs
    | None -> raise @@ Formula.Bug (Invalid_deref (rhs, formula))
  in
  if lhs = rhs_target then formula else assign lhs rhs_target formula

(** transfer function for [var->field = var;] *)
let assign_lhs_field (lhs : Formula.var) (lhs_field : Types.field_type)
    (rhs : Formula.var) (formula : Formula.t) : Formula.t =
  Formula.change_pto_target lhs lhs_field rhs formula

let stack_ptr_field = Types.Other Constants.ptr_field_name

(** transfer function for [*var = var;], lhs is assumed to be a stack pointer *)
let assign_lhs_deref (lhs : Formula.var) (rhs : Formula.var)
    (formula : Formula.t) : Formula.t =
  let lhs_target =
    Formula.get_spatial_target lhs stack_ptr_field formula |> Option.get
  in
  assign lhs_target rhs formula
  |> Formula.change_pto_target lhs stack_ptr_field lhs_target

(** transfer function for [var = &var;], we need to check if there is already a
    _target ptr and change it, otherwise add it *)
let assign_ref (lhs : Formula.var) (rhs : Formula.var) (formula : Formula.t) :
    Formula.t =
  match Formula.get_spatial_target lhs stack_ptr_field formula with
  | Some _ -> Formula.change_pto_target lhs stack_ptr_field rhs formula
  | None ->
      Formula.add_atom
        (Formula.PointsTo (lhs, Generic [ (Constants.ptr_field_name, rhs) ]))
        formula

(** transfer function for function calls *)
let call (lhs_opt : Formula.var option) (func : Cil_types.varinfo)
    (args : Formula.var list) (formula : Formula.t) : Formula.state =
  let get_allocation (init_vars_to_null : bool) : Formula.state =
    let lhs, pto =
      match lhs_opt with
      | Some lhs -> (
          let sort = SL.Variable.get_sort lhs in
          let fresh_from_lhs () =
            if init_vars_to_null then Formula.nil
            else Common.mk_fresh_var_from lhs
          in
          ( lhs,
            match () with
            | _ when sort = SL_builtins.loc_ls ->
                Formula.PointsTo (lhs, LS_t (fresh_from_lhs ()))
            | _ when sort = SL_builtins.loc_dls ->
                Formula.PointsTo
                  (lhs, DLS_t (fresh_from_lhs (), fresh_from_lhs ()))
            | _ when sort = SL_builtins.loc_nls ->
                Formula.PointsTo
                  (lhs, NLS_t (fresh_from_lhs (), fresh_from_lhs ()))
            | _ ->
                let fields =
                  Types.get_struct_def sort |> MemoryModel.StructDef.get_fields
                in
                let names = List.map MemoryModel0.Field.show fields in
                let vars =
                  if init_vars_to_null then
                    List.map (fun _ -> Formula.nil) fields
                  else
                    List.map MemoryModel0.Field.get_sort fields
                    |> List.map
                         (SL.Variable.mk_fresh (SL.Variable.get_name lhs))
                in
                Formula.PointsTo (lhs, Generic (List.combine names vars)) ))
      | None ->
          let lhs = SL.Variable.mk_fresh "leak" Sort.loc_nil in
          (lhs, Formula.PointsTo (lhs, LS_t (Common.mk_fresh_var_from lhs)))
    in
    let allocation =
      formula |> Formula.substitute_by_fresh lhs |> Formula.add_atom pto
    in
    if Config.Benchmark_mode.get () then [ allocation ]
    else
      [
        allocation;
        formula
        |> Formula.substitute_by_fresh lhs
        |> Formula.add_eq lhs Formula.nil;
      ]
  in

  match (func.vname, args) with
  | "malloc", _ -> get_allocation false
  | "calloc", _ -> get_allocation true
  | "realloc", var :: _ ->
      (* realloc changes the pointer value => all references to `var` are now dangling *)
      Formula.materialize var formula
      |> List.map (fun formula ->
             let spatial_atom = Formula.get_spatial_atom_from var formula in
             formula
             |> Formula.remove_spatial_from var
             |> Formula.substitute_by_fresh var
             |> Formula.add_atom spatial_atom)
  | "free", [ src ] -> (
      try
        formula |> Formula.materialize src
        |> List.map (Formula.remove_spatial_from src)
        |> List.map (Formula.add_atom @@ Formula.Freed src)
      with
      | Formula.Bug (Invalid_deref (var, formula)) ->
          raise @@ Formula.Bug (Invalid_free (var, formula))
      | e -> raise e)
  | _, args -> Func_call.func_call args func formula lhs_opt
