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
  (* TODO: remove this func and other unused cases *)
  let rhs_target =
    Formula.get_spatial_target_opt rhs rhs_field formula |> function
    | Some rhs -> rhs
    | None -> Formula.report_bug (Invalid_deref (rhs, formula))
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
  let lhs_target = Formula.get_spatial_target lhs stack_ptr_field formula in
  assign lhs_target rhs formula
  |> Formula.change_pto_target lhs stack_ptr_field lhs_target

(** transfer function for [var = &var;], we need to check if there is already a
    _target ptr and change it, otherwise add it *)
let assign_ref (lhs : Formula.var) (rhs : Formula.var) (formula : Formula.t) :
    Formula.t =
  match Formula.get_spatial_target_opt lhs stack_ptr_field formula with
  | Some _ -> Formula.change_pto_target lhs stack_ptr_field rhs formula
  | None ->
      Formula.add_atom
        (Formula.PointsTo (lhs, Generic [ (Constants.ptr_field_name, rhs) ]))
        formula

(** transfer function for function calls *)
let call (lhs_sort : SL.Sort.t) (func : Cil_types.varinfo)
    (args : Formula.var list) (formula : Formula.t) :
    Formula.t list * Formula.var list =
  let get_allocation (init_vars_to_null : bool) =
    let lhs = SL.Variable.mk_fresh "func_ret" lhs_sort in
    let pto =
      let fresh_from_lhs () =
        if init_vars_to_null then Formula.nil
        else SL.Variable.mk_fresh "fresh" lhs_sort
      in
      match () with
      | _ when lhs_sort = SL_builtins.loc_ls || lhs_sort = SL.Sort.loc_nil ->
          Formula.PointsTo (lhs, LS_t (fresh_from_lhs ()))
      | _ when lhs_sort = SL_builtins.loc_dls ->
          Formula.PointsTo (lhs, DLS_t (fresh_from_lhs (), fresh_from_lhs ()))
      | _ when lhs_sort = SL_builtins.loc_nls ->
          Formula.PointsTo (lhs, NLS_t (fresh_from_lhs (), fresh_from_lhs ()))
      | _ ->
          let fields =
            Types.get_struct_def lhs_sort |> MemoryModel.StructDef.get_fields
          in
          let names = List.map MemoryModel0.Field.show fields in
          let vars =
            if init_vars_to_null then List.map (fun _ -> Formula.nil) fields
            else
              List.map MemoryModel0.Field.get_sort fields
              |> List.map (SL.Variable.mk_fresh (SL.Variable.get_name lhs))
          in
          Formula.PointsTo (lhs, Generic (List.combine names vars))
    in
    let allocation = formula |> Formula.add_atom pto in
    if Config.Svcomp_mode.get () then ([ allocation ], [ lhs ])
    else
      ( [
          (* success *)
          allocation;
          (* failure *)
          formula
          |> Formula.substitute_by_fresh lhs
          |> Formula.add_eq lhs Formula.nil;
        ],
        [ lhs; lhs ] )
  in

  match (func.vname, args) with
  | "malloc", _ -> get_allocation false
  | "calloc", _ -> get_allocation true
  (*TODO: *)
  (* | "realloc", var :: _ -> *)
  (*     (* realloc changes the pointer value => all references to `var` are now dangling *) *)
  (*     Formula.materialize var formula *)
  (*     |> List.map (fun formula -> *)
  (*            let spatial_atom = Formula.get_spatial_atom_from var formula in *)
  (*            formula *)
  (*            |> Formula.remove_spatial_from var *)
  (*            |> Formula.substitute_by_fresh var *)
  (*            |> Formula.add_atom spatial_atom) *)
  | "free", [ src ] -> (
      try
        formula |> Formula.materialize src
        |> List.map (Formula.remove_spatial_from src)
        |> List.map (Formula.add_atom @@ Formula.Freed src)
        |> List.map (fun f -> (f, Formula.nil))
        |> List.split
      with
      | Formula.Bug (Invalid_deref (var, formula), pos) ->
          raise @@ Formula.Bug (Invalid_free (var, formula), pos)
      | e -> raise e)
  | "__VERIFIER_nondet_int", _ -> ([ formula ], [ Formula.nondet ])
  | _, args -> Func_call.func_call args func formula lhs_sort
