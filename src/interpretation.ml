open Cil_types
open Common

let var = Types.varinfo_to_var

let rec eval (formula : Formula.t) (exp : exp) : (Formula.t * Formula.var) list
    =
  let eval = eval formula in
  let results =
    match exp.enode with
    | Lval (Var v, NoOffset) -> [ (formula, var v) ]
    | Lval (Mem inner, NoOffset) ->
        List.map
          (fun (formula, var) ->
            let var =
              (Formula.get_spatial_target var (Other Constants.ptr_field_name))
                formula
            in
            (formula, var))
          (eval inner)
    | Lval (Mem inner, Field (field, NoOffset)) ->
        List.map
          (fun (formula, var) ->
            let var =
              Formula.get_spatial_target var
                (Types.get_field_type field)
                formula
            in
            (formula, var))
          (eval inner)
    | _ -> fail "unsupported expr: %a" Printer.pp_exp exp
  in
  List.concat_map
    (fun (formula, var) ->
      Formula.materialize var formula |> List.map (fun f -> (f, var)))
    results

let set_value (lhs : lval) (rhs : Formula.var) (formula : Formula.t) :
    Formula.t list =
  let eval = eval formula in
  match lhs with
  | lhs when Cil.typeOfLval lhs |> Types.is_relevant_type |> not ->
      (* TODO: check validity of accesses inside non-ptr expressions *)
      (* ignore @@ eval @@ Cil.dummy_exp (Lval lhs); *)
      (* ignore @@ eval rhs; *)
      [ formula ]
  (* *expr = expr; *)
  | Mem lhs, NoOffset ->
      List.map
        (fun (formula, lhs) -> Transfer.assign_lhs_deref lhs rhs formula)
        (eval lhs)
  (* expr->field = expr; *)
  | Mem lhs, Field (lhs_field, NoOffset) ->
      List.map
        (fun (formula, lhs) ->
          Transfer.assign_lhs_field lhs
            (Types.get_field_type lhs_field)
            rhs formula)
        (eval lhs)
  (* var = expr; *)
  | Var lhs, NoOffset -> [ Transfer.assign (var lhs) rhs formula ]
  | _ -> fail "invaliud lval: %a" Printer.pp_lval lhs

(* formula and a list of func arguments *)
type function_input = Formula.t * Formula.var list

let eval_arg (arg : exp) (input : function_input) : function_input list =
  let formula, prev_args = input in
  List.map
    (fun (formula, var) -> (formula, var :: prev_args))
    (eval formula arg)

let interpret_instr (instr : instr) (formula : Formula.t) : Formula.t list =
  let eval = eval formula in
  match instr with
  | Set (lhs, rhs, _) when Cil.typeOfLval lhs |> Types.is_relevant_type |> not
    ->
      (* TODO: check validity of accesses inside non-ptr expressions *)
      (* ignore @@ eval @@ Cil.dummy_exp (Lval lhs); *)
      (* ignore @@ eval rhs; *)
      [ formula ]
  (* var = &var; // limited form, only used for stack pointers *)
  | Set ((Var lhs, NoOffset), { enode = AddrOf (Var rhs, NoOffset); _ }, _) ->
      [ Transfer.assign_ref (var lhs) (var rhs) formula ]
  (* [var =] func(expr1, expr2, ...); *)
  | Set (lhs, rhs, _) ->
      List.concat_map
        (fun (formula, rhs) -> set_value lhs rhs formula)
        (eval rhs)
  | Call (lhs_option, { enode = Lval (Var func, NoOffset); _ }, args, _) -> (
      let lhs_sort =
        Option.map
          (fun lhs -> Cil.typeOfLval lhs |> Types.get_type_info |> fst)
          lhs_option
        |> Option.value ~default:Astral.Sort.loc_nil
      in
      let inputs =
        List.fold_left
          (fun inputs arg -> List.concat_map (eval_arg arg) inputs)
          [ (formula, []) ]
          args
      in
      let formulas, return_vars =
        List.concat_map
          (fun (formula, args) ->
            let formulas, return_vars =
              Transfer.call lhs_sort func args formula
            in
            List.combine formulas return_vars)
          inputs
        |> List.split
      in
      lhs_option |> function
      | Some lhs ->
          List.map2 (set_value lhs) return_vars formulas |> List.concat
      | None -> formulas)
  | Skip _ -> [ formula ]
  | _ -> fail "unsupported instr: %a" Cil_printer.pp_instr instr
