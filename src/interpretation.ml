open Cil_types
open Common
open Astral

let var = Types.varinfo_to_var

let eval_binop (op : binop) (lhs : Formula.var) (rhs : Formula.var)
    (formula : Formula.t) : Formula.t * Formula.var =
  let fresh_var = SL.Variable.mk_fresh "exp" Sort.int in
  match
    (Formula.get_int_val_opt lhs formula, Formula.get_int_val_opt rhs formula)
  with
  | Some lhs, Some rhs ->
      let value =
        match op with
        | PlusA -> lhs + rhs
        | MinusA -> lhs - rhs
        | Mult -> lhs * rhs
        | Div -> lhs / rhs
        | Mod -> lhs mod rhs
        | Shiftlt -> lhs lsl rhs
        | Shiftrt -> lhs asr rhs
        | Lt -> lhs < rhs |> Bool.to_int
        | Gt -> lhs > rhs |> Bool.to_int
        | Le -> lhs <= rhs |> Bool.to_int
        | Ge -> lhs >= rhs |> Bool.to_int
        | Eq -> lhs = rhs |> Bool.to_int
        | Ne -> lhs <> rhs |> Bool.to_int
        | _ -> fail "unsupported binop: %a" Printer.pp_binop op
      in
      (Formula.update_int_eq fresh_var value formula, fresh_var)
  | _ -> (
      match op with
      | Eq | Ne -> (formula, fresh_var)
      | _ -> fail "unsupported binop: %a" Printer.pp_binop op)

let rec eval (formula : Formula.t) (exp : exp) : (Formula.t * Formula.var) list
    =
  let eval_orig = eval formula in
  let results =
    match exp.enode with
    | _ when Cil.typeOf exp |> Ast_types.is_ptr && Cil.is_nullptr exp ->
        [ (formula, Formula.nil) ]
    | _ when Cil.constFoldToInt exp |> Option.is_some ->
        let fresh_var = SL.Variable.mk_fresh "int" Sort.int in
        let value = Cil.constFoldToInt exp |> Option.get |> Z.to_int in
        [ (Formula.update_int_eq fresh_var value formula, fresh_var) ]
    | CastE (typ, exp)
      when Ast_types.is_ptr typ && Cil.typeOf exp |> Ast_types.is_ptr ->
        eval_orig exp
    | BinOp (op, lhs, rhs, _) ->
        List.concat_map
          (fun (formula, lhs) ->
            List.map
              (fun (formula, rhs) -> eval_binop op lhs rhs formula)
              (eval formula rhs))
          (eval_orig lhs)
    | Lval (Var v, NoOffset) -> [ (formula, var v) ]
    | Lval (Mem inner, NoOffset) ->
        List.map
          (fun (formula, var) ->
            let var =
              (Formula.get_spatial_target var (Other Constants.ptr_field_name))
                formula
            in
            (formula, var))
          (eval_orig inner)
    | Lval (Mem inner, Field (field, NoOffset)) ->
        List.map
          (fun (formula, var) ->
            if Types.is_relevant_type field.ftype then
              let var =
                Formula.get_spatial_target var
                  (Types.get_field_type field)
                  formula
              in
              (formula, var)
            (* reading from non-pointer field *)
              else (formula, Formula.unknown))
          (eval_orig inner)
    | _ -> fail "unsupported expr: %a" Printer.pp_exp exp
  in
  (* caller doesn't have to materialize [var] *)
  List.concat_map
    (fun (formula, var) ->
      Formula.materialize var formula |> List.map (fun f -> (f, var)))
    results

let set_value (lhs : lval) (rhs : Formula.var) (formula : Formula.t) :
    Formula.t list =
  let eval = eval formula in
  match lhs with
  (* *expr = expr; *)
  | Mem lhs, NoOffset ->
      List.map
        (fun (formula, lhs) -> Transfer.assign_lhs_deref lhs rhs formula)
        (eval lhs)
  (* expr->field = expr; *)
  | Mem lhs, Field (lhs_field, NoOffset) ->
      List.map
        (fun (formula, lhs) ->
          if Types.is_relevant_type lhs_field.ftype then
            Transfer.assign_lhs_field lhs
              (Types.get_field_type lhs_field)
              rhs formula
          (* assignment into non-pointer field *)
            else formula)
        (eval lhs)
  (* var = expr; // integer var *)
  | Var lhs, NoOffset when Ast_types.is_integral lhs.vtype -> (
      let lhs = var lhs in
      Formula.get_int_val_opt lhs formula |> function
      | Some value -> [ Formula.update_int_eq lhs value formula ]
      | None -> [ formula ])
  (* var = expr; // pointer var *)
  | Var lhs, NoOffset -> [ Transfer.assign (var lhs) rhs formula ]
  | _ -> fail "invalid lval: %a" Printer.pp_lval lhs

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
  (* var = &var; // limited form, only used for stack pointers *)
  | Set ((Var lhs, NoOffset), { enode = AddrOf (Var rhs, NoOffset); _ }, _) ->
      [ Transfer.assign_ref (var lhs) (var rhs) formula ]
  (* lval = expr; *)
  | Set (lhs, rhs, _) ->
      List.concat_map
        (fun (formula, rhs) -> set_value lhs rhs formula)
        (eval rhs)
  (* [var =] func(expr1, expr2, ...); *)
  | Call (lhs_option, { enode = Lval (Var func, NoOffset); _ }, args, _) -> (
      let lhs_sort =
        Option.map
          (fun lhs -> Cil.typeOfLval lhs |> Types.get_type_info |> fst)
          lhs_option
        |> Option.value ~default:Astral.Sort.loc_nil
      in
      (* evaluate all function arguments *)
      let inputs =
        List.fold_left
          (fun inputs arg -> List.concat_map (eval_arg arg) inputs)
          [ (formula, []) ]
          args
      in
      (* evaluate function call on all (formula * arguments) tuples *)
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
