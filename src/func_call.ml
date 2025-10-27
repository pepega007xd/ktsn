open Astral
open Cil_types
open Common

(** This module implements the transfer function for function calls, including
    the swiching of the analysis context and function summaries *)

type summary_input = Kernel_function.t * Formula.t

(* Context used for the analysis of a single function *)
type function_context = {
  results : (Cil_types.stmt, Formula.state) Hashtbl.t;
  loop_cycles : (stmt, int) Hashtbl.t;
}

let summaries : (summary_input, Formula.state) Hashtbl.t = Hashtbl.create 113

(** for running dataflow analysis recursively on other functions *)
let compute_function : (Cil_types.stmt list -> unit) ref =
  ref (fun _ -> fail "called compute_function without initializing it")

let get_default_context () =
  { results = Hashtbl.create 113; loop_cycles = Hashtbl.create 113 }

(** All data specific for the analysis of a single function *)
let function_context = ref @@ get_default_context ()

(** stores past results from analysis of all visited functions, used to show all
    generated states in Ivette at the end of analysis *)
let previous_results : (Cil_types.stmt, Formula.state) Hashtbl.t list ref =
  ref []

let get_anchor (var : Formula.var) : Formula.var =
  let name = "A$" ^ SL.Variable.get_name var in
  SL.Variable.mk name (SL.Variable.get_sort var)

(** Creates a new context, runs the analysis recursively of the called function,
    and then restores the original context *)
let run_function_analysis (func : Kernel_function.t) (formula : Formula.t) :
    Formula.state =
  let first_stmt = Kernel_function.find_first_stmt func in
  let return_stmt = Kernel_function.find_return func in
  (* backup current state of analysis into a variable *)
  let current_context = !function_context in

  function_context := get_default_context ();
  Hashtbl.add !function_context.results first_stmt [ formula ];
  !compute_function [ first_stmt ];
  let result_state = Hashtbl.find !function_context.results return_stmt in

  (* save generated states to display them later *)
  previous_results := !function_context.results :: !previous_results;

  (* restore current state of analysis *)
  function_context := current_context;

  result_state

(** Implements the caching of function summaries *)
let get_result_state (func : Kernel_function.t) (formula : Formula.t) :
    Formula.state =
  Hashtbl.find_opt summaries (func, formula) |> function
  | Some result -> result
  | None ->
      let result_state = run_function_analysis func formula in
      Hashtbl.add summaries (func, formula) result_state;
      result_state

let func_call (args : Formula.var list) (func : varinfo) (formula : Formula.t)
    (lhs_sort : Sort.t) : Formula.t list * Formula.var list =
  let func = Globals.Functions.get func in
  let return_stmt = Kernel_function.find_return func in

  let params =
    Kernel_function.get_formals func |> List.map Types.varinfo_to_var
  in
  let locals =
    params @ (Kernel_function.get_locals func |> List.map Types.varinfo_to_var)
  in

  let reachable, unreachable = Formula.split_by_reachability args formula in

  let rename_anchors (formula : Formula.t) =
    List.fold_left2
      (fun formula param arg ->
        Formula.substitute ~var:(get_anchor param) ~by:arg formula)
      formula params args
  in

  let convert_back_result (result : Formula.t) : Formula.t * Formula.var =
    let result = rename_anchors result @ unreachable in
    let lhs = SL.Variable.mk_fresh "func_ret" lhs_sort in
    let return_var =
      match return_stmt.skind with
      | Return (Some { enode = Lval (Var var, NoOffset); _ }, _) ->
          SL.Variable.mk var.vname lhs_sort
      | _ -> SL.Variable.mk "dummy" Sort.loc_nil
    in
    ( result
      |> Formula.substitute ~var:return_var ~by:lhs
      |> Simplification.convert_vars_to_fresh locals,
      lhs )
  in

  let reachable =
    List.fold_left2
      (fun formula param arg ->
        formula
        (* add anchor variables *)
        |> Formula.add_eq param (get_anchor param)
        (* substitute argument names with parameter names *)
        |> Formula.substitute ~var:arg ~by:param)
      reachable params args
  in

  let result_state = get_result_state func reachable in

  Config.Self.debug ~current:true ~dkey:Printing.func_call
    "input formula:\n%a\nreachable subformula:\n%a\nunreachable subformula:\n%a"
    Formula.pp_formula formula Formula.pp_formula reachable Formula.pp_formula
    unreachable;

  List.map convert_back_result result_state |> List.split

let func_call (args : Formula.var list) (func : varinfo) (formula : Formula.t)
    (lhs_sort : Sort.t) : Formula.t list * Formula.var list =
  try func_call args func formula lhs_sort
  with Kernel_function.No_Definition | Kernel_function.No_Statement ->
    warning "skipping function %s (no definition)" func.vname;
    (* TODO: should this be a hard error? *)
    ([ formula ], [ Formula.nil ])

(** Merges results of analyses of all functions into the current [results]
    table, so that it can be displayed by Ivette *)
let merge_all_results () =
  List.iter
    (Hashtbl.iter (Hashtbl.replace !function_context.results))
    !previous_results
