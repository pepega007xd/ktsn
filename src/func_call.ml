open Astral
open Cil_types
open Common

type summary_input = Kernel_function.t * Formula.t

let summaries : (summary_input, Formula.state) Hashtbl.t ref =
  ref @@ Hashtbl.create 113

(** for running dataflow analysis recursively on other functions *)
let compute_function : (Cil_types.stmt list -> unit) ref =
  ref (fun _ -> fail "called compute_function without initializing it")

(** state of dataflow analysis is stored here *)
let results : (Cil_types.stmt, Formula.state) Hashtbl.t ref =
  ref (Hashtbl.create 113)

let get_anchor (var : Formula.var) : Formula.var =
  let name = SSL.Variable.get_name var in
  SSL.Variable.mk
    (* '$' is used not to collide with an existing var name, eg. x!0 -> x_0,
       it has no special meaning, x$0 behaves like a regular program variable *)
    ("A_" ^ String.map (function '!' -> '$' | c -> c) name)
    (SSL.Variable.get_sort var)

let run_analysis (func : Kernel_function.t) (formula : Formula.t) :
    Formula.state =
  let first_stmt = Kernel_function.find_first_stmt func in
  let return_stmt = Kernel_function.find_return func in
  (* backup current state of analysis into a variable *)
  let current_results = !results in
  results := Hashtbl.create 113;

  Hashtbl.add !results first_stmt [ formula ];
  !compute_function [ first_stmt ];
  let result_state = Hashtbl.find !results return_stmt in
  (* restore current state of analysis *)
  results := current_results;

  result_state

let get_result_state (func : Kernel_function.t) (formula : Formula.t) :
    Formula.state =
  let summary_input = Formula.standardize_fresh_var_names formula in
  Hashtbl.find_opt !summaries (func, summary_input) |> function
  | Some result -> result
  | None ->
      let result_state = run_analysis func formula in
      Hashtbl.add !summaries (func, summary_input) result_state;
      result_state

let func_call (args : Formula.var list) (func : varinfo) (formula : Formula.t)
    (lhs_opt : Formula.var option) =
  let func = Globals.Functions.get func in
  let fundec = Kernel_function.get_definition func in
  let return_stmt = Kernel_function.find_return func in
  let params = fundec.sformals |> List.map Preprocessing.varinfo_to_var in
  let locals =
    params @ (fundec.slocals |> List.map Preprocessing.varinfo_to_var)
  in

  let reachable, unreachable = Formula.split_by_reachability args formula in
  let reachable_vars =
    reachable |> Formula.get_fresh_vars
    |> List.filter (fun var -> Formula.is_spatial_target var unreachable)
  in

  let rename_anchors (formula : Formula.t) =
    List.fold_left2
      (fun formula param arg ->
        Formula.substitute ~var:(get_anchor param) ~by:arg formula)
      formula params args
  in

  let remove_anchors (formula : Formula.t) =
    List.fold_left
      (fun formula var ->
        Formula.substitute ~var:(get_anchor var) ~by:var formula)
      formula reachable_vars
  in

  let convert_back_result (result : Formula.t) =
    let result = (result |> rename_anchors |> remove_anchors) @ unreachable in

    (match lhs_opt with
    | Some lhs ->
        let return_var =
          match return_stmt.skind with
          | Return (Some { enode = Lval (Var var, NoOffset); _ }, _) ->
              SSL.Variable.mk var.vname (SSL.Variable.get_sort lhs)
          | _ ->
              fail
                "function which is supposed to return something does not have \
                 a return value"
        in
        result
        |> Formula.substitute_by_fresh lhs
        |> Formula.substitute ~var:return_var ~by:lhs
    | None -> result)
    |> Simplification.convert_vars_to_fresh locals
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

  (* add anchors for variables reachable from the unreachable part of formula *)
  let reachable =
    List.fold_left
      (fun formula var -> Formula.add_eq var (get_anchor var) formula)
      reachable reachable_vars
  in

  let result_state = get_result_state func reachable in

  Config.Self.debug ~current:true ~dkey:Printing.func_call
    "input formula:\n%a\nreachable subformula:\n%a\nunreachable subformula:\n%a"
    Formula.pp_formula formula Formula.pp_formula reachable Formula.pp_formula
    unreachable;

  List.map convert_back_result result_state
