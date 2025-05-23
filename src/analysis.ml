open Config
open Cil_types
open Common
open Dataflow2

(** This module contains the implementation of the dataflow analysis, this
    module implements the [Dataflow2.ForwardsTransfer] interface *)

let name = "ktsn"
let debug = false

type t = Formula.state

let copy state = state
let pretty fmt state = Formula.pp_state fmt state
let var = Types.varinfo_to_var

(** this is the transfer function for instructions, we take the instr and
    previous state, and create new state *)
let doInstr _ (instr : instr) (prev_state : t) : t =
  (* this allows Ivette to load the current
     state of analysis (messages, AST properties, etc) *)
  Async.yield ();

  let new_state =
    match Instruction_type.get_instr_type instr with
    (* assignment into "_const" is used to check if rhs is allocated *)
    | Assign_simple (lhs, rhs) when lhs.vname = Constants.const_var_name ->
        prev_state
        |> List.concat_map (Formula.materialize (var rhs))
        |> List.iter (Formula.assert_allocated (var rhs));
        prev_state
    | Assign_simple (lhs, rhs) ->
        prev_state |> List.map (Transfer.assign (var lhs) (var rhs))
    | Assign_rhs_field (lhs, rhs, rhs_field) ->
        prev_state
        |> List.concat_map (Formula.materialize (var rhs))
        |> List.map
             (Transfer.assign_rhs_field (var lhs) (var rhs)
                (Types.get_field_type rhs_field))
    | Assign_lhs_field (lhs, lhs_field, rhs) ->
        prev_state
        |> List.concat_map (Formula.materialize (var lhs))
        |> List.map
             (Transfer.assign_lhs_field (var lhs)
                (Types.get_field_type lhs_field)
                (var rhs))
    | Assign_deref_rhs (lhs, rhs) ->
        List.map
          (Transfer.assign_rhs_field (var lhs) (var rhs)
             (Other Constants.ptr_field_name))
          prev_state
    | Assign_deref_lhs (lhs, rhs) ->
        prev_state |> List.map (Transfer.assign_lhs_deref (var lhs) (var rhs))
    | Assign_ref (lhs, rhs) ->
        List.map (Transfer.assign_ref (var lhs) (var rhs)) prev_state
    | Call (lhs_opt, func, params) ->
        prev_state
        |> List.concat_map
             (Transfer.call (Option.map var lhs_opt) func (List.map var params))
    | ComplexInstr -> assert false
    | Ignored -> prev_state
  in
  Self.debug ~current:true ~dkey:Printing.do_instr
    "previous state:\n%anew state:\n%a" Formula.pp_state prev_state
    Formula.pp_state new_state;
  new_state

let computeFirstPredecessor _ state = state

(** Removes formulas that are covered by other formulas using entailment *)
let deduplicate_formulas (state : t) : t =
  let is_covered current to_keep =
    if Config.Simple_join.get () then
      List.exists
        (fun formula_to_keep ->
          Astral_query.check_entailment [ current ] [ formula_to_keep ])
        to_keep
    else Astral_query.check_entailment [ current ] to_keep
  in

  state
  |> List.sort Formula.compare_bounds
  |> List.fold_left
       (fun to_keep current ->
         if is_covered current to_keep then to_keep else current :: to_keep)
       []

(** Iterate through all formulas of new_state [phi], and each one that doesn't
    satisfy (phi => old) has to be added to [old]. If old is not changed, None
    is returned. *)
let combinePredecessors _ ~old:(old_state : t) (new_state : t) : t option =
  Async.yield ();

  let joined_state = deduplicate_formulas @@ new_state @ old_state in

  let state_changed joined_state old_state =
    if Config.Simple_join.get () then
      List.for_all
        (fun joined_formula ->
          List.exists
            (fun old_formula ->
              Astral_query.check_entailment [ joined_formula ] [ old_formula ])
            old_state)
        joined_state
    else Astral_query.check_entailment joined_state old_state
  in

  if state_changed joined_state old_state then (
    Self.debug ~current:true ~dkey:Printing.combine_predecessors
      "old state:\n%anew state:\n%a<state did not change>" Formula.pp_state
      old_state Formula.pp_state new_state;
    None)
  else (
    Self.debug ~current:true ~dkey:Printing.combine_predecessors
      "old state:\n%anew state:\n%acombined state:\n%a" Formula.pp_state
      old_state Formula.pp_state new_state Formula.pp_state joined_state;
    Some joined_state)

let unknown_condition_reached = ref false

(* Filter the formulas of [state] for each branch to only those,
   which are satisfiable in each of the branches *)
let doGuard _ (condition : exp) (state : t) : t guardaction * t guardaction =
  Async.yield ();

  let th, el =
    match condition.enode with
    | BinOp
        ( operator,
          { enode = Lval (Var lhs, NoOffset); _ },
          { enode = Lval (Var rhs, NoOffset); _ },
          _ ) -> (
        let lhs = var lhs in
        let rhs = var rhs in
        let eq = List.map (Formula.add_eq lhs rhs) state in
        let ne = List.map (Formula.add_distinct lhs rhs) state in
        match operator with
        | Eq -> (eq, ne)
        | Ne -> (ne, eq)
        | _ ->
            unknown_condition_reached := true;
            (state, state))
    (* nondeterministic conditions do not count as unknown *)
    | Lval (Var var, NoOffset) when var.vname = Constants.nondet_var_name ->
        (state, state)
    | _ ->
        unknown_condition_reached := true;
        (state, state)
  in
  let th = List.filter Astral_query.check_sat th in
  let el = List.filter Astral_query.check_sat el in

  Self.debug ~current:true ~dkey:Printing.do_guard
    "state:\n%athen branch:\n%aelse branch:\n%a" Formula.pp_state state
    Formula.pp_state th Formula.pp_state el;

  let th = if List.is_empty th then GUnreachable else GUse th in
  let el = if List.is_empty el then GUnreachable else GUse el in
  (th, el)

let get_inner_loops (block : block) =
  let loops = ref [] in
  let visitor =
    object
      inherit Cil.nopCilVisitor

      method! vstmt s =
        (match s.skind with Loop _ -> loops := s :: !loops | _ -> ());
        DoChildren
    end
  in
  ignore (Cil.visitCilBlock visitor block);
  !loops

(** Decides whether to stop the analysis *)
let doStmt (stmt : stmt) (_ : t) : t stmtaction =
  let loop_cycles = !Func_call.function_context.loop_cycles in
  match stmt.skind with
  (* stop when reaching the maximum number of loop 
      iterations in underapproximation mode *)
  | Loop (_, block, _, _, _) when Config.Max_loop_cycles.is_set () -> (
      match Hashtbl.find_opt loop_cycles stmt with
      | Some x when x > 0 ->
          Hashtbl.add loop_cycles stmt (x - 1);
          (* reset counters for all inner loops of the loop we are entering *)
          get_inner_loops block
          |> List.iter (fun key ->
                 Common.debug "removing loop %a" Printer.pp_stmt key;
                 Hashtbl.remove loop_cycles key);
          SDefault
      | Some _ ->
          warning "Skipping loop cycle";
          SDone
      | None ->
          Hashtbl.add loop_cycles stmt (Config.Max_loop_cycles.get ());
          SDefault)
  | Instr instr when Config.Benchmark_mode.get () -> (
      match Instruction_type.get_instr_type instr with
      | Instruction_type.Call (_, fn, _) ->
          if List.mem fn.vname [ "reach_error"; "myexit"; "fail"; "exit" ] then
            SDone
          else SDefault
      | _ -> SDefault)
  | _ -> SDefault

(* Simplify and deduplicate formulas *)
let doEdge (prev_stmt : stmt) (next_stmt : stmt) (state : t) : t =
  Async.yield ();

  let end_of_scope_locals =
    Kernel_function.blocks_closed_by_edge prev_stmt next_stmt
    |> List.concat_map (fun block -> block.blocals)
    |> List.filter Types.is_relevant_var
    |> List.map Types.varinfo_to_var
  in

  let end_of_scope_stack_vars =
    List.filter
      (fun var -> List.mem var !Preprocessing.stack_allocated_vars)
      end_of_scope_locals
  in

  let do_abstraction (formula : Formula.t) : Formula.t =
    match next_stmt.skind with
    | _ when Config.Edge_abstraction.get () ->
        formula |> Abstraction.convert_to_ls |> Abstraction.convert_to_dls
        |> Abstraction.convert_to_nls
    | Loop _ ->
        formula |> Abstraction.convert_to_ls |> Abstraction.convert_to_dls
        |> Abstraction.convert_to_nls
    | _ -> formula
  in

  let deduplicate_states : t -> t =
    if Config.Edge_deduplication.get () then deduplicate_formulas else Fun.id
  in

  let open Simplification in
  let modified =
    state
    |> List.map (remove_ptos_from_vars end_of_scope_stack_vars)
    |> List.map (convert_vars_to_fresh end_of_scope_locals)
    |> List.map remove_leaks
    |> List.map reduce_equiv_classes
    |> List.map do_abstraction
    |> List.map remove_irrelevant_vars
    |> List.map remove_empty_lists
    |> Formula.canonicalize_state
    |> Common.list_map_pairs generalize_similar_formulas
    (* deduplicate formulas syntactically *)
    |> List.sort_uniq compare
    (* deduplicate formulas semantically *)
    |> deduplicate_states
  in

  Self.debug ~current:true ~dkey:Printing.do_edge
    "original state:\n%anew state:\n%a" Formula.pp_state state Formula.pp_state
    modified;
  modified

module StmtStartData = struct
  type data = t

  let clear () = Hashtbl.clear !Func_call.function_context.results

  (* we cannot just assign `let mem = Hashtbl.mem !results`, that would
      evaluate `!results` immediately, but we need it to be evaluated each time
      (inside function calls, `results` refers to a different hashtable *)
  let mem stmt = Hashtbl.mem !Func_call.function_context.results stmt
  let find stmt = Hashtbl.find !Func_call.function_context.results stmt
  let replace stmt = Hashtbl.replace !Func_call.function_context.results stmt
  let add stmt = Hashtbl.add !Func_call.function_context.results stmt
  let iter f = Hashtbl.iter f !Func_call.function_context.results
  let length () = Hashtbl.length !Func_call.function_context.results
end

module Tests = struct
  open Testing

  let%test "atom_deduplication" =
    let input = [ Distinct (x, y); Distinct (x, y); Distinct (x, y) ] in
    let expected = [ Distinct (x, y) ] in
    assert_eq (List.sort_uniq compare input) expected

  let%test "join_1" =
    let input = [ [ PointsTo (x, LS_t y') ]; [ PointsTo (x, LS_t z') ] ] in
    let expected = [ [ PointsTo (x, LS_t y') ] ] in
    let joined = deduplicate_formulas input in
    assert_eq_state joined expected

  let%test "join_2" =
    let input = [ [ PointsTo (x, LS_t y) ]; [ PointsTo (x, LS_t z) ] ] in
    let joined = deduplicate_formulas input in
    assert_eq_state joined input

  let%test "join_ls" =
    List.init 3 Fun.id
    |> List.for_all (fun len ->
           let input = [ [ mk_ls x y' len ]; [ mk_ls x z' len ] ] in
           let expected = [ [ mk_ls x y' len ] ] in
           let joined = deduplicate_formulas input in
           assert_eq_state joined expected)
end
