open Astral
open Common

(** time spent in Astral *)
let solver_time = ref 0.0

let sat_cache = ref @@ Hashtbl.create 113
let entl_cache = ref @@ Hashtbl.create 113

let init () =
  let dump_queries =
    if Config.Dump_queries.get () then `Full "astral_queries" else `None
  in
  let backend = Config.Backend_solver.get () in
  let encoding = Config.Astral_encoding.get () in
  solver := Solver.init ~dump_queries ~backend ~encoding ()

let check_sat (formula : Formula.t) : bool =
  let astral_formula = Formula.to_astral formula in
  let cache_input =
    formula |> Formula.canonicalize |> Formula.standardize_fresh_var_names
  in

  let cached, result =
    match Hashtbl.find_opt !sat_cache cache_input with
    | Some result -> (true, result)
    | None ->
        let start = Unix.gettimeofday () in
        let result = Solver.check_sat !solver astral_formula in
        solver_time := !solver_time +. Unix.gettimeofday () -. start;

        Hashtbl.add !sat_cache cache_input result;

        (false, result)
  in

  if Config.Astral_debug.get () then (
    Config.Self.debug ~current:true ~dkey:Printing.astral_query
      "SAT id = %s \n Native: %a \n Astral: %a \n RESULT: %b"
      (if cached then "(cached)" else string_of_int @@ Solver.query_id ())
      Formula.pp_formula formula SL.pp astral_formula result;
    Async.yield ());

  result

let check_entailment (lhs : Formula.state) (rhs : Formula.state) : bool =
  let cache_input state =
    state |> Formula.canonicalize_state
    |> List.map Formula.standardize_fresh_var_names
  in
  let cache_input = (cache_input lhs, cache_input rhs) in

  let astral_lhs, astral_rhs =
    (Formula.state_to_astral lhs, Formula.state_to_astral rhs)
  in
  let fresh_vars = SL.get_vars astral_rhs |> List.filter is_fresh_var in
  let astral_rhs = SL.mk_exists fresh_vars astral_rhs in

  let cached, result =
    match Hashtbl.find_opt !entl_cache cache_input with
    | Some result -> (true, result)
    | None ->
        let start = Unix.gettimeofday () in
        let result = Solver.check_entl !solver astral_lhs astral_rhs in
        solver_time := !solver_time +. Unix.gettimeofday () -. start;

        Hashtbl.add !entl_cache cache_input result;

        (false, result)
  in

  if Config.Astral_debug.get () then (
    Config.Self.debug ~current:true ~dkey:Printing.astral_query
      "ENTL id = %s \n\
      \ Native: \n\
      \ LHS: %a \n\
      \ RHS: %a \n\
      \ Astral: \n\
      \ LHS: %a \n\n\
      \ RHS: %a \n\
      \ RESULT: %b"
      (if cached then "(cached)" else string_of_int @@ Solver.query_id ())
      Formula.pp_state lhs Formula.pp_state rhs SL.pp astral_lhs SL.pp
      astral_rhs result;
    Async.yield ());

  result

let check_inequality (lhs : Formula.var) (rhs : Formula.var)
    (formula : Formula.t) : bool =
  formula |> Formula.add_eq lhs rhs |> check_sat |> not
