open Astral
open Common

(** This module implements wrapper functions for Astral's sat and entailment
    queries, with the caching of query results *)

(** Reference to a backend solver module *)
let convertor = ref (module Astral_v1 : Astral_convertor_sig.CONVERTOR)

let convert (formula : Formula.t) : Astral.SL.t =
  let module Convertor = (val !convertor) in
  Convertor.convert formula

let convert_state (state : Formula.state) : Astral.SL.t =
  let module Convertor = (val !convertor) in
  state |> List.map Convertor.convert |> SL.mk_or

let cnt = ref 0

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
  let () =
    convertor :=
      match Config.Astral_mode.get () with
      | `Old -> (module Astral_v1)
      | `New -> (module Astral_v2)
  in
  let module C = (val !convertor) in
  Common.solver := Some (C.init ~dump_queries ~backend ~encoding ())

let check_sat (formula : Formula.t) : bool =
  let astral_formula = convert formula in
  let cache_input = Formula.canonicalize formula in

  let cached, result =
    match Hashtbl.find_opt !sat_cache cache_input with
    | Some result -> (true, result)
    | None ->
        let start = Unix.gettimeofday () in
        let result = Solver.check_sat (Option.get !solver) astral_formula in
        cnt := !cnt + 1;
        Config.Self.debug ~level:2 "Query (sat) %d: %f -> %b \n" !cnt
          (Unix.gettimeofday () -. start)
          result;
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
  let cache_input =
    (Formula.canonicalize_state lhs, Formula.canonicalize_state rhs)
  in

  let astral_lhs, astral_rhs = (convert_state lhs, convert_state rhs) in
  let fresh_vars_lhs = SL.get_vars astral_lhs |> List.filter is_fresh_var in
  let fresh_vars_rhs = SL.get_vars astral_rhs |> List.filter is_fresh_var in
  let existential_vars =
    SL.Variable.Set.(
      elements @@ diff (of_list fresh_vars_rhs) (of_list fresh_vars_lhs))
  in
  let astral_rhs = SL.mk_exists existential_vars astral_rhs in

  let cached, result =
    match Hashtbl.find_opt !entl_cache cache_input with
    | Some result -> (true, result)
    | None ->
        let start = Unix.gettimeofday () in
        let result =
          Solver.check_entl (Option.get !solver) astral_lhs astral_rhs
        in
        cnt := !cnt + 1;
        Config.Self.debug ~level:2 "Query (entl) %d: %f -> %b \n" !cnt
          (Unix.gettimeofday () -. start)
          result;
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
