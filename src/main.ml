open Config
open Dataflow2
open Astral
module ForwardsAnalysis = Forwards (Analysis)

let run_analysis () =
  Func_call.compute_function := ForwardsAnalysis.compute;

  Astral_query.init ();
  Preprocessing.preprocess ();

  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  Hashtbl.add !Func_call.function_context.results first_stmt [ [] ];

  ForwardsAnalysis.compute [ first_stmt ];

  Func_call.merge_all_results ();

  Solver.dump_stats !Common.solver;
  Self.result "Astral took %.2f seconds" !Astral_query.solver_time

let main () =
  Printexc.record_backtrace true;
  try run_analysis () with
  | Formula.Invalid_deref (var, formula) ->
      Common.warning "Invalid_deref: var '%a' in formula '%a'" SL.Variable.pp
        var Formula.pp_formula formula
  | Formula.Invalid_free (var, formula) ->
      Common.warning "Invalid_free: var '%a' in formula '%a'" SL.Variable.pp var
        Formula.pp_formula formula
  | e ->
      if Config.Benchmark_mode.get () then raise e
      else
        let backtrace = Printexc.get_backtrace () in
        Self.warning "EXCEPTION: %s" (Printexc.to_string e);
        Self.warning "BACKTRACE: \n%s" backtrace

let () = Boot.Main.extend (fun () -> if Enable_analysis.get () then main ())
