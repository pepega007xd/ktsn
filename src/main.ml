open Config
open Dataflow2
open Astral
open Common

(** This module is the entrypoint of the analysis, it runs the preprocessing and
    the dataflow analysis itself *)

module ForwardsAnalysis = Forwards (Analysis)

let run_analysis () =
  Func_call.compute_function := ForwardsAnalysis.compute;

  (* initialize the solver instance *)
  Astral_query.init ();

  (* run the preprocessing passes *)
  Preprocessing.preprocess ();

  let main, _ = Globals.entry_point () in
  let first_stmt = Kernel_function.find_first_stmt main in

  (* set [emp] as the initial state for the analysis *)
  Hashtbl.add !Func_call.function_context.results first_stmt [ [] ];

  (* run the dataflow analysis *)
  ForwardsAnalysis.compute [ first_stmt ];

  (* check if there are any allocations left after the main function *)
  let return_stmt = Kernel_function.find_return main in
  let final_state =
    Hashtbl.find !Func_call.function_context.results return_stmt
  in
  List.iter
    (fun formula ->
      Formula.get_spatial_atoms formula |> function
      | atom :: _ -> Formula.report_bug (Invalid_memtrack (atom, formula))
      | _ -> ())
    final_state;

  Self.result "Successful_verification"

let main () =
  Printexc.record_backtrace true;

  (* run the analysis and catch exceptions representing bug detections *)
  (try run_analysis () with
  | Formula.Bug _ when !Analysis.unknown_condition_reached ->
      Self.result "Unknown_result"
  | Formula.Bug (bug_type, pos) ->
      if Config.Svcomp_mode.get () then Witness.write_witness bug_type pos;
      (* print the type of detected bug *)
      Self.result "%a" Formula.pp_bug_type bug_type
  | e ->
      if Config.Catch_exceptions.get () then (
        let backtrace = Printexc.get_backtrace () in
        Self.warning "EXCEPTION: %s" (Printexc.to_string e);
        Self.warning "BACKTRACE: \n%s" backtrace)
      else raise e);

  (* dump analysis results *)
  Solver.dump_stats (Option.get !solver);
  Func_call.merge_all_results ();
  Self.result "Astral time: %.2f" !Astral_query.solver_time

(* register the analysis entrypoint into Frama-C  *)
let () =
  Boot.Main.extend (function
    | _ when Print_version.get () -> print_endline "0.1"
    | _ when Enable_analysis.get () -> main ()
    | _ -> ())
