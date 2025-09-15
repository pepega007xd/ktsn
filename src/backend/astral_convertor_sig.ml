(** Signature of backend. *)

open Astral

module type CONVERTOR = sig
  
  val init : 
    backend : Options.backend ->
    encoding : Options.encoding ->
    dump_queries : [`None | `Full of string] ->
    unit -> Solver.solver

  val convert : Formula.t -> Astral.SL.t

end
