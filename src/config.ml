(** This module contains the definitions of the command-line parameters *)

module Self = Plugin.Register (struct
  let name = "SEAL Static Analyzer"
  let shortname = "seal"
  let help = "Static analysis using separation logic"
end)

module Enable_analysis = Self.False (struct
  let option_name = "-seal"
  let help = "Run analysis"
end)

module Dump_queries = Self.False (struct
  let option_name = "-seal-dump-queries"
  let help = "Dump Astral queries to 'astral_queries' directory."
end)

module Edge_abstraction = Self.False (struct
  let option_name = "-seal-edge-abstraction"

  let help =
    "Do abstraction on every edge between stmts (default: abstraction is done \
     on loop return)"
end)

module Edge_deduplication = Self.True (struct
  let option_name = "-seal-edge-deduplication"

  let help =
    "Deduplicate states using join (entailments) on every edge between stmts"
end)

module Backend_solver = Self.Enum (struct
  let option_name = "-seal-backend-solver"
  let help = "Which solver should be used by Astral, default: Auto"

  type t = Astral.Options.backend

  let default = `Bitwuzla

  let values =
    [ (`Bitwuzla, "Bitwuzla"); (`CVC5, "CVC5"); (`Z3, "Z3"); (`Auto, "Auto") ]
end)

module Astral_mode = Self.Enum (struct
  let option_name = "-seal-astral-mode"

  let help =
    "Old == builtin predicate encoding, New == user defined predicates \
     (default)"

  type t = [ `Old | `New ]

  let default = `New
  let values = [ (`Old, "old"); (`New, "new") ]
end)

module Astral_encoding = Self.Enum (struct
  let option_name = "-seal-astral-encoding"
  let help = "Which location encoding should Astral use, default: Bitvectors"

  type t = Astral.Options.encoding

  let default = `Bitvectors
  let values = [ (`Bitvectors, "Bitvectors"); (`Sets, "Sets") ]
end)

module Print_sort = Self.False (struct
  let option_name = "-seal-print-sort"
  let help = "Print sort of variables along with their names"
end)

module Simple_join = Self.True (struct
  let option_name = "-seal-simple-join"
  let help = "Compute join of states using entailment on single formulas"
end)

module Astral_debug = Self.False (struct
  let option_name = "-seal-astral-debug"
  let help = "Print info about queries to Astral"
end)

module Svcomp_mode = Self.False (struct
  let option_name = "-seal-svcomp-mode"

  let help =
    "SV-COMP exit functions are treated as exits, allocations are infallible"
end)

module Max_loop_cycles = Self.Int (struct
  let option_name = "-seal-max-loop-cycles"

  let help =
    "If set, the analysis will traverse loops only N times (default: disabled)"

  let arg_name = "N"
  let default = -1
end)

module Catch_exceptions = Self.True (struct
  let option_name = "-seal-catch-exceptions"

  let help =
    "Catch and print exceptions in main function (disable for benchmarks)"
end)

module Max_int_value = Self.Int (struct
  let option_name = "-seal-max-int"
  let default = 5
  let arg_name = "N"

  let help =
    Format.sprintf "Represent only integers within range [-N, N] (default: %i)"
      default
end)

module Print_version = Self.False (struct
  let option_name = "-seal-version"
  let help = "Print version and exit"
end)
