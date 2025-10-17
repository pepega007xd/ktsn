(** This module contains the definitions of the command-line parameters *)

module Self = Plugin.Register (struct
  let name = "KTSN Static Analyzer"
  let shortname = "ktsn"
  let help = "Static analysis using separation logic"
end)

module Enable_analysis = Self.False (struct
  let option_name = "-ktsn"
  let help = "Run analysis"
end)

module Dump_queries = Self.False (struct
  let option_name = "-ktsn-dump-queries"
  let help = "Dump Astral queries to 'astral_queries' directory."
end)

module Edge_abstraction = Self.False (struct
  let option_name = "-ktsn-edge-abstraction"

  let help =
    "Do abstraction on every edge between stmts (default: abstraction is done \
     on loop return)"
end)

module Edge_deduplication = Self.True (struct
  let option_name = "-ktsn-edge-deduplication"

  let help =
    "Deduplicate states using join (entailments) on every edge between stmts"
end)

module Backend_solver = Self.Enum (struct
  let option_name = "-ktsn-backend-solver"
  let help = "Which solver should be used by Astral, default: Auto"

  type t = Astral.Options.backend

  let default = `Bitwuzla

  let values =
    [ (`Bitwuzla, "Bitwuzla"); (`CVC5, "CVC5"); (`Z3, "Z3"); (`Auto, "Auto") ]
end)

module Astral_mode = Self.Enum (struct
  let option_name = "-ktsn-astral-mode"

  let help =
    "Old == builtin predicate encoding, New == user defined predicates \
     (default)"

  type t = [ `Old | `New ]

  let default = `New
  let values = [ (`Old, "old"); (`New, "new") ]
end)

module Astral_encoding = Self.Enum (struct
  let option_name = "-ktsn-astral-encoding"
  let help = "Which location encoding should Astral use, default: Bitvectors"

  type t = Astral.Options.encoding

  let default = `Bitvectors
  let values = [ (`Bitvectors, "Bitvectors"); (`Sets, "Sets") ]
end)

module Print_sort = Self.False (struct
  let option_name = "-ktsn-print-sort"
  let help = "Print sort of variables along with their names"
end)

module Simple_join = Self.True (struct
  let option_name = "-ktsn-simple-join"
  let help = "Compute join of states using entailment on single formulas"
end)

module Astral_debug = Self.False (struct
  let option_name = "-ktsn-astral-debug"
  let help = "Print info about queries to Astral"
end)

module Benchmark_mode = Self.False (struct
  let option_name = "-ktsn-benchmark-mode"

  let help =
    "SV-COMP exit functions are treated as exits, allocations are infallible"
end)

module Max_loop_cycles = Self.Int (struct
  let option_name = "-ktsn-max-loop-cycles"
  let help = "If set, the analysis will traverse loops only N times"
  let arg_name = "N"
  let default = -1
end)

module Catch_exceptions = Self.True (struct
  let option_name = "-ktsn-catch-exceptions"
  let help = "Catch exceptions in main function (disable for benchmarks)"
end)
