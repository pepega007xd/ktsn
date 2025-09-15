# KTSN Static Analyzer

KTSN is a static analyzer for C programs built in OCaml. The tool is built to detect memory safety bugs and to verify the correctness of programs that work with linked lists. The analysis method is based on dataflow analysis with separation logic as the abstract domain.

## Installation

This installation guide was tested on Fedora 41 and Ubuntu 24.04, but it should work on any recent Linux distribution.

- Install OCaml using [this guide](https://ocaml.org/install).

- Install the necessary dependencies:

  ```bash
  opam install . --deps-only
  ```

- Install the Astral solver outside of this repository:

  ```bash
  git clone https://github.com/TDacik/Astral.git
  cd Astral
  git checkout dev-stable
  opam install .
  ```

- Build and install KTSN. Inside this repository, run:

  ```bash
  dune build
  dune install
  ```

## Usage

To run the analysis of a single program in the terminal, use:
```bash
frama-c -sl <filename.c>
```

To run with the recommended settings in Ivette (Frama-C GUI), use:
```bash
make run <filename.c>
```

_Note: when running Ivette for the first time, you need to install its dependencies according to instructions printed to the terminal_

To run with the recommended settings without constant propagation and loop unrolling, use:
```bash
make run-direct <filename.c>
```

To run the full benchmark suite, you must first clone the repository with SV-COMP benchmarks into `../sv-benchmarks` relative to this repository:
```bash
git clone https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks.git ../sv-benchmarks --depth 1 --branch svcomp25
```

Then, you can run the benchmarks and display the results:
```bash
make benchmark
make results
```

_Note: for running benchmarks, you need to first install Benchexec using [this guide](https://github.com/sosy-lab/benchexec/blob/main/doc/INSTALL.md)._

## Analysis Example

Results of the analysis displayed in Ivette (Frama-C GUI frontend):

![Ivette screenshot](bp/ivette_screenshot.png)

## Configuration options

- `-sl` - Enable the analysis
- `-sl-astral-debug` - Print debug info about queries to Astral
- `-sl-astral-encoding <Bitvectors | Sets>` - Which location encoding should Astral use, default: Bitvectors
- `-sl-backend-solver <Auto | Bitwuzla | CVC5 | Z3>` - Which solver should be used by Astral, default: Auto
- `-sl-benchmark-mode` - Enables features needed to run benchmarks
- `-sl-catch-exceptions` - Catch exceptions in main function (disable this for benchmarks) (set by default, opposite option is -sl-no-catch-exceptions)
- `-sl-dump-queries` - Dump Astral queries to the `astral_queries` directory.
- `-sl-edge-abstraction` - Do abstraction on every edge between statements (default: abstraction is done on loop return)
- `-sl-edge-deduplication` - Deduplicate states on every edge between statements
- `-sl-max-loop-cycles <N>` - If set, the analysis will traverse loops only `N` times
- `-sl-print-sort` - Print sort of variables along with their names
- `-sl-simple-join` - Compute join of states using entailment on single formulae (default: entailments of disjunctions are computed)

## Repository structure

```
├── bench -- files for running Benchexec benchmarks
│   ├── ktsn -- Benchexec tool definition for KTSN
│   │   ├── __init__.py
│   │   └── ktsn.py
│   ├── ktsn-results.xml -- Description for rendering HTML results
│   ├── ktsn.xml -- Definition of SV-COMP and custom benchmarks
│   └── setup.py
├── bp -- Typst source files and assets for the thesis
│   ├── ...
│   ├── template.typ
│   └── main.typ -- Text of the thesis
├── dune-project -- OCamp project declaration
├── excel_at_fit -- Poster and abstract from Excel@FIT 2025
│   ├── abstract.pdf
│   └── poster.pdf
├── Makefile -- makefile with useful shortcut commands
├── README.md
├── src -- Source code of the analyzer
│   ├── ...
│   ├── analysis.ml
│   └── formula.ml
├── test_programs -- Collection of custom tests for the analyzer
│   ├── ...
│   ├── nls_memory_leak.c -- Test program
│   └── nls_memory_leak.yml -- benchmark declaration for Benchexec
└── thesis.pdf -- Final PDF of the thesis
```

## Code Structure

- `src/main.ml` - entrypoint of the analyzer, runs all phases of the analysis in order and handles exceptions that signify found bugs, uses `analysis.ml` and `preprocessing.ml`
- `src/analysis.ml` - implements the dataflow analysis interface required by Frama-C, uses `transfer.ml`, `simplification.ml`, and `abstraction.ml`
- `src/transfer.ml` - implements the transfer function for dataflow analysis, uses `func_call.ml`
- `src/func_call.ml` - implements interprocedural analysis (transfer function for function calls)
- `src/simplification.ml` - implements most of the formula simplifications
- `src/abstraction.ml` - implements the abstraction of spatial atoms into list predicates
- `src/astral_query.ml` - wrapper for the Astral solver's API, implements caching of queries
- `src/formula.ml` - module that implements the type representing SL formulae in the analysis, and a set of functions for their manipulation
- `src/preprocessing/preprocessing.ml` - entrypoint of all preprocessing, implements multiple simple preprocessing passes
- `src/preprocessing/types.ml` - implements the analysis of C types that determines, which type of linked list they represent
- `src/preprocessing/block_builder.ml` - implements splitting expressions into a series of basic assignments
- `src/preprocessing/condition_split.ml` - implements preprocessing of conditional statements
- `src/preprocessing/stmt_split.ml` - implements preprocessing of assignments and function calls
- `src/preprocessing/instruction_type.ml` - implements the type representing the basic instructions, for which the transfer function is implemented
- `src/preprocessing/constants.ml` - contains common constants used by the preprocessing passes
- `src/common.ml` - common code used by other modules
- `src/config.ml` - definitions of command-line parameters
- `src/printing.ml` - definines categories of debug output
- `src/ivette.ml` - definitions of data exposed to Ivette (Frama-C GUI)
- `src/testing.ml` - utility functions for unit tests in other modules, unit tests for the `formula.ml` module
