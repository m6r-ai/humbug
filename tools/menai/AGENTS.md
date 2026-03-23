# AGENTS.md — tools/menai

Developer tools for working with Menai source files and bytecode. Each tool is
a standalone Python script that adds `src/` to its own path, so all can be run
from any working directory.

For invocation details (flags, options, examples) read the tool's own source
file or its README where one exists.

## checker — `checker/check.py`

Validates parenthesis balance in a `.menai` file. Uses the Menai lexer so that
parentheses inside strings and comments are correctly ignored.

Produces a line-by-line depth chart and identifies the exact location of any
imbalance. Closing parentheses can optionally be annotated with the special
form they close (`lambda`, `let`, `letrec`, `if`, `match`). Supports ANSI
colour output with depth-matched colouring on parentheses, line numbers, and
annotations. Exit codes are suitable for CI/CD use.

## disassembler — `disassembler/disassemble.py`

Compiles a `.menai` file and prints annotated bytecode for every `CodeObject`
in the module, recursing into nested closures.

Each function section lists its code-objects table, constants table, input
parameters, captured free variables, and an annotated instruction listing.
Instructions are annotated with what they load, call, or close over. Jump
targets are marked and control-flow boundaries are visually separated. An
optional call-trace mode summarises which functions call which.

## pretty-print — `pretty-print/pretty-print.py`

Formats Menai source code using `MenaiPrettyPrinter` from `src/menai`. Reads
from a file or stdin and writes to stdout, a file, or back in-place.

Short expressions are kept on one line; longer ones are broken across lines
with consistent indentation. `lambda`, `if`, `match`, `let`, `let*`, and
`letrec` receive special multi-line layouts with aligned bindings. End-of-line
and standalone comments are preserved. A check mode (exit 0/1) is available
for use in pre-commit hooks or CI.

## profiler — `profiler/profile.py`

Compiles and executes a `.menai` file under Python's `cProfile`, then prints a
summary of the hottest call sites. Prelude compilation happens before profiling
starts and is excluded from the results, so the profile reflects only the user
program's execution cost.

Raw profile data can optionally be saved to a `.prof` file for further
inspection with `pstats` or `snakeviz`. Benchmark programs are in
`tests/` (e.g. `sudoku-solver.menai`, `rubiks_cube.menai`, `list-sort.menai`).

## runner — `runner/run.py`

Discovers `*_test.menai` files, parses their test trees, and executes each
leaf thunk in an isolated VM invocation. A runtime error in one test does not
affect any other.

Test files export a dict with a `"tests"` key containing a node-list — a
nested structure of named groups (branches) and zero-argument lambdas (leaves).
The runner reports pass/fail per leaf with the full group path, and exits with
a non-zero code if any test fails. An optional name filter runs only the tests
whose path contains a given substring.

The `menai_test` support module (`runner/menai_test.menai`) is injected by the
runner and provides `assert-equal` (polymorphic structural equality check) and
the internal `test-find` navigation helper. It is not part of the standard
`menai_modules/` library.
