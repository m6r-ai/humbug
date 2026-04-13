# Menai Benchmark

Compares Menai performance against idiomatic Python and pure-functional Python
across a set of algorithmic benchmarks.

## Usage

Run from this directory with the virtual environment active:

```
python run.py                        # run all suites
python run.py --suite sort           # run only the sort suite
python run.py --suite sort sudoku    # run multiple suites
python run.py --iterations 3         # override iteration count on every case
python run.py --no-validate          # skip correctness checks (timing only)
```

## Structure

```
benchmark/
├── benchmark.py          # Framework: BenchmarkSuite, BenchmarkRunner, BenchmarkReporter
├── run.py                # CLI entry point — discovers and runs suites
├── README.md
└── suites/
    ├── json_parser/
    │   ├── suite.py          # JSON parser benchmark suite
    │   ├── json_parser.menai
    │   └── json_parser.py
    ├── rubiks/
    │   ├── suite.py          # Rubik's cube IDA* benchmark suite
    │   └── rubiks_cube.menai
    ├── sort/
    │   ├── suite.py          # Sort benchmark suite
    │   └── list-sort.menai
    └── sudoku/
        ├── suite.py          # Sudoku solver benchmark suite
        └── sudoku-solver.menai
```

## Implementations compared

Each suite benchmarks three implementations:

- **Menai** — the Menai language implementation, run via the Menai VM
- **Python (idiomatic)** — Python using its natural idioms (mutation, built-ins)
- **Python (functional)** — Python using the same pure-functional style as Menai (no mutation)

The first implementation (Menai) is the reference. All others are validated
against it using per-suite correctness checks.

## Suites

### JSON Parser
Parses JSON strings of varying structure and size using a hand-written parser
in Menai, Python's `json.loads()` in idiomatic Python, and an explicit-stack
pure-functional parser in functional Python. Twelve cases cover primitives
(integer, float, booleans, null), strings with escapes, empty collections,
nested arrays, a long string (~2000 chars), and a deeply nested array
(500 levels).

### Rubik's Cube
Solves scrambled Rubik's cubes using IDA* with a misplaced-stickers heuristic.
Six scramble depths from 1 to 6 moves.
Validation applies the returned solution to the scrambled cube and checks it
is solved.

### Sort
Sorts a list of random integers using `sort-list` in Menai, `sorted()` in
idiomatic Python, and a recursive merge sort in functional Python.
Sizes: 10, 50, 100, 250, 500, 1000, 2000 elements.

### Sudoku
Solves sudoku puzzles using a backtracking solver. Four difficulty levels:
easy (36 givens), medium (30), hard (25), expert (23).
Validation checks that every row, column, and 3×3 box contains digits 1–9.

## Adding a new suite

1. Create `suites/<name>/suite.py` containing a class named `Suite` that
   subclasses `BenchmarkSuite` from `benchmark`.
2. Implement `cases()`, `implementations()`, and `results_equal()`.
3. Place any required `.menai` files in the same directory.

The runner discovers suites automatically via `suites/*/suite.py`.
