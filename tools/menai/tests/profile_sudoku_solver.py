#!/usr/bin/env python3
"""
Profile sudoku-solver.menai

Benchmarks the Menai sudoku solver across a suite of puzzles ranging from
easy (many givens) to hard (few givens), then optionally runs cProfile on
a chosen puzzle so you can drill into VM and compiler hotspots.

Usage:
    python profile_sudoku_solver.py                              # Benchmark all puzzles
    python profile_sudoku_solver.py --difficulty medium          # Benchmark up to medium
    python profile_sudoku_solver.py --profile                    # Benchmark + profile hardest run puzzle
    python profile_sudoku_solver.py --profile-difficulty easy    # Profile a specific difficulty
    python profile_sudoku_solver.py --output stats.prof          # Save profile data
    python profile_sudoku_solver.py --top 50                     # Show top 50 functions
    python profile_sudoku_solver.py --sort time                  # Sort by time instead of cumulative
    python profile_sudoku_solver.py --repeat 5                   # Repeat each puzzle N times for timing
"""

import argparse
import cProfile
import pstats
import sys
import time
from io import StringIO
from pathlib import Path

_SCRIPT_DIR = Path(__file__).resolve().parent
_REPO_ROOT = _SCRIPT_DIR.parent.parent.parent
sys.path.insert(0, str(_REPO_ROOT / "src"))
_MENAI_MODULES_DIR = _REPO_ROOT / "menai_modules"

from menai import Menai


# ---------------------------------------------------------------------------
# Puzzle suite
# Each entry is (label, board) where board is a list of 81 integers,
# row-major, with 0 for empty cells.
# ---------------------------------------------------------------------------

DIFFICULTY_NAMES = ["easy", "medium", "hard", "expert"]

# Ordered easy → expert; difficulty tag at index 1 matches DIFFICULTY_NAMES.
PUZZLES: list[tuple[str, str, list[int]]] = [
    (
        "Easy (36 givens)",
        "easy",
        [
            5, 3, 0,  0, 7, 0,  0, 0, 0,
            6, 0, 0,  1, 9, 5,  0, 0, 0,
            0, 9, 8,  0, 0, 0,  0, 6, 0,

            8, 0, 0,  0, 6, 0,  0, 0, 3,
            4, 0, 0,  8, 0, 3,  0, 0, 1,
            7, 0, 0,  0, 2, 0,  0, 0, 6,

            0, 6, 0,  0, 0, 0,  2, 8, 0,
            0, 0, 0,  4, 1, 9,  0, 0, 5,
            0, 0, 0,  0, 8, 0,  0, 7, 9,
        ],
    ),
    (
        "Medium (30 givens)",
        "medium",
        [
            0, 0, 0,  2, 6, 0,  7, 0, 1,
            6, 8, 0,  0, 7, 0,  0, 9, 0,
            1, 9, 0,  0, 0, 4,  5, 0, 0,

            8, 2, 0,  1, 0, 0,  0, 4, 0,
            0, 0, 4,  6, 0, 2,  9, 0, 0,
            0, 5, 0,  0, 0, 3,  0, 2, 8,

            0, 0, 9,  3, 0, 0,  0, 7, 4,
            0, 4, 0,  0, 5, 0,  0, 3, 6,
            7, 0, 3,  0, 1, 8,  0, 0, 0,
        ],
    ),
    (
        "Hard (25 givens)",
        "hard",
        [
            0, 0, 0,  6, 0, 0,  4, 0, 0,
            7, 0, 0,  0, 0, 3,  6, 0, 0,
            0, 0, 0,  0, 9, 1,  0, 8, 0,

            0, 0, 0,  0, 0, 0,  0, 0, 0,
            0, 5, 0,  1, 8, 0,  0, 0, 3,
            0, 0, 0,  3, 0, 6,  0, 4, 5,

            0, 4, 0,  2, 0, 0,  0, 6, 0,
            9, 0, 3,  0, 0, 0,  0, 0, 0,
            0, 2, 0,  0, 0, 0,  1, 0, 0,
        ],
    ),
    (
        "Expert (23 givens – near-minimal)",
        "expert",
        [
            8, 0, 0,  0, 0, 0,  0, 0, 0,
            0, 0, 3,  6, 0, 0,  0, 0, 0,
            0, 7, 0,  0, 9, 0,  2, 0, 0,

            0, 5, 0,  0, 0, 7,  0, 0, 0,
            0, 0, 0,  0, 4, 5,  7, 0, 0,
            0, 0, 0,  1, 0, 0,  0, 3, 0,

            0, 0, 1,  0, 0, 0,  0, 6, 8,
            0, 0, 8,  5, 0, 0,  0, 1, 0,
            0, 9, 0,  0, 0, 0,  4, 0, 0,
        ],
    ),
]


def puzzles_up_to(difficulty: str) -> list[tuple[str, str, list[int]]]:
    """Return all puzzles whose difficulty is <= the given level."""
    cutoff = DIFFICULTY_NAMES.index(difficulty)
    return [p for p in PUZZLES if DIFFICULTY_NAMES.index(p[1]) <= cutoff]


def puzzle_by_difficulty(difficulty: str) -> tuple[str, str, list[int]]:
    """Return the single puzzle matching the given difficulty name."""
    return next(p for p in PUZZLES if p[1] == difficulty)


def board_to_menai(flat: list[int]) -> str:
    """Convert a flat 81-element board into a Menai list-of-lists literal."""
    rows = []
    for r in range(9):
        cells = " ".join(str(flat[r * 9 + c]) for c in range(9))
        rows.append(f"(list {cells})")
    return "(list\n    " + "\n    ".join(rows) + ")"


def make_solve_expression(flat: list[int]) -> str:
    """Build a Menai expression that imports the solver module and solves one puzzle."""
    board_expr = board_to_menai(flat)
    return (
        f'(let ((sudoku (import "sudoku-solver")))\n'
        f'  (let ((solve-fn (dict-get sudoku "solve")))\n'
        f'    (solve-fn {board_expr})))\n'
    )


def run_benchmarks(
    repeat: int,
    puzzles: list[tuple[str, str, list[int]]],
) -> list[tuple[str, float, bool]]:
    """Solve every puzzle ``repeat`` times and report timing."""
    module_path = [str(_SCRIPT_DIR), str(_MENAI_MODULES_DIR)]
    menai = Menai(module_path=module_path)
    results: list[tuple[str, float, bool]] = []

    # Warm up (compile prelude, JIT caches, etc.)
    menai.evaluate("(integer+ 1 2)")

    print(f"\n{'Puzzle':<32}  {'Reps':>4}  {'Total (s)':>10}  {'Per run (ms)':>13}  Status")
    print("-" * 72)

    for label, _, flat in puzzles:
        expr = make_solve_expression(flat)
        try:
            start = time.perf_counter()
            for _ in range(repeat):
                result = menai.evaluate(expr)
            elapsed = time.perf_counter() - start

            ok = isinstance(result, list)
            per_run_ms = elapsed / repeat * 1000
            status = "✓ solved" if ok else "✗ no solution"
            print(f"{label:<32}  {repeat:>4}  {elapsed:>10.4f}  {per_run_ms:>13.2f}  {status}")
            results.append((label, elapsed / repeat, ok))

        except Exception as exc:
            print(f"{label:<32}  {repeat:>4}  {'ERROR':>10}  {'':>13}  ✗ {exc}")
            results.append((label, 0.0, False))

    return results


def run_profile(
    output_file: str | None,
    top_n: int,
    sort_by: str,
    puzzle: tuple[str, str, list[int]],
) -> None:
    """Run cProfile on one puzzle and print the results."""
    label, _, flat = puzzle
    expr = make_solve_expression(flat)

    module_path = [str(_SCRIPT_DIR), str(_MENAI_MODULES_DIR)]
    menai = Menai(module_path=module_path)
    menai.evaluate("(integer+ 1 2)")

    print(f"\nProfiling puzzle: {label}")
    print("=" * 100)

    profiler = cProfile.Profile()
    profiler.enable()

    try:
        result = menai.evaluate(expr)
        if isinstance(result, list):
            print(f"✓ Solved successfully")
        else:
            print(f"✗ No solution returned")
    except Exception as exc:
        print(f"✗ Failed with error: {exc}")
        raise
    finally:
        profiler.disable()

    print("\n" + "=" * 100)
    print(f"PROFILING RESULTS (Top {top_n} functions, sorted by {sort_by})")
    print("=" * 100)

    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats(sort_by)
    stats.print_stats(top_n)
    print(s.getvalue())

    print("=" * 100)
    print("KEY FUNCTION CALLERS")
    print("=" * 100)

    key_patterns = ["compile", "execute", "evaluate", "_lex", "_parse", "semantic", "desugar"]
    s2 = StringIO()
    stats2 = pstats.Stats(profiler, stream=s2)
    for pattern in key_patterns:
        print(f"\nCallers of functions matching '{pattern}':")
        stats2.print_callers(pattern)

    if output_file:
        profiler.dump_stats(output_file)
        print(f"\n✓ Profile data saved to: {output_file}")
        print(f"  View with: python -m pstats {output_file}")
        print(f"  Or:        snakeviz {output_file}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Profile the Menai sudoku solver",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--profile",
        action="store_true",
        help="Run cProfile after benchmarking (uses hardest puzzle by default)",
    )
    parser.add_argument(
        "--profile-difficulty",
        choices=DIFFICULTY_NAMES,
        default=None,
        metavar="LEVEL",
        help="Difficulty of the puzzle to profile: easy, medium, hard, expert (default: hardest run puzzle)",
    )
    parser.add_argument(
        "--difficulty",
        choices=DIFFICULTY_NAMES,
        default="expert",
        metavar="LEVEL",
        help="Run puzzles up to this difficulty: easy, medium, hard, expert (default: expert)",
    )
    parser.add_argument(
        "--output",
        metavar="FILE",
        help="Save profile data to file (for pstats / snakeviz)",
    )
    parser.add_argument(
        "--top",
        type=int,
        default=40,
        metavar="N",
        help="Show top N functions in profile output (default: 40)",
    )
    parser.add_argument(
        "--sort",
        default="cumulative",
        choices=["cumulative", "time", "calls", "name", "filename"],
        help="Sort profile results by this metric (default: cumulative)",
    )
    parser.add_argument(
        "--repeat",
        type=int,
        default=3,
        metavar="N",
        help="Number of times to repeat each puzzle for timing (default: 3)",
    )

    args = parser.parse_args()

    puzzles = puzzles_up_to(args.difficulty)

    print("Menai sudoku solver benchmark")
    print("=" * 72)
    print(f"Difficulty  : up to {args.difficulty}  ({len(puzzles)} puzzle(s))")
    print(f"Repeat      : {args.repeat}x per puzzle")

    results = run_benchmarks(args.repeat, puzzles)

    # Scaling summary
    solved = [(lbl, t) for lbl, t, ok in results if ok]
    if len(solved) >= 2:
        print("\nScaling summary:")
        for i in range(1, len(solved)):
            lbl_a, t_a = solved[i - 1]
            lbl_b, t_b = solved[i]
            if t_a > 0:
                print(f"  {lbl_a}  →  {lbl_b}: {t_b / t_a:.1f}x slower")

    if args.profile or args.output:
        if args.profile_difficulty:
            profile_puzzle = puzzle_by_difficulty(args.profile_difficulty)
        else:
            # Default: hardest puzzle that was actually benchmarked
            profile_puzzle = puzzles[-1]
        run_profile(args.output, args.top, args.sort, profile_puzzle)


if __name__ == "__main__":
    main()
