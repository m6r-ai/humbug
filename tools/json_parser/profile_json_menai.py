#!/usr/bin/env python3
"""
Profile the Menai JSON parser against the standard test suite.

Usage:
    python profile_json_menai.py                  # Benchmarks only
    python profile_json_menai.py --profile        # Benchmarks + cProfile
    python profile_json_menai.py --output f.prof  # Save profile data
    python profile_json_menai.py --top 50         # Show top 50 functions
    python profile_json_menai.py --sort time      # Sort by time instead of cumulative
    python profile_json_menai.py --repeat 10      # Repeat each test N times
"""

import argparse
import cProfile
import pstats
import sys
import time
from io import StringIO
from pathlib import Path

from menai import Menai


# Path to the menai_modules directory containing json_parser.menai
MENAI_MODULES_DIR = str(Path(__file__).parent.parent.parent / "menai_modules")


# ---------------------------------------------------------------------------
# Shared test cases — must match profile_json_python.py exactly
# ---------------------------------------------------------------------------

def make_test_cases() -> list[tuple[str, str]]:
    """Return (label, json_string) pairs."""
    long_string = '"' + ('abcdefghij' * 200) + '"'
    deep_array  = ('[' * 500) + '0' + (']' * 500)

    return [
        ("object",       '{"name": "Alice", "age": 30, "active": true, "score": 9.5, "tags": ["admin", "user"], "address": {"city": "Wonderland", "zip": null}}'),
        ("integer",      '42'),
        ("float",        '-3.14'),
        ("true",         'true'),
        ("false",        'false'),
        ("null",         'null'),
        ("string_esc",   '"hello\\nworld"'),
        ("empty_array",  '[]'),
        ("empty_object", '{}'),
        ("nested",       '[1, [2, [3]]]'),
        ("long_string",  long_string),
        ("deep_array",   deep_array),
    ]


def make_expression(json_str: str) -> str:
    """Wrap a JSON string in a Menai parse call, escaping for Menai string syntax."""
    # Escape backslashes and double quotes for embedding in a Menai string literal
    escaped = json_str.replace('\\', '\\\\').replace('"', '\\"')
    return f'(let ((json (import "json_parser"))) ((dict-get json "parse") "{escaped}"))'


def run_benchmarks(
    menai: Menai,
    test_cases: list[tuple[str, str]],
    repeat: int
) -> list[tuple[str, float, bool]]:
    """Run all test cases, report per-test timing."""
    results = []

    print(f"\n{'Test':>15}  {'Time (ms)':>12}  {'Reps':>6}  Status")
    print("-" * 50)

    for label, json_str in test_cases:
        expr = make_expression(json_str)
        try:
            # Correctness check
            result = menai.evaluate(expr)
            success = result is not None or json_str.strip() == 'null'

            # Timing
            start = time.perf_counter()
            for _ in range(repeat):
                menai.evaluate(expr)
            elapsed_ms = (time.perf_counter() - start) / repeat * 1000

            print(f"{label:>15}  {elapsed_ms:>12.4f}  {repeat:>6}  ✓")
            results.append((label, elapsed_ms, success))

        except Exception as e:
            print(f"{label:>15}  {'ERROR':>12}  {repeat:>6}  ✗ {e}")
            results.append((label, 0.0, False))

    return results


def run_profile(
    menai: Menai,
    test_cases: list[tuple[str, str]],
    repeat: int,
    output_file: str | None,
    top_n: int,
    sort_by: str
) -> None:
    """Run cProfile across all test cases combined."""
    print(f"\nProfiling Menai JSON parser ({repeat} reps per test)...")
    print("=" * 100)

    expressions = [(label, make_expression(js)) for label, js in test_cases]

    profiler = cProfile.Profile()
    profiler.enable()

    try:
        for _label, expr in expressions:
            for _ in range(repeat):
                menai.evaluate(expr)
    finally:
        profiler.disable()

    print("=" * 100)
    print(f"PROFILING RESULTS (Top {top_n} functions, sorted by {sort_by})")
    print("=" * 100)

    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats(sort_by)
    stats.print_stats(top_n)
    print(s.getvalue())

    if output_file:
        profiler.dump_stats(output_file)
        print(f"✓ Profile data saved to: {output_file}")
        print(f"  View with: python -m pstats {output_file}")
        print(f"  Or: snakeviz {output_file}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Profile the Menai JSON parser",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('--profile', action='store_true', help='Run cProfile')
    parser.add_argument('--output', metavar='FILE', help='Save profile data to file')
    parser.add_argument('--top', type=int, default=50, metavar='N',
                        help='Show top N functions in profile (default: 50)')
    parser.add_argument('--sort', default='cumulative',
                        choices=['cumulative', 'time', 'calls', 'name', 'filename'],
                        help='Sort profile results by this metric (default: cumulative)')
    parser.add_argument('--repeat', type=int, default=10, metavar='N',
                        help='Repeat each test N times for stable timing (default: 10)')

    args = parser.parse_args()

    test_cases = make_test_cases()

    menai = Menai()
    menai.set_module_path([MENAI_MODULES_DIR])

    # Warm up: compile prelude and json_parser module before timing
    print("Warming up (compiling prelude and json_parser module)...")
    menai.evaluate('(let ((json (import "json_parser"))) (dict-get json "parse"))')
    print("Ready.")

    print("\nMenai JSON parser benchmark")
    print("=" * 50)
    print(f"Test cases:       {len(test_cases)}")
    print(f"Repeats per test: {args.repeat}")
    print(f"Module path:      {MENAI_MODULES_DIR}")

    results = run_benchmarks(menai, test_cases, args.repeat)

    total_ms = sum(t for _, t, _ in results)
    passed   = sum(1 for _, _, ok in results if ok)
    print(f"\nTotal avg time per full suite: {total_ms:.4f} ms")
    print(f"Passed: {passed}/{len(results)}")

    if args.profile or args.output:
        run_profile(menai, test_cases, args.repeat, args.output, args.top, args.sort)


if __name__ == '__main__':
    main()
