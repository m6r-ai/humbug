#!/usr/bin/env python3
"""
Profile the pure Python JSON parser against the standard test suite.

Usage:
    python profile_json_python.py                  # Benchmarks only
    python profile_json_python.py --profile        # Benchmarks + cProfile
    python profile_json_python.py --output f.prof  # Save profile data
    python profile_json_python.py --top 50         # Show top 50 functions
    python profile_json_python.py --sort time      # Sort by time instead of cumulative
    python profile_json_python.py --repeat 1000    # Repeat each test N times
"""

import argparse
import cProfile
import pstats
import sys
import time
from io import StringIO
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from json_parser import parse


# ---------------------------------------------------------------------------
# Shared test cases — must match profile_json_menai.py exactly
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


def run_benchmarks(test_cases: list[tuple[str, str]], repeat: int) -> list[tuple[str, float, bool]]:
    """Run all test cases, report per-test timing."""
    results = []

    print(f"\n{'Test':>15}  {'Time (ms)':>12}  {'Reps':>6}  Status")
    print("-" * 50)

    for label, json_str in test_cases:
        try:
            # Correctness check
            result = parse(json_str)
            success = True

            # Timing
            start = time.perf_counter()
            for _ in range(repeat):
                parse(json_str)
            elapsed_ms = (time.perf_counter() - start) / repeat * 1000

            print(f"{label:>15}  {elapsed_ms:>12.4f}  {repeat:>6}  ✓")
            results.append((label, elapsed_ms, success))

        except Exception as e:
            print(f"{label:>15}  {'ERROR':>12}  {repeat:>6}  ✗ {e}")
            results.append((label, 0.0, False))

    return results


def run_profile(
    test_cases: list[tuple[str, str]],
    repeat: int,
    output_file: str | None,
    top_n: int,
    sort_by: str
) -> None:
    """Run cProfile across all test cases combined."""
    print(f"\nProfiling Python JSON parser ({repeat} reps per test)...")
    print("=" * 100)

    profiler = cProfile.Profile()
    profiler.enable()

    try:
        for _label, json_str in test_cases:
            for _ in range(repeat):
                parse(json_str)
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
        description="Profile the pure Python JSON parser",
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
    parser.add_argument('--repeat', type=int, default=1000, metavar='N',
                        help='Repeat each test N times for stable timing (default: 1000)')

    args = parser.parse_args()

    test_cases = make_test_cases()

    print("Python JSON parser benchmark")
    print("=" * 50)
    print(f"Test cases:       {len(test_cases)}")
    print(f"Repeats per test: {args.repeat}")

    results = run_benchmarks(test_cases, args.repeat)

    total_ms = sum(t for _, t, _ in results)
    passed   = sum(1 for _, _, ok in results if ok)
    print(f"\nTotal avg time per full suite: {total_ms:.4f} ms")
    print(f"Passed: {passed}/{len(results)}")

    if args.profile or args.output:
        run_profile(test_cases, args.repeat, args.output, args.top, args.sort)


if __name__ == '__main__':
    main()
