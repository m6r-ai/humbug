#!/usr/bin/env python3
"""
Profile sort-list performance across various list sizes.

This script benchmarks the Menai sort-list prelude function to understand
VM performance characteristics and identify optimization opportunities.

Usage:
    python profile_list_sort.py                      # Run benchmarks only
    python profile_list_sort.py --profile            # Run with cProfile on largest size
    python profile_list_sort.py --output stats.prof  # Save profile data
    python profile_list_sort.py --top 50             # Show top 50 functions
    python profile_list_sort.py --sort time          # Sort by time instead of cumulative
    python profile_list_sort.py --sizes 10,50,100    # Custom list sizes
    python profile_list_sort.py --iterations 5       # Iterations per size (default: 10)
"""

import argparse
import cProfile
import pstats
import random
import sys
import time
from io import StringIO
from pathlib import Path

_SCRIPT_DIR = Path(__file__).resolve().parent
_REPO_ROOT = _SCRIPT_DIR.parent.parent.parent
sys.path.insert(0, str(_REPO_ROOT / "src"))

from menai import Menai


DEFAULT_SIZES = [10, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
DEFAULT_ITERATIONS = 10
MENAI_FILE = _SCRIPT_DIR / "list-sort.menai"


def load_sort_source() -> str:
    """Read the sort lambda expression from list-sort.menai."""
    if not MENAI_FILE.exists():
        print(f"Error: Menai file not found: {MENAI_FILE}")
        sys.exit(1)

    return MENAI_FILE.read_text(encoding="utf-8").strip()


def make_sort_expression(sort_src: str, values: list[int]) -> str:
    """Wrap the sort lambda with a concrete list of integers to sort."""
    items = " ".join(str(v) for v in values)
    return f"({sort_src} (list {items}))"


def run_benchmarks(sort_src: str, sizes: list[int], iterations: int) -> list[tuple[int, float, float, bool]]:
    """
    Run sort benchmarks for each size, repeated `iterations` times.

    Returns list of (size, mean_seconds, min_seconds, success) tuples.
    """
    menai = Menai()

    # Warm up: compile prelude and exercise the sort path once
    menai.evaluate(make_sort_expression(sort_src, random.sample(range(30), 3)))

    results = []

    print(f"\n{'Size':>8}  {'Mean (s)':>10}  {'Min (s)':>10}  {'Items/s':>12}  {'Status'}")
    print("-" * 60)

    for size in sizes:
        times = []
        all_ok = True

        for _ in range(iterations):
            values = random.sample(range(size * 10), size)
            expr = make_sort_expression(sort_src, values)

            try:
                start = time.perf_counter()
                result = menai.evaluate(expr)
                elapsed = time.perf_counter() - start

                sorted_ok = all(
                    result[i] <= result[i + 1]
                    for i in range(len(result) - 1)
                )
                if not sorted_ok:
                    all_ok = False

                times.append(elapsed)

            except Exception as e:
                print(f"{size:>8}  {'ERROR':>10}  {'':>10}  {'':>12}  ✗ {e}")
                all_ok = False
                break

        if times:
            mean_t = sum(times) / len(times)
            min_t = min(times)
            items_per_sec = size / mean_t if mean_t > 0 else float('inf')
            status = "✓" if all_ok else "✗ WRONG ORDER"
            print(f"{size:>8}  {mean_t:>10.4f}  {min_t:>10.4f}  {items_per_sec:>12.1f}  {status}")
            results.append((size, mean_t, min_t, all_ok))
        else:
            results.append((size, 0.0, 0.0, False))

    return results


def run_profile(
    sort_src: str,
    size: int,
    iterations: int,
    output_file: str | None,
    top_n: int,
    sort_by: str,
) -> None:
    """Run cProfile on repeated sorts of the given size."""
    menai = Menai()

    # Warm up
    menai.evaluate(make_sort_expression(sort_src, random.sample(range(30), 3)))

    print(f"\nProfiling sort-list on {size} elements × {iterations} iterations...")
    print("=" * 100)

    profiler = cProfile.Profile()
    profiler.enable()

    try:
        for _ in range(iterations):
            values = random.sample(range(size * 10), size)
            menai.evaluate(make_sort_expression(sort_src, values))
        print(f"✓ Sorted {size} elements × {iterations} iterations successfully")
    except Exception as e:
        print(f"✗ Failed: {e}")
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

    if output_file:
        profiler.dump_stats(output_file)
        print(f"✓ Profile data saved to: {output_file}")
        print(f"  View with: python -m pstats {output_file}")
        print(f"  Or: snakeviz {output_file}")


def main():
    parser = argparse.ArgumentParser(
        description="Profile Menai sort-list performance",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument(
        '--profile',
        action='store_true',
        help='Run cProfile on the largest benchmark size'
    )
    parser.add_argument(
        '--profile-size',
        type=int,
        default=None,
        metavar='N',
        help='List size to use for profiling (default: largest benchmark size)'
    )
    parser.add_argument(
        '--output',
        metavar='FILE',
        help='Save profile data to file'
    )
    parser.add_argument(
        '--top',
        type=int,
        default=40,
        metavar='N',
        help='Show top N functions in profile (default: 40)'
    )
    parser.add_argument(
        '--sort',
        default='cumulative',
        choices=['cumulative', 'time', 'calls', 'name', 'filename'],
        help='Sort profile results by this metric (default: cumulative)'
    )
    parser.add_argument(
        '--sizes',
        metavar='N,N,...',
        help=f'Comma-separated list sizes to benchmark (default: {",".join(str(s) for s in DEFAULT_SIZES)})'
    )
    parser.add_argument(
        '--iterations',
        type=int,
        default=DEFAULT_ITERATIONS,
        metavar='N',
        help=f'Number of iterations per size (default: {DEFAULT_ITERATIONS})'
    )
    parser.add_argument(
        '--seed',
        type=int,
        default=42,
        help='Random seed for reproducible input (default: 42)'
    )

    args = parser.parse_args()

    random.seed(args.seed)

    sizes = DEFAULT_SIZES
    if args.sizes:
        try:
            sizes = [int(s.strip()) for s in args.sizes.split(',')]
        except ValueError:
            print(f"Error: --sizes must be comma-separated integers, got: {args.sizes}")
            raise SystemExit(1)

    sort_src = load_sort_source()

    print("Menai sort-list benchmark")
    print("=" * 50)
    print(f"Menai file  : {MENAI_FILE}")
    print(f"Sizes       : {sizes}")
    print(f"Iterations  : {args.iterations} per size")
    print(f"Random seed : {args.seed}")

    results = run_benchmarks(sort_src, sizes, args.iterations)

    print("\nSummary:")
    successful = [(s, t) for s, t, _, ok in results if ok]
    if len(successful) >= 2:
        s1, t1 = successful[-2]
        s2, t2 = successful[-1]
        if t1 > 0 and t2 > 0:
            ratio = t2 / t1
            size_ratio = s2 / s1
            print(f"  Scaling from {s1} → {s2} items: {ratio:.2f}x slower ({size_ratio:.1f}x more items)")
            print(f"  Expected for O(n log n): ~{size_ratio * (s2.bit_length() / s1.bit_length()):.2f}x")

    if args.profile or args.output:
        profile_size = args.profile_size or sizes[-1]
        run_profile(sort_src, profile_size, args.iterations, args.output, args.top, args.sort)


if __name__ == '__main__':
    main()
