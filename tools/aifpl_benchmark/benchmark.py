#!/usr/bin/env python3
"""
AIFPL Performance Benchmark Suite

Measures performance of AIFPL interpreter across various workload patterns
to identify bottlenecks and track optimization improvements over time.

Usage:
    python benchmark.py                    # Run all benchmarks
    python benchmark.py --quick            # Run quick benchmarks only
    python benchmark.py --profile          # Run with profiling
    python benchmark.py --compare old.json # Compare with previous results
"""

import argparse
import json
import statistics
import sys
import time
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Callable
import cProfile
import pstats
from io import StringIO

# Add src to path so we can import aifpl
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL


@dataclass
class BenchmarkResult:
    """Result from a single benchmark run."""
    name: str
    category: str
    expression: str
    iterations: int
    total_time: float
    mean_time: float
    median_time: float
    min_time: float
    max_time: float
    std_dev: float
    ops_per_sec: float

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return asdict(self)


@dataclass
class BenchmarkSuite:
    """Collection of benchmark results."""
    timestamp: str
    python_version: str
    aifpl_version: str
    results: List[BenchmarkResult]

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            'timestamp': self.timestamp,
            'python_version': self.python_version,
            'aifpl_version': self.aifpl_version,
            'results': [r.to_dict() for r in self.results]
        }


class Benchmark:
    """Individual benchmark test."""

    def __init__(
        self,
        name: str,
        category: str,
        expression: str,
        iterations: int = 100,
        warmup: int = 10
    ):
        self.name = name
        self.category = category
        self.expression = expression
        self.iterations = iterations
        self.warmup = warmup

    def run(self, aifpl: AIFPL) -> BenchmarkResult:
        """Run the benchmark and return results."""
        # Warmup
        for _ in range(self.warmup):
            aifpl.evaluate(self.expression)

        # Measure
        times = []
        for _ in range(self.iterations):
            start = time.perf_counter()
            aifpl.evaluate(self.expression)
            elapsed = time.perf_counter() - start
            times.append(elapsed)

        total_time = sum(times)
        mean_time = statistics.mean(times)
        median_time = statistics.median(times)
        min_time = min(times)
        max_time = max(times)
        std_dev = statistics.stdev(times) if len(times) > 1 else 0.0
        ops_per_sec = 1.0 / mean_time if mean_time > 0 else 0.0

        return BenchmarkResult(
            name=self.name,
            category=self.category,
            expression=self.expression,
            iterations=self.iterations,
            total_time=total_time,
            mean_time=mean_time,
            median_time=median_time,
            min_time=min_time,
            max_time=max_time,
            std_dev=std_dev,
            ops_per_sec=ops_per_sec
        )


# Define benchmark suite
BENCHMARKS = [
    # === ARITHMETIC OPERATIONS ===
    Benchmark(
        "Simple Addition",
        "arithmetic",
        "(+ 1 2 3 4 5)",
        iterations=1000
    ),
    Benchmark(
        "Nested Arithmetic",
        "arithmetic",
        "(* (+ 1 2 3) (- 10 5) (/ 20 4))",
        iterations=1000
    ),
    Benchmark(
        "Complex Math",
        "arithmetic",
        "(sqrt (+ (* 3 3) (* 4 4)))",
        iterations=1000
    ),

    # === FUNCTION CALLS ===
    Benchmark(
        "Simple Lambda Call",
        "functions",
        "((lambda (x) (* x x)) 5)",
        iterations=1000
    ),
    Benchmark(
        "Lambda with Multiple Args",
        "functions",
        "((lambda (x y z) (+ (* x y) z)) 3 4 5)",
        iterations=1000
    ),
    Benchmark(
        "Nested Lambda Calls",
        "functions",
        "((lambda (x) ((lambda (y) (+ x y)) 10)) 5)",
        iterations=1000
    ),

    # === RECURSION ===
    Benchmark(
        "Factorial (10)",
        "recursion",
        """(let ((factorial (lambda (n)
                             (if (<= n 1)
                                 1
                                 (* n (factorial (- n 1)))))))
             (factorial 10))""",
        iterations=100
    ),
    Benchmark(
        "Fibonacci (15)",
        "recursion",
        """(let ((fib (lambda (n)
                       (if (< n 2)
                           n
                           (+ (fib (- n 1)) (fib (- n 2)))))))
             (fib 15))""",
        iterations=10
    ),
    Benchmark(
        "Tail Recursive Sum (100)",
        "recursion",
        """(let ((sum-tail (lambda (n acc)
                            (if (<= n 0)
                                acc
                                (sum-tail (- n 1) (+ acc n))))))
             (sum-tail 100 0))""",
        iterations=100
    ),

    # === LIST OPERATIONS ===
    Benchmark(
        "List Creation (10 elements)",
        "lists",
        "(list 1 2 3 4 5 6 7 8 9 10)",
        iterations=1000
    ),
    Benchmark(
        "List Append",
        "lists",
        "(append (list 1 2 3) (list 4 5 6))",
        iterations=1000
    ),
    Benchmark(
        "List Reverse (20 elements)",
        "lists",
        "(reverse (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))",
        iterations=1000
    ),
    Benchmark(
        "Cons Building (10 elements)",
        "lists",
        "(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 ()))))))))))",
        iterations=1000
    ),

    # === HIGHER-ORDER FUNCTIONS ===
    Benchmark(
        "Map (10 elements)",
        "higher-order",
        "(map (lambda (x) (* x x)) (list 1 2 3 4 5 6 7 8 9 10))",
        iterations=500
    ),
    Benchmark(
        "Map (100 elements)",
        "higher-order",
        "(map (lambda (x) (* x x)) (range 1 101))",
        iterations=50
    ),
    Benchmark(
        "Filter (100 elements)",
        "higher-order",
        "(filter (lambda (x) (> x 50)) (range 1 101))",
        iterations=50
    ),
    Benchmark(
        "Fold (100 elements)",
        "higher-order",
        "(fold + 0 (range 1 101))",
        iterations=50
    ),
    Benchmark(
        "Map + Fold Pipeline (100 elements)",
        "higher-order",
        "(fold + 0 (map (lambda (x) (* x x)) (range 1 101)))",
        iterations=50
    ),

    # === LET BINDINGS ===
    Benchmark(
        "Simple Let (2 bindings)",
        "let",
        "(let ((x 5) (y 10)) (+ x y))",
        iterations=1000
    ),
    Benchmark(
        "Nested Let (3 levels)",
        "let",
        "(let ((a 1)) (let ((b 2)) (let ((c 3)) (+ a b c))))",
        iterations=1000
    ),
    Benchmark(
        "Let with Many Bindings (10)",
        "let",
        "(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) (+ a b c d e f g h i j))",
        iterations=500
    ),
    Benchmark(
        "Recursive Let Binding",
        "let",
        """(let ((factorial (lambda (n)
                             (if (<= n 1) 1 (* n (factorial (- n 1)))))))
             (factorial 5))""",
        iterations=500
    ),

    # === STRING OPERATIONS ===
    Benchmark(
        "String Append",
        "strings",
        '(string-append "hello" " " "world")',
        iterations=1000
    ),
    Benchmark(
        "String Manipulation",
        "strings",
        '(string-upcase (string-append "hello" " " "world"))',
        iterations=1000
    ),

    # === ALIST OPERATIONS ===
    Benchmark(
        "Alist Creation (5 pairs)",
        "alists",
        '(alist ("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5))',
        iterations=1000
    ),
    Benchmark(
        "Alist Get",
        "alists",
        '(alist-get (alist ("name" "Alice") ("age" 30) ("city" "NYC")) "age")',
        iterations=1000
    ),
    Benchmark(
        "Alist Set",
        "alists",
        '(alist-set (alist ("a" 1) ("b" 2) ("c" 3)) "d" 4)',
        iterations=1000
    ),
    Benchmark(
        "Alist Merge",
        "alists",
        '(alist-merge (alist ("a" 1) ("b" 2)) (alist ("c" 3) ("d" 4)))',
        iterations=1000
    ),

    # === COMPLEX REAL-WORLD SCENARIOS ===
    Benchmark(
        "Data Processing Pipeline",
        "complex",
        """(let ((data (map (lambda (i) (alist ("id" i) ("value" (* i i)))) (range 1 51))))
             (fold + 0 (map (lambda (item) (alist-get item "value")) data)))""",
        iterations=20
    ),
    Benchmark(
        "Nested Data Transformation",
        "complex",
        """(let ((transform (lambda (x) (* x 2)))
                   (filter-fn (lambda (x) (> x 10)))
                   (data (range 1 51)))
             (fold + 0 (filter filter-fn (map transform data))))""",
        iterations=20
    ),
]

# Quick benchmark subset for fast iteration
QUICK_BENCHMARKS = [b for b in BENCHMARKS if b.category in ["arithmetic", "functions", "lists"]]


def run_benchmarks(benchmarks: List[Benchmark], verbose: bool = True) -> List[BenchmarkResult]:
    """Run a list of benchmarks and return results."""
    aifpl = AIFPL()
    results = []

    if verbose:
        print(f"\nRunning {len(benchmarks)} benchmarks...")
        print("=" * 80)

    for i, benchmark in enumerate(benchmarks, 1):
        if verbose:
            print(f"[{i}/{len(benchmarks)}] {benchmark.name}...", end=" ", flush=True)

        try:
            result = benchmark.run(aifpl)
            results.append(result)

            if verbose:
                print(f"✓ {result.mean_time*1000:.3f}ms (±{result.std_dev*1000:.3f}ms)")

        except Exception as e:
            if verbose:
                print(f"✗ ERROR: {e}")
            # Continue with other benchmarks

    if verbose:
        print("=" * 80)

    return results


def print_results(results: List[BenchmarkResult]):
    """Print formatted results table."""
    # Group by category
    by_category: Dict[str, List[BenchmarkResult]] = {}
    for result in results:
        if result.category not in by_category:
            by_category[result.category] = []
        by_category[result.category].append(result)

    print("\n" + "=" * 80)
    print("BENCHMARK RESULTS")
    print("=" * 80)

    for category in sorted(by_category.keys()):
        print(f"\n{category.upper()}")
        print("-" * 80)
        print(f"{'Benchmark':<40} {'Mean':<12} {'Median':<12} {'Ops/sec':<12}")
        print("-" * 80)

        for result in by_category[category]:
            name = result.name[:38] + ".." if len(result.name) > 40 else result.name
            mean = f"{result.mean_time*1000:.3f}ms"
            median = f"{result.median_time*1000:.3f}ms"
            ops = f"{result.ops_per_sec:.1f}"
            print(f"{name:<40} {mean:<12} {median:<12} {ops:<12}")

    # Summary statistics
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    total_benchmarks = len(results)
    total_time = sum(r.total_time for r in results)
    avg_time = statistics.mean([r.mean_time for r in results])

    print(f"Total benchmarks: {total_benchmarks}")
    print(f"Total time: {total_time:.2f}s")
    print(f"Average operation time: {avg_time*1000:.3f}ms")

    # Find slowest and fastest
    slowest = max(results, key=lambda r: r.mean_time)
    fastest = min(results, key=lambda r: r.mean_time)

    print(f"\nSlowest: {slowest.name} ({slowest.mean_time*1000:.3f}ms)")
    print(f"Fastest: {fastest.name} ({fastest.mean_time*1000:.3f}ms)")


def save_results(results: List[BenchmarkResult], filename: str):
    """Save results to JSON file."""
    suite = BenchmarkSuite(
        timestamp=datetime.now().isoformat(),
        python_version=sys.version,
        aifpl_version="1.0",  # TODO: Get from package
        results=results
    )

    output_path = Path(__file__).parent / filename
    with open(output_path, 'w') as f:
        json.dump(suite.to_dict(), f, indent=2)

    print(f"\nResults saved to: {output_path}")


def compare_results(current: List[BenchmarkResult], baseline_file: str):
    """Compare current results with baseline."""
    baseline_path = Path(__file__).parent / baseline_file

    if not baseline_path.exists():
        print(f"Baseline file not found: {baseline_path}")
        return

    with open(baseline_path) as f:
        baseline_data = json.load(f)

    baseline_results = {r['name']: r for r in baseline_data['results']}

    print("\n" + "=" * 80)
    print(f"COMPARISON WITH BASELINE: {baseline_file}")
    print("=" * 80)
    print(f"{'Benchmark':<40} {'Current':<12} {'Baseline':<12} {'Change':<12}")
    print("-" * 80)

    improvements = []
    regressions = []

    for result in current:
        if result.name in baseline_results:
            baseline = baseline_results[result.name]
            current_time = result.mean_time * 1000
            baseline_time = baseline['mean_time'] * 1000

            change = ((current_time - baseline_time) / baseline_time) * 100

            name = result.name[:38] + ".." if len(result.name) > 40 else result.name
            current_str = f"{current_time:.3f}ms"
            baseline_str = f"{baseline_time:.3f}ms"

            if abs(change) < 1:
                change_str = "~"
            elif change < 0:
                change_str = f"↑ {abs(change):.1f}%"
                improvements.append((result.name, abs(change)))
            else:
                change_str = f"↓ {change:.1f}%"
                regressions.append((result.name, change))

            print(f"{name:<40} {current_str:<12} {baseline_str:<12} {change_str:<12}")

    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)

    if improvements:
        print(f"\nImprovements ({len(improvements)}):")
        for name, pct in sorted(improvements, key=lambda x: x[1], reverse=True)[:5]:
            print(f"  {name}: {pct:.1f}% faster")

    if regressions:
        print(f"\nRegressions ({len(regressions)}):")
        for name, pct in sorted(regressions, key=lambda x: x[1], reverse=True)[:5]:
            print(f"  {name}: {pct:.1f}% slower")

    if improvements or regressions:
        avg_change = statistics.mean([
            -pct for _, pct in improvements
        ] + [pct for _, pct in regressions])

        if avg_change < 0:
            print(f"\nOverall: {abs(avg_change):.1f}% faster on average")
        else:
            print(f"\nOverall: {avg_change:.1f}% slower on average")


def profile_benchmarks(benchmarks: List[Benchmark]):
    """Run benchmarks with profiling."""
    print("\nRunning with profiler...")

    aifpl = AIFPL()
    profiler = cProfile.Profile()

    profiler.enable()

    # Run each benchmark once
    for benchmark in benchmarks:
        aifpl.evaluate(benchmark.expression)

    profiler.disable()

    # Print stats
    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats('cumulative')
    stats.print_stats(30)

    print("\n" + "=" * 80)
    print("PROFILING RESULTS (Top 30 functions by cumulative time)")
    print("=" * 80)
    print(s.getvalue())


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="AIFPL Performance Benchmark Suite",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--quick',
        action='store_true',
        help='Run quick benchmark subset only'
    )
    parser.add_argument(
        '--profile',
        action='store_true',
        help='Run with profiling enabled'
    )
    parser.add_argument(
        '--compare',
        metavar='FILE',
        help='Compare results with baseline JSON file'
    )
    parser.add_argument(
        '--save',
        metavar='FILE',
        default='benchmark_results.json',
        help='Save results to JSON file (default: benchmark_results.json)'
    )
    parser.add_argument(
        '--quiet',
        action='store_true',
        help='Minimal output'
    )

    args = parser.parse_args()

    # Select benchmark set
    benchmarks = QUICK_BENCHMARKS if args.quick else BENCHMARKS

    if args.profile:
        profile_benchmarks(benchmarks)
        return

    # Run benchmarks
    results = run_benchmarks(benchmarks, verbose=not args.quiet)

    # Print results
    if not args.quiet:
        print_results(results)

    # Save results
    if args.save:
        save_results(results, args.save)

    # Compare with baseline
    if args.compare:
        compare_results(results, args.compare)


if __name__ == '__main__':
    main()
