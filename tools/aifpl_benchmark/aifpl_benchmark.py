#!/usr/bin/env python3
"""
AIFPL Unified Performance Benchmark Suite

Comprehensive benchmarking tool for AIFPL that measures:
- Full pipeline performance (lex + parse + semantic analysis + desugar + compile + execute)
- Compilation performance (lex + parse + semantic analysis + desugar + compile)
- Execution performance (VM bytecode execution only)

Features:
- Profiling support
- JSON export/import for comparison
- Statistical analysis
- Category filtering
- Quick mode for fast iteration

Usage:
    python unified_benchmark.py                    # Run all benchmarks
    python unified_benchmark.py --quick            # Run quick subset
    python unified_benchmark.py --profile          # Run with profiling
    python unified_benchmark.py --compare old.json # Compare with previous results
    python unified_benchmark.py --category lists   # Run only list benchmarks
    python unified_benchmark.py --save results.json # Save results
"""

import argparse
import cProfile
import json
import pstats
import statistics
import sys
import time
from dataclasses import dataclass, asdict
from datetime import datetime
from io import StringIO
from pathlib import Path
from typing import List, Dict, Any

# Add src to path
#sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM


@dataclass
class BenchmarkResult:
    """Result from a single benchmark run."""
    name: str
    category: str
    expression: str
    iterations: int

    # Full pipeline (lex + parse + analyze + desugar + compile + execute)
    full_mean: float
    full_median: float
    full_std_dev: float

    # Compilation only (lex + parse + analyze + desugar + compile)
    compile_mean: float
    compile_median: float
    compile_std_dev: float

    # Execution only (VM bytecode execution)
    exec_mean: float
    exec_median: float
    exec_std_dev: float

    # Derived metrics
    ops_per_sec: float

    # More accurate breakdown - what % of full pipeline time is spent where
    compile_time_ms: float  # Absolute time in ms
    exec_time_ms: float     # Absolute time in ms
    overhead_time_ms: float # Difference between full and (compile + exec)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return asdict(self)


@dataclass
class BenchmarkSuite:
    """Collection of benchmark results."""
    timestamp: str
    python_version: str
    results: List[BenchmarkResult]

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            'timestamp': self.timestamp,
            'python_version': self.python_version,
            'results': [r.to_dict() for r in self.results]
        }


class Benchmark:
    """Individual benchmark test."""

    def __init__(
        self,
        name: str,
        category: str,
        expression: str,
        iterations: int = 200,
        warmup: int = 10
    ):
        self.name = name
        self.category = category
        self.expression = expression
        self.iterations = iterations
        self.warmup = warmup

    def run(self, iterations_multiplier: float = 1.0) -> BenchmarkResult:
        """
        Run the benchmark and return results.

        Args:
            iterations_multiplier: Multiply iteration counts by this factor for better statistics
        """
        # Setup - parse and compile once for execution-only benchmarks
        compiler = AIFPLCompiler()
        vm = AIFPLVM()

        # Get prelude for VM
        aifpl = AIFPL()
        prelude = aifpl._prelude
        constants = AIFPL.CONSTANTS

        # Pre-compile for execution-only benchmark
        code = compiler.compile(self.expression)

        # Apply multiplier to iterations
        actual_iterations = int(self.iterations * iterations_multiplier)
        actual_warmup = int(self.warmup * iterations_multiplier)

        # === WARMUP ===
        for _ in range(actual_warmup):
            aifpl.evaluate(self.expression)

        # === MEASURE FULL PIPELINE ===
        full_times = []
        for _ in range(actual_iterations):
            start = time.perf_counter()
            aifpl.evaluate(self.expression)
            elapsed = time.perf_counter() - start
            full_times.append(elapsed)

        # === MEASURE COMPILATION ONLY ===
        compile_times = []
        for _ in range(actual_iterations):
            start = time.perf_counter()
            compiler.compile(self.expression)
            elapsed = time.perf_counter() - start
            compile_times.append(elapsed)

        # === MEASURE EXECUTION ONLY ===
        exec_times = []
        for _ in range(actual_iterations):
            start = time.perf_counter()
            vm.execute(code, constants, prelude)
            elapsed = time.perf_counter() - start
            exec_times.append(elapsed)

        # Calculate statistics
        full_mean = statistics.mean(full_times)
        full_median = statistics.median(full_times)
        full_std = statistics.stdev(full_times) if len(full_times) > 1 else 0.0

        compile_mean = statistics.mean(compile_times)
        compile_median = statistics.median(compile_times)
        compile_std = statistics.stdev(compile_times) if len(compile_times) > 1 else 0.0

        exec_mean = statistics.mean(exec_times)
        exec_median = statistics.median(exec_times)
        exec_std = statistics.stdev(exec_times) if len(exec_times) > 1 else 0.0

        ops_per_sec = 1.0 / full_mean if full_mean > 0 else 0.0

        # Calculate absolute times in milliseconds
        compile_time_ms = compile_mean * 1000
        exec_time_ms = exec_mean * 1000
        full_time_ms = full_mean * 1000

        # Calculate overhead (the difference between full pipeline and measured components)
        # This represents AIFPL wrapper overhead, context setup, etc.
        overhead_time_ms = full_time_ms - (compile_time_ms + exec_time_ms)

        return BenchmarkResult(
            name=self.name,
            category=self.category,
            expression=self.expression,
            iterations=actual_iterations,
            full_mean=full_mean,
            full_median=full_median,
            full_std_dev=full_std,
            compile_mean=compile_mean,
            compile_median=compile_median,
            compile_std_dev=compile_std,
            exec_mean=exec_mean,
            exec_median=exec_median,
            exec_std_dev=exec_std,
            ops_per_sec=ops_per_sec,
            compile_time_ms=compile_time_ms,
            exec_time_ms=exec_time_ms,
            overhead_time_ms=overhead_time_ms
        )


# Define comprehensive benchmark suite (92 benchmarks)
BENCHMARKS = [
    # === ARITHMETIC ===
    Benchmark("Simple Addition", "arithmetic", "(integer+ 1 2 3 4 5)", iterations=5000),
    Benchmark("Nested Arithmetic", "arithmetic", "(integer* (integer+ 1 2 3) (integer- 10 5) (integer/ 20 4))", iterations=5000),
    Benchmark("Deep Nesting", "arithmetic", "(integer+ (integer* (integer- (integer/ 100 5) 3) 2) (integer- (integer* 7 3) (integer/ 42 6)))", iterations=5000),
    Benchmark("Many Operations", "arithmetic", "(integer+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)", iterations=5000),

    # === COMPARISONS ===
    Benchmark("Simple Comparison", "comparisons", "(integer<? 5 10)", iterations=5000),
    Benchmark("Chained Comparisons", "comparisons", "(and (integer<? 1 5) (integer>? 10 3) (integer=? 7 7) (integer!=? 4 5))", iterations=5000),
    Benchmark("List Equality", "comparisons", "(list=? (list 1 2 3 4 5) (list 1 2 3 4 5))", iterations=3000),
    Benchmark(
        "Deep List Equality",
        "comparisons",
        "(list=? (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))",
        iterations=2000
    ),

    # === BOOLEAN LOGIC ===
    Benchmark("AND Operations", "boolean", "(and #t #t #t #t #t)", iterations=5000),
    Benchmark("OR Operations", "boolean", "(or #f #f #f #f #t)", iterations=5000),
    Benchmark("NOT Operations", "boolean", "(boolean-not (boolean-not (boolean-not #t)))", iterations=5000),
    Benchmark("Complex Boolean", "boolean", "(and (or #t #f) (boolean-not #f) (or (and #t #t) #f))", iterations=5000),

    # === CONDITIONALS ===
    Benchmark("Simple If", "conditionals", "(if (integer>? 5 3) 10 20)", iterations=5000),
    Benchmark("Nested If", "conditionals", "(if (integer>? 5 3) (if (integer<? 2 4) 10 20) 30)", iterations=5000),
    Benchmark("If Chain", "conditionals", "(if #f 1 (if #f 2 (if #f 3 (if #f 4 5))))", iterations=5000),

    # === LAMBDA FUNCTIONS ===
    Benchmark("Simple Lambda", "lambda", "((lambda (x) (integer* x x)) 5)", iterations=3000),
    Benchmark("Lambda Multiple Args", "lambda", "((lambda (x y z) (integer+ (integer* x y) z)) 3 4 5)", iterations=3000),
    Benchmark("Nested Lambda", "lambda", "((lambda (x) ((lambda (y) (integer+ x y)) 10)) 5)", iterations=2000),
    Benchmark("Lambda Returning Lambda", "lambda", "(((lambda (x) (lambda (y) (integer+ x y))) 5) 10)", iterations=2000),

    # === CLOSURES ===
    Benchmark("Simple Closure", "closures", "(let ((x 10)) ((lambda (y) (integer+ x y)) 5))", iterations=3000),
    Benchmark("Multiple Captures", "closures", "(let ((a 1) (b 2) (c 3)) ((lambda (x) (integer+ a b c x)) 4))", iterations=2000),
    Benchmark("Nested Closures", "closures", "(let ((x 10)) (let ((f (lambda (y) (integer+ x y)))) (f 5)))", iterations=2000),
    Benchmark(
        "Closure Factory",
        "closures",
        "(let ((make-adder (lambda (n) (lambda (x) (integer+ x n))))) ((make-adder 10) 5))",
        iterations=2000
    ),

    # === LET BINDINGS ===
    Benchmark("Simple Let", "let", "(let ((x 5) (y 10)) (integer+ x y))", iterations=5000),
    Benchmark(
        "Let Many Bindings",
        "let",
        "(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) (integer+ a b c d e f g h i j))",
        iterations=3000
    ),
    Benchmark("Nested Let", "let", "(let ((x 5)) (let ((y 10)) (let ((z 15)) (integer+ x y z))))", iterations=3000),
    Benchmark("Let with Computation", "let", "(let ((x (integer* 5 5)) (y (integer+ 10 10)) (z (integer- 30 5))) (integer* x y z))", iterations=3000),

    # === RECURSION ===
    Benchmark(
        "Factorial (5)",
        "recursion",
        "(letrec ((factorial (lambda (n) (if (integer<=? n 1) 1 (integer* n (factorial (integer- n 1))))))) (factorial 5))",
        iterations=1000
    ),
    Benchmark(
        "Factorial (10)",
        "recursion",
        "(letrec ((factorial (lambda (n) (if (integer<=? n 1) 1 (integer* n (factorial (integer- n 1))))))) (factorial 10))",
        iterations=500
    ),
    Benchmark(
        "Fibonacci (10)",
        "recursion",
        "(letrec ((fib (lambda (n) (if (integer<=? n 1) n (integer+ (fib (integer- n 1)) (fib (integer- n 2))))))) (fib 10))",
        iterations=100
    ),
    Benchmark(
        "Tail Recursive Sum (50)",
        "recursion",
        "(letrec ((sum-tail (lambda (n acc) (if (integer<=? n 0) acc (sum-tail (integer- n 1) (integer+ acc n)))))) (sum-tail 50 0))",
        iterations=1000
    ),
    Benchmark(
        "Tail Recursive Sum (100)",
        "recursion",
        "(letrec ((sum-tail (lambda (n acc) (if (integer<=? n 0) acc (sum-tail (integer- n 1) (integer+ acc n)))))) (sum-tail 100 0))",
        iterations=500
    ),

    # === LIST OPERATIONS ===
    Benchmark("List Creation (5)", "lists", "(list 1 2 3 4 5)", iterations=5000),
    Benchmark("List Creation (10)", "lists", "(list 1 2 3 4 5 6 7 8 9 10)", iterations=5000),
    Benchmark("List Creation (20)", "lists", "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)", iterations=3000),
    Benchmark("List Concatenate", "lists", "(list-concat (list 1 2 3) (list 4 5 6))", iterations=3000),
    Benchmark("List Reverse (10)", "lists", "(list-reverse (list 1 2 3 4 5 6 7 8 9 10))", iterations=3000),
    Benchmark("List Reverse (20)", "lists", "(list-reverse (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))", iterations=2000),
    Benchmark("List First/Rest", "lists", "(list-first (list-rest (list 1 2 3 4 5)))", iterations=5000),
    Benchmark("List Member", "lists", "(list-member? (list 1 2 3 4 5 6 7 8 9 10) 5)", iterations=3000),
    Benchmark("List Position", "lists", "(list-index (list 1 2 3 4 5 6 7 8 9 10) 7)", iterations=3000),

    # === HIGHER-ORDER FUNCTIONS ===
    Benchmark("Map (10)", "higher-order", "(list-map (lambda (x) (integer* x x)) (list 1 2 3 4 5 6 7 8 9 10))", iterations=1000),
    Benchmark("Map (50)", "higher-order", "(list-map (lambda (x) (integer* x x)) (range 1 51))", iterations=200),
    Benchmark("Map (100)", "higher-order", "(list-map (lambda (x) (integer* x x)) (range 1 101))", iterations=100),
    Benchmark("Filter (50)", "higher-order", "(list-filter (lambda (x) (integer>? x 25)) (range 1 51))", iterations=200),
    Benchmark("Filter (100)", "higher-order", "(list-filter (lambda (x) (integer>? x 50)) (range 1 101))", iterations=100),
    Benchmark("Fold (50)", "higher-order", "(list-fold integer+ 0 (range 1 51))", iterations=200),
    Benchmark("Fold (100)", "higher-order", "(list-fold integer+ 0 (range 1 101))", iterations=100),
    Benchmark(
        "Map+Filter Pipeline", "higher-order", "(list-filter (lambda (x) (integer>? x 50)) (list-map (lambda (x) (integer* x 2)) (range 1 51)))",
        iterations=100
    ),
    Benchmark("Map+Fold Pipeline", "higher-order", "(list-fold integer+ 0 (list-map (lambda (x) (integer* x x)) (range 1 51)))", iterations=100),
    Benchmark("Find", "higher-order", "(list-find (lambda (x) (integer>? x 50)) (range 1 101))", iterations=500),
    Benchmark("Any?", "higher-order", "(list-any? (lambda (x) (integer>? x 90)) (range 1 101))", iterations=500),
    Benchmark("All?", "higher-order", "(list-all? (lambda (x) (integer>? x 0)) (range 1 101))", iterations=500),

    # === STRING OPERATIONS ===
    Benchmark("String Concatenate", "strings", '(string-concat "hello" " " "world")', iterations=5000),
    Benchmark("String Concatenate Many", "strings", '(string-concat "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")', iterations=3000),
    Benchmark("String Upcase", "strings", '(string-upcase "hello world")', iterations=5000),
    Benchmark("String Downcase", "strings", '(string-downcase "HELLO WORLD")', iterations=5000),
    Benchmark("String Manipulation", "strings", '(string-upcase (string-concat "hello" " " "world"))', iterations=3000),
    Benchmark("String Index", "strings", '(string-index "hello world" "wor")', iterations=5000),
    Benchmark("String Replace", "strings", '(string-replace "hello world" "world" "universe")', iterations=3000),
    Benchmark("String Slice", "strings", '(string-slice "hello world" 0 5)', iterations=5000),

    # === DICT OPERATIONS ===
    Benchmark("Alist Creation (5)", "dicts", '(dict (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4) (list "e" 5))', iterations=3000),
    Benchmark("Alist Creation (10)", "dicts", '(dict (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4) (list "e" 5) (list "f" 6) (list "g" 7) (list "h" 8) (list "i" 9) (list "j" 10))', iterations=2000),
    Benchmark("Alist Get", "dicts", '(dict-get (dict (list "name" "Alice") (list "age" 30) (list "city" "NYC")) "age")', iterations=3000),
    Benchmark("Alist Set", "dicts", '(dict-set (dict (list "name" "Alice") (list "age" 30)) "age" 31)', iterations=2000),
    Benchmark("Alist Has", "dicts", '(dict-has? (dict (list "name" "Alice") (list "age" 30)) "name")', iterations=3000),
    Benchmark("Alist Keys", "dicts", '(dict-keys (dict (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4) (list "e" 5)))', iterations=2000),
    Benchmark("Alist Merge", "dicts", '(dict-merge (dict (list "a" 1) (list "b" 2)) (dict (list "c" 3) (list "d" 4)))', iterations=2000),

    # === PATTERN MATCHING ===
    Benchmark("Pattern Match Literal", "match", "(match 42 (42 \"found\") (_ \"not found\"))", iterations=3000),
    Benchmark("Pattern Match Variable", "match", "(match 42 (x (integer* x 2)))", iterations=3000),
    Benchmark("Pattern Match Type", "match", "(match 42 ((integer? i) (integer* i 2)) (_ 0))", iterations=2000),
    Benchmark("Pattern Match List", "match", "(match (list 1 2 3) ((a b c) b))", iterations=2000),
    Benchmark("Pattern Match Nested", "match", "(match (list (list 1 2) (list 3 4)) (((a b) (c d)) (integer+ a b c d)))", iterations=1000),
    Benchmark("Pattern Match Cons", "match", "(match (list 1 2 3 4 5) ((head . tail) head))", iterations=2000),
    Benchmark("Pattern Match Multiple", "match", "(match 5 (1 \"one\") (2 \"two\") (3 \"three\") (4 \"four\") (5 \"five\") (_ \"other\"))", iterations=2000),

    # === MATH FUNCTIONS ===
    Benchmark("Sqrt", "math", "(float-sqrt 16.0)", iterations=5000),
    Benchmark("Sqrt Negative", "math", "(complex-sqrt -4+0j)", iterations=3000),
    Benchmark("Abs", "math", "(integer-abs -42)", iterations=5000),
    Benchmark("Min/Max", "math", "(integer+ (integer-min 1 2 3 4 5) (integer-max 1 2 3 4 5))", iterations=5000),
    Benchmark("Pow", "math", "(float-expn 2.0 10.0)", iterations=5000),
    Benchmark("Trigonometry", "math", "(float+ (float-sin 0.5) (float-cos 0.5) (float-tan 0.5))", iterations=3000),
    Benchmark("Logarithms", "math", "(float+ (float-log 10.0) (float-log10 100.0))", iterations=3000),
    Benchmark("Complex Numbers", "math", "(float+ (complex-real (integer->complex 3 4)) (complex-imag (integer->complex 3 4)))", iterations=3000),
    Benchmark("Rounding", "math", "(integer+ (float->integer (float-round 3.7)) (float->integer (float-floor 3.7)) (float->integer (float-ceil 3.2)))", iterations=5000),

    # === TYPE PREDICATES ===
    Benchmark("Type Checks", "types", "(and (integer? 42) (string? \"hi\") (boolean? #t) (list? (list 1 2)))", iterations=5000),
    Benchmark("Integer/Float/Complex", "types", "(and (integer? 42) (float? 3.14) (complex? (integer->complex 1 2)))", iterations=3000),

    # === REDICTIC WORKLOADS ===
    Benchmark("Data Processing", "redictic",
              """(let ((data (range 1 21)))
                   (list-fold integer+ 0 (list-map (lambda (x) (integer* x x)) (list-filter (lambda (x) (integer>? x 10)) data))))""", iterations=200),
    Benchmark("Nested Data Structure", "redictic",
              """(let ((users (list
                              (dict (list "name" "Alice") (list "age" 30))
                              (dict (list "name" "Bob") (list "age" 25))
                              (dict (list "name" "Charlie") (list "age" 35)))))
                   (list-map (lambda (user) (dict-get user "age")) users))""", iterations=500),
    Benchmark("Recursive List Processing", "redictic",
              """(letrec ((sum-list (lambda (lst)
                                     (if (list-null? lst)
                                         0
                                         (integer+ (list-first lst) (sum-list (list-rest lst)))))))
                   (sum-list (list 1 2 3 4 5 6 7 8 9 10)))""", iterations=500),
    Benchmark("Pattern Match Pipeline", "redictic",
              """(list-map (lambda (x)
                               (match x
                                 ((integer? i) (integer* i 2))
                                 ((string? s) (string-length s))
                                 (_ 0)))
                             (list 1 2 "hello" 3 "world" 4))""", iterations=500),
    Benchmark("Closure-based Counter", "redictic",
              """(let ((make-counter (lambda (start)
                                      (lambda (inc) (integer+ start inc)))))
                   (let ((counter (make-counter 10)))
                     (integer+ (counter 1) (counter 2) (counter 3))))""", iterations=1000),
]

# Quick benchmark subset for fast iteration
QUICK_BENCHMARKS = [b for b in BENCHMARKS if b.category in ["arithmetic", "functions", "lists", "let"]]


def run_benchmarks(
    benchmarks: List[Benchmark],
    verbose: bool = True,
    iterations_multiplier: float = 1.0
) -> List[BenchmarkResult]:
    """
    Run a list of benchmarks and return results.

    Args:
        benchmarks: List of benchmarks to run
        verbose: Print progress
        iterations_multiplier: Multiply all iteration counts by this factor
    """
    results = []

    if verbose:
        print(f"\nRunning {len(benchmarks)} benchmarks...")
        print("=" * 120)

    for i, benchmark in enumerate(benchmarks, 1):
        if verbose:
            print(f"[{i}/{len(benchmarks)}] {benchmark.name}...", end=" ", flush=True)

        try:
            result = benchmark.run(iterations_multiplier=iterations_multiplier)
            results.append(result)

            if verbose:
                print(f"✓ {result.full_mean*1000:.3f}ms (compile: {result.compile_time_ms:.3f}ms, exec: {result.exec_time_ms:.3f}ms, overhead: {result.overhead_time_ms:.3f}ms)")

        except Exception as e:
            if verbose:
                print(f"✗ ERROR: {e}")

    if verbose:
        print("=" * 120)

    return results


def print_results(results: List[BenchmarkResult], show_breakdown: bool = False):
    """Print formatted results table."""
    # Group by category
    by_category: Dict[str, List[BenchmarkResult]] = {}
    for result in results:
        if result.category not in by_category:
            by_category[result.category] = []
        by_category[result.category].append(result)

    print("\n" + "=" * 120)
    print("BENCHMARK RESULTS")
    print("=" * 120)

    for category in sorted(by_category.keys()):
        print(f"\n{category.upper()}")
        print("-" * 120)

        if show_breakdown:
            print(f"{'Benchmark':<35} {'Full':<12} {'Compile':<12} {'Exec':<12} {'Overhead':<12} {'Ops/sec':<12}")
        else:
            print(f"{'Benchmark':<40} {'Mean':<12} {'Median':<12} {'Ops/sec':<12}")

        print("-" * 120)

        for result in by_category[category]:
            name = result.name[:33] + ".." if len(result.name) > 35 else result.name

            if show_breakdown:
                full = f"{result.full_mean*1000:.3f}ms"
                compile = f"{result.compile_time_ms:.3f}ms"
                exec_time = f"{result.exec_time_ms:.3f}ms"
                overhead = f"{result.overhead_time_ms:.3f}ms"
                ops = f"{result.ops_per_sec:.1f}"
                print(f"{name:<35} {full:<12} {compile:<12} {exec_time:<12} {overhead:<12} {ops:<12}")
            else:
                name = result.name[:38] + ".." if len(result.name) > 40 else result.name
                mean = f"{result.full_mean*1000:.3f}ms"
                median = f"{result.full_median*1000:.3f}ms"
                ops = f"{result.ops_per_sec:.1f}"
                print(f"{name:<40} {mean:<12} {median:<12} {ops:<12}")

    # Summary statistics
    print("\n" + "=" * 120)
    print("SUMMARY")
    print("=" * 120)

    total_benchmarks = len(results)
    avg_full = statistics.mean([r.full_mean for r in results])
    avg_compile = statistics.mean([r.compile_time_ms for r in results])
    avg_exec = statistics.mean([r.exec_time_ms for r in results])
    avg_overhead = statistics.mean([r.overhead_time_ms for r in results])

    print(f"Total benchmarks:        {total_benchmarks}")
    print(f"Average full time:       {avg_full*1000:.3f}ms")
    print(f"  - Compile time:        {avg_compile:.3f}ms ({avg_compile/(avg_full*1000)*100:.1f}%)")
    print(f"  - Exec time:           {avg_exec:.3f}ms ({avg_exec/(avg_full*1000)*100:.1f}%)")
    print(f"  - Overhead:            {avg_overhead:.3f}ms ({avg_overhead/(avg_full*1000)*100:.1f}%)")
    print(f"  - Accounted for:       {(avg_compile + avg_exec + avg_overhead):.3f}ms ({(avg_compile + avg_exec + avg_overhead)/(avg_full*1000)*100:.1f}%)")

    # Find slowest and fastest
    slowest = max(results, key=lambda r: r.full_mean)
    fastest = min(results, key=lambda r: r.full_mean)

    print(f"\nSlowest: {slowest.name} ({slowest.full_mean*1000:.3f}ms)")
    print(f"Fastest: {fastest.name} ({fastest.full_mean*1000:.3f}ms)")

    # Analyze overhead
    high_overhead = [r for r in results if r.overhead_time_ms < 0]
    if high_overhead:
        print(f"\nNOTE: {len(high_overhead)} benchmarks show negative overhead (measurement variance)")
        print("This indicates compile+exec times exceed full pipeline time, likely due to:")
        print("  - Timing measurement variance")
        print("  - CPU cache effects between separate measurement runs")
        print("  - Different code paths in separate vs. integrated execution")


def save_results(results: List[BenchmarkResult], filename: str):
    """Save results to JSON file."""
    suite = BenchmarkSuite(
        timestamp=datetime.now().isoformat(),
        python_version=sys.version,
        results=results
    )

    output_path = Path(__file__).parent / filename
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(suite.to_dict(), f, indent=2)

    print(f"\nResults saved to: {output_path}")


def compare_results(current: List[BenchmarkResult], baseline_file: str):
    """Compare current results with baseline."""
    baseline_path = Path(__file__).parent / baseline_file

    if not baseline_path.exists():
        print(f"Baseline file not found: {baseline_path}")
        return

    with open(baseline_path, 'r', encoding='utf-8') as f:
        baseline_data = json.load(f)

    baseline_results = {r['name']: r for r in baseline_data['results']}

    print("\n" + "=" * 100)
    print(f"COMPARISON WITH BASELINE: {baseline_file}")
    print("=" * 100)
    print(f"{'Benchmark':<40} {'Current':<12} {'Baseline':<12} {'Change':<12}")
    print("-" * 100)

    improvements = []
    regressions = []

    for result in current:
        if result.name in baseline_results:
            baseline = baseline_results[result.name]
            current_time = result.full_mean * 1000
            baseline_time = baseline['full_mean'] * 1000

            speedup = baseline_time / current_time if current_time > 0 else 0
            pct_change = ((baseline_time - current_time) / baseline_time) * 100

            name = result.name[:38] + ".." if len(result.name) > 40 else result.name
            current_str = f"{current_time:.3f}ms"
            baseline_str = f"{baseline_time:.3f}ms"

            if abs(speedup - 1.0) < 0.01:
                change_str = "~"
            elif speedup > 1.0:
                change_str = f"{speedup:.2f}x ✓"
                improvements.append((result.name, speedup, pct_change))
            else:
                change_str = f"{speedup:.2f}x"
                regressions.append((result.name, speedup, pct_change))

            print(f"{name:<40} {current_str:<12} {baseline_str:<12} {change_str:<12}")

    print("\n" + "=" * 100)
    print("SUMMARY")
    print("=" * 100)

    if improvements:
        print(f"\nImprovements ({len(improvements)}):")
        for name, speedup, pct in sorted(improvements, key=lambda x: x[1], reverse=True)[:5]:
            print(f"  {name}: {speedup:.2f}x faster ({pct:.1f}% time saved)")

    if regressions:
        print(f"\nRegressions ({len(regressions)}):")
        for name, speedup, pct in sorted(regressions, key=lambda x: x[1])[:5]:
            print(f"  {name}: {speedup:.2f}x ({abs(pct):.1f}% time increase)")


def profile_benchmarks(benchmarks: List[Benchmark]):
    """Run benchmarks with profiling."""
    print("\nRunning with profiler...")

    aifpl = AIFPL()
    profiler = cProfile.Profile()

    profiler.enable()

    # Run each benchmark a few times
    for benchmark in benchmarks:
        for _ in range(benchmark.iterations // 10):
            aifpl.evaluate(benchmark.expression)

    profiler.disable()

    # Print stats
    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats('cumulative')
    stats.print_stats(80)

    print("\n" + "=" * 100)
    print("PROFILING RESULTS (Top 80 functions)")
    print("=" * 100)
    print(s.getvalue())


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="AIFPL Unified Performance Benchmark Suite",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('--quick', action='store_true', help='Run quick benchmark subset only')
    parser.add_argument('--category', metavar='CAT', help='Run only benchmarks in this category')
    parser.add_argument('--profile', action='store_true', help='Run with profiling enabled')
    parser.add_argument('--compare', metavar='FILE', help='Compare results with baseline JSON file')
    parser.add_argument('--save', metavar='FILE', default='benchmark_results.json', help='Save results to JSON file')
    parser.add_argument('--no-save', action='store_true', help='Do not save results')
    parser.add_argument('--breakdown', action='store_true', help='Show compile/exec breakdown in results')
    parser.add_argument('--quiet', action='store_true', help='Minimal output')
    parser.add_argument(
        '--iterations-multiplier',
        type=float,
        default=1.0,
        metavar='N',
        help='Multiply all iteration counts by N for better statistics (default: 1.0)'
    )

    args = parser.parse_args()

    # Select benchmark set
    benchmarks = QUICK_BENCHMARKS if args.quick else BENCHMARKS

    if args.category:
        benchmarks = [b for b in BENCHMARKS if b.category.lower() == args.category.lower()]
        if not benchmarks:
            cats = sorted(set(b.category for b in BENCHMARKS))
            print(f"No benchmarks found for category: {args.category}")
            print(f"Available categories: {', '.join(cats)}")
            return

    if args.profile:
        profile_benchmarks(benchmarks)
        return

    # Run benchmarks
    results = run_benchmarks(benchmarks, verbose=not args.quiet, iterations_multiplier=args.iterations_multiplier)

    # Print results
    if not args.quiet:
        print_results(results, show_breakdown=args.breakdown)

    # Save results
    if not args.no_save and args.save:
        save_results(results, args.save)

    # Compare with baseline
    if args.compare:
        compare_results(results, args.compare)


if __name__ == '__main__':
    main()
