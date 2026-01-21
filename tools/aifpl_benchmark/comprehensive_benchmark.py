#!/usr/bin/env python3
"""
Comprehensive performance benchmark comparing bytecode vs interpreter.

This benchmark suite tests a wide variety of AIFPL operations including
newly implemented features like pattern matching, complex numbers, and
all the recent bug fixes.
"""

import argparse
import statistics
import sys
import time
from pathlib import Path
from typing import List, Tuple, Dict
from dataclasses import dataclass

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_evaluator import AIFPLEvaluator


@dataclass
class BenchmarkResult:
    """Results from a single benchmark."""
    name: str
    category: str
    interpreter_mean: float
    interpreter_median: float
    bytecode_full_mean: float
    bytecode_full_median: float
    bytecode_mean: float
    bytecode_median: float
    speedup_full: float
    speedup_exec: float
    iterations: int


class BenchmarkCase:
    """Single benchmark case."""

    def __init__(self, name: str, category: str, expression: str, iterations: int = 100):
        self.name = name
        self.category = category
        self.expression = expression
        self.iterations = iterations


# Comprehensive benchmark suite
BENCHMARKS = [
    # === ARITHMETIC ===
    BenchmarkCase("Simple Addition", "Arithmetic", 
                  "(+ 1 2 3 4 5)", 5000),
    BenchmarkCase("Nested Arithmetic", "Arithmetic",
                  "(* (+ 1 2 3) (- 10 5) (/ 20 4))", 5000),
    BenchmarkCase("Deep Nesting", "Arithmetic",
                  "(+ (* (- (/ 100 5) 3) 2) (- (* 7 3) (/ 42 6)))", 5000),
    BenchmarkCase("Many Operations", "Arithmetic",
                  "(+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)", 5000),

    # === COMPARISONS ===
    BenchmarkCase("Simple Comparison", "Comparisons",
                  "(< 5 10)", 5000),
    BenchmarkCase("Chained Comparisons", "Comparisons",
                  "(and (< 1 5) (> 10 3) (= 7 7) (!= 4 5))", 5000),
    BenchmarkCase("List Equality", "Comparisons",
                  "(= (list 1 2 3 4 5) (list 1 2 3 4 5))", 3000),
    BenchmarkCase("Deep List Equality", "Comparisons",
                  "(= (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))", 2000),

    # === BOOLEAN LOGIC ===
    BenchmarkCase("AND Operations", "Boolean",
                  "(and #t #t #t #t #t)", 5000),
    BenchmarkCase("OR Operations", "Boolean",
                  "(or #f #f #f #f #t)", 5000),
    BenchmarkCase("NOT Operations", "Boolean",
                  "(not (not (not #t)))", 5000),
    BenchmarkCase("Complex Boolean", "Boolean",
                  "(and (or #t #f) (not #f) (or (and #t #t) #f))", 5000),

    # === CONDITIONALS ===
    BenchmarkCase("Simple If", "Conditionals",
                  "(if (> 5 3) 10 20)", 5000),
    BenchmarkCase("Nested If", "Conditionals",
                  "(if (> 5 3) (if (< 2 4) 10 20) 30)", 5000),
    BenchmarkCase("If Chain", "Conditionals",
                  "(if #f 1 (if #f 2 (if #f 3 (if #f 4 5))))", 5000),

    # === LAMBDA FUNCTIONS ===
    BenchmarkCase("Simple Lambda", "Lambda",
                  "((lambda (x) (* x x)) 5)", 3000),
    BenchmarkCase("Lambda Multiple Args", "Lambda",
                  "((lambda (x y z) (+ (* x y) z)) 3 4 5)", 3000),
    BenchmarkCase("Nested Lambda", "Lambda",
                  "((lambda (x) ((lambda (y) (+ x y)) 10)) 5)", 2000),
    BenchmarkCase("Lambda Returning Lambda", "Lambda",
                  "(((lambda (x) (lambda (y) (+ x y))) 5) 10)", 2000),

    # === CLOSURES ===
    BenchmarkCase("Simple Closure", "Closures",
                  "(let ((x 10)) ((lambda (y) (+ x y)) 5))", 3000),
    BenchmarkCase("Multiple Captures", "Closures",
                  "(let ((a 1) (b 2) (c 3)) ((lambda (x) (+ a b c x)) 4))", 2000),
    BenchmarkCase("Nested Closures", "Closures",
                  "(let ((x 10)) (let ((f (lambda (y) (+ x y)))) (f 5)))", 2000),
    BenchmarkCase("Closure Factory", "Closures",
                  "(let ((make-adder (lambda (n) (lambda (x) (+ x n))))) ((make-adder 10) 5))", 2000),

    # === LET BINDINGS ===
    BenchmarkCase("Simple Let", "Let Bindings",
                  "(let ((x 5) (y 10)) (+ x y))", 5000),
    BenchmarkCase("Let Many Bindings", "Let Bindings",
                  "(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) (+ a b c d e f g h i j))", 3000),
    BenchmarkCase("Nested Let", "Let Bindings",
                  "(let ((x 5)) (let ((y 10)) (let ((z 15)) (+ x y z))))", 3000),
    BenchmarkCase("Let with Computation", "Let Bindings",
                  "(let ((x (* 5 5)) (y (+ 10 10)) (z (- 30 5))) (* x y z))", 3000),

    # === RECURSION ===
    BenchmarkCase("Factorial (5)", "Recursion",
                  "(let ((factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))) (factorial 5))", 1000),
    BenchmarkCase("Factorial (10)", "Recursion",
                  "(let ((factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))) (factorial 10))", 500),
    BenchmarkCase("Fibonacci (10)", "Recursion",
                  "(let ((fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10))", 100),
    BenchmarkCase("Tail Recursive Sum (50)", "Recursion",
                  "(let ((sum-tail (lambda (n acc) (if (<= n 0) acc (sum-tail (- n 1) (+ acc n)))))) (sum-tail 50 0))", 1000),
    BenchmarkCase("Tail Recursive Sum (100)", "Recursion",
                  "(let ((sum-tail (lambda (n acc) (if (<= n 0) acc (sum-tail (- n 1) (+ acc n)))))) (sum-tail 100 0))", 500),

    # === LIST OPERATIONS ===
    BenchmarkCase("List Creation (5)", "Lists",
                  "(list 1 2 3 4 5)", 5000),
    BenchmarkCase("List Creation (10)", "Lists",
                  "(list 1 2 3 4 5 6 7 8 9 10)", 5000),
    BenchmarkCase("List Creation (20)", "Lists",
                  "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)", 3000),
    BenchmarkCase("List Append", "Lists",
                  "(append (list 1 2 3) (list 4 5 6))", 3000),
    BenchmarkCase("List Reverse (10)", "Lists",
                  "(reverse (list 1 2 3 4 5 6 7 8 9 10))", 3000),
    BenchmarkCase("List Reverse (20)", "Lists",
                  "(reverse (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))", 2000),
    BenchmarkCase("List First/Rest", "Lists",
                  "(first (rest (list 1 2 3 4 5)))", 5000),
    BenchmarkCase("List Member", "Lists",
                  "(member? 5 (list 1 2 3 4 5 6 7 8 9 10))", 3000),
    BenchmarkCase("List Take/Drop", "Lists",
                  "(take 5 (drop 3 (list 1 2 3 4 5 6 7 8 9 10)))", 3000),
    BenchmarkCase("List Position", "Lists",
                  "(position 7 (list 1 2 3 4 5 6 7 8 9 10))", 3000),

    # === HIGHER-ORDER FUNCTIONS ===
    BenchmarkCase("Map (10)", "Higher-Order",
                  "(map (lambda (x) (* x x)) (list 1 2 3 4 5 6 7 8 9 10))", 1000),
    BenchmarkCase("Map (50)", "Higher-Order",
                  "(map (lambda (x) (* x x)) (range 1 51))", 200),
    BenchmarkCase("Map (100)", "Higher-Order",
                  "(map (lambda (x) (* x x)) (range 1 101))", 100),
    BenchmarkCase("Filter (50)", "Higher-Order",
                  "(filter (lambda (x) (> x 25)) (range 1 51))", 200),
    BenchmarkCase("Filter (100)", "Higher-Order",
                  "(filter (lambda (x) (> x 50)) (range 1 101))", 100),
    BenchmarkCase("Fold (50)", "Higher-Order",
                  "(fold + 0 (range 1 51))", 200),
    BenchmarkCase("Fold (100)", "Higher-Order",
                  "(fold + 0 (range 1 101))", 100),
    BenchmarkCase("Map+Filter Pipeline", "Higher-Order",
                  "(filter (lambda (x) (> x 50)) (map (lambda (x) (* x 2)) (range 1 51)))", 100),
    BenchmarkCase("Map+Fold Pipeline", "Higher-Order",
                  "(fold + 0 (map (lambda (x) (* x x)) (range 1 51)))", 100),
    BenchmarkCase("Find", "Higher-Order",
                  "(find (lambda (x) (> x 50)) (range 1 101))", 500),
    BenchmarkCase("Any?", "Higher-Order",
                  "(any? (lambda (x) (> x 90)) (range 1 101))", 500),
    BenchmarkCase("All?", "Higher-Order",
                  "(all? (lambda (x) (> x 0)) (range 1 101))", 500),

    # === STRING OPERATIONS ===
    BenchmarkCase("String Append", "Strings",
                  '(string-append "hello" " " "world")', 5000),
    BenchmarkCase("String Append Many", "Strings",
                  '(string-append "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")', 3000),
    BenchmarkCase("String Upcase", "Strings",
                  '(string-upcase "hello world")', 5000),
    BenchmarkCase("String Downcase", "Strings",
                  '(string-downcase "HELLO WORLD")', 5000),
    BenchmarkCase("String Manipulation", "Strings",
                  '(string-upcase (string-append "hello" " " "world"))', 3000),
    BenchmarkCase("String Contains", "Strings",
                  '(string-contains? "hello world" "wor")', 5000),
    BenchmarkCase("String Replace", "Strings",
                  '(string-replace "hello world" "world" "universe")', 3000),
    BenchmarkCase("Substring", "Strings",
                  '(substring "hello world" 0 5)', 5000),

    # === ALIST OPERATIONS ===
    BenchmarkCase("Alist Creation (5)", "Alists",
                  '(alist ("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5))', 3000),
    BenchmarkCase("Alist Creation (10)", "Alists",
                  '(alist ("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5) ("f" 6) ("g" 7) ("h" 8) ("i" 9) ("j" 10))', 2000),
    BenchmarkCase("Alist Get", "Alists",
                  '(alist-get (alist ("name" "Alice") ("age" 30) ("city" "NYC")) "age")', 3000),
    BenchmarkCase("Alist Set", "Alists",
                  '(alist-set (alist ("name" "Alice") ("age" 30)) "age" 31)', 2000),
    BenchmarkCase("Alist Has", "Alists",
                  '(alist-has? (alist ("name" "Alice") ("age" 30)) "name")', 3000),
    BenchmarkCase("Alist Keys", "Alists",
                  '(alist-keys (alist ("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))', 2000),
    BenchmarkCase("Alist Merge", "Alists",
                  '(alist-merge (alist ("a" 1) ("b" 2)) (alist ("c" 3) ("d" 4)))', 2000),

    # === PATTERN MATCHING ===
    BenchmarkCase("Pattern Match Literal", "Pattern Matching",
                  "(match 42 (42 \"found\") (_ \"not found\"))", 3000),
    BenchmarkCase("Pattern Match Variable", "Pattern Matching",
                  "(match 42 (x (* x 2)))", 3000),
    BenchmarkCase("Pattern Match Type", "Pattern Matching",
                  "(match 42 ((integer? i) (* i 2)) (_ 0))", 2000),
    BenchmarkCase("Pattern Match List", "Pattern Matching",
                  "(match (list 1 2 3) ((a b c) b))", 2000),
    BenchmarkCase("Pattern Match Nested", "Pattern Matching",
                  "(match (list (list 1 2) (list 3 4)) (((a b) (c d)) (+ a b c d)))", 1000),
    BenchmarkCase("Pattern Match Cons", "Pattern Matching",
                  "(match (list 1 2 3 4 5) ((head . tail) head))", 2000),
    BenchmarkCase("Pattern Match Multiple", "Pattern Matching",
                  "(match 5 (1 \"one\") (2 \"two\") (3 \"three\") (4 \"four\") (5 \"five\") (_ \"other\"))", 2000),

    # === MATH FUNCTIONS ===
    BenchmarkCase("Sqrt", "Math",
                  "(sqrt 16)", 5000),
    BenchmarkCase("Sqrt Negative", "Math",
                  "(sqrt -4)", 3000),
    BenchmarkCase("Abs", "Math",
                  "(abs -42)", 5000),
    BenchmarkCase("Min/Max", "Math",
                  "(+ (min 1 2 3 4 5) (max 1 2 3 4 5))", 5000),
    BenchmarkCase("Pow", "Math",
                  "(pow 2 10)", 5000),
    BenchmarkCase("Trigonometry", "Math",
                  "(+ (sin 0.5) (cos 0.5) (tan 0.5))", 3000),
    BenchmarkCase("Logarithms", "Math",
                  "(+ (log 10) (log10 100))", 3000),
    BenchmarkCase("Complex Numbers", "Math",
                  "(+ (real (complex 3 4)) (imag (complex 3 4)))", 3000),
    BenchmarkCase("Rounding", "Math",
                  "(+ (round 3.7) (floor 3.7) (ceil 3.2))", 5000),

    # === TYPE PREDICATES ===
    BenchmarkCase("Type Checks", "Type Predicates",
                  "(and (number? 42) (string? \"hi\") (boolean? #t) (list? (list 1 2)))", 5000),
    BenchmarkCase("Integer/Float/Complex", "Type Predicates",
                  "(and (integer? 42) (float? 3.14) (complex? (complex 1 2)))", 3000),

    # === REALISTIC WORKLOADS ===
    BenchmarkCase("Data Processing", "Realistic",
                  """(let ((data (range 1 21)))
                       (fold + 0 (map (lambda (x) (* x x)) (filter (lambda (x) (> x 10)) data))))""", 200),
    BenchmarkCase("Nested Data Structure", "Realistic",
                  """(let ((users (list 
                                  (alist ("name" "Alice") ("age" 30))
                                  (alist ("name" "Bob") ("age" 25))
                                  (alist ("name" "Charlie") ("age" 35)))))
                       (map (lambda (user) (alist-get user "age")) users))""", 500),
    BenchmarkCase("Recursive List Processing", "Realistic",
                  """(let ((sum-list (lambda (lst)
                                      (if (null? lst) 
                                          0 
                                          (+ (first lst) (sum-list (rest lst)))))))
                       (sum-list (list 1 2 3 4 5 6 7 8 9 10)))""", 500),
    BenchmarkCase("Pattern Match Pipeline", "Realistic",
                  """(map (lambda (x) 
                              (match x
                                ((integer? i) (* i 2))
                                ((string? s) (string-length s))
                                (_ 0)))
                            (list 1 2 "hello" 3 "world" 4))""", 500),
    BenchmarkCase("Closure-based Counter", "Realistic",
                  """(let ((make-counter (lambda (start)
                                          (lambda (inc) (+ start inc)))))
                       (let ((counter (make-counter 10)))
                         (+ (counter 1) (counter 2) (counter 3))))""", 1000),
]


def benchmark_interpreter(expression: str, iterations: int) -> Tuple[float, float]:
    """Benchmark using tree-walking interpreter."""
    aifpl = AIFPL(use_bytecode=False)

    # Warmup
    for _ in range(min(10, iterations // 10)):
        aifpl.evaluate(expression)

    # Measure
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        aifpl.evaluate(expression)
        elapsed = time.perf_counter() - start
        times.append(elapsed)

    return statistics.mean(times), statistics.median(times)


def benchmark_bytecode(expression: str, iterations: int) -> Tuple[float, float]:
    """Benchmark using bytecode compiler/VM (execution only)."""
    tokenizer = AIFPLTokenizer()
    evaluator = AIFPLEvaluator()
    compiler = AIFPLCompiler()

    # Parse and compile once
    tokens = tokenizer.tokenize(expression)
    ast = AIFPLParser(tokens, expression).parse()
    code = compiler.compile(ast)

    # Setup VM
    vm = AIFPLVM()
    globals_dict = {**evaluator.CONSTANTS, **evaluator._builtin_functions}
    vm.set_globals(globals_dict)

    # Warmup
    for _ in range(min(10, iterations // 10)):
        vm.execute(code)

    # Measure
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        vm.execute(code)
        elapsed = time.perf_counter() - start
        times.append(elapsed)

    return statistics.mean(times), statistics.median(times)


def benchmark_bytecode_full(expression: str, iterations: int) -> Tuple[float, float]:
    """Benchmark using bytecode compiler/VM (full pipeline: parse + compile + execute)."""
    aifpl = AIFPL(use_bytecode=True)

    # Warmup
    for _ in range(min(10, iterations // 10)):
        aifpl.evaluate(expression)

    # Measure
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        aifpl.evaluate(expression)
        elapsed = time.perf_counter() - start
        times.append(elapsed)

    return statistics.mean(times), statistics.median(times)


def print_results_table(results: List[BenchmarkResult]):
    """Print results in a formatted table."""
    print(f"{'Benchmark':<35} {'Category':<18} {'Interp':<12} {'BC (Full)':<12} {'BC (Exec)':<12} {'Speed (F)':<10} {'Speed (E)':<10}")
    print("-" * 130)

    for result in results:
        interp_us = result.interpreter_mean * 1_000_000
        bc_full_us = result.bytecode_full_mean * 1_000_000
        bc_us = result.bytecode_mean * 1_000_000

        # Add emoji for speedup
        speedup_full_str = f"{result.speedup_full:.2f}x"
        speedup_exec_str = f"{result.speedup_exec:.2f}x"

        if result.speedup_exec > 10.0:
            speedup_exec_str += " 🚀"
        elif result.speedup_exec > 5.0:
            speedup_exec_str += " ✨"
        elif result.speedup_exec > 2.0:
            speedup_exec_str += " ✓"

        print(f"{result.name:<35} {result.category:<18} {interp_us:<12.2f} {bc_full_us:<12.2f} {bc_us:<12.2f} {speedup_full_str:<10} {speedup_exec_str:<10}")


def print_category_summary(results: List[BenchmarkResult]):
    """Print summary by category."""
    categories = {}
    for result in results:
        if result.category not in categories:
            categories[result.category] = {'full': [], 'exec': []}
        categories[result.category]['full'].append(result.speedup_full)
        categories[result.category]['exec'].append(result.speedup_exec)

    print("\n" + "=" * 130)
    print("SUMMARY BY CATEGORY")
    print("=" * 130)
    print(f"{'Category':<18} {'Count':<8} {'Type':<8} {'Avg':<12} {'Median':<12} {'Min':<12} {'Max':<12}")
    print("-" * 130)

    for category in sorted(categories.keys()):
        speedups_full = categories[category]['full']
        speedups_exec = categories[category]['exec']
        count = len(speedups_full)

        # Print full pipeline stats
        avg = statistics.mean(speedups_full)
        median = statistics.median(speedups_full)
        min_s = min(speedups_full)
        max_s = max(speedups_full)
        print(f"{category:<18} {count:<8} {'Full':<8} {avg:<12.2f}x {median:<12.2f}x {min_s:<12.2f}x {max_s:<12.2f}x")

        # Print exec only stats
        avg = statistics.mean(speedups_exec)
        median = statistics.median(speedups_exec)
        min_s = min(speedups_exec)
        max_s = max(speedups_exec)
        print(f"{'':<18} {'':<8} {'Exec':<8} {avg:<12.2f}x {median:<12.2f}x {min_s:<12.2f}x {max_s:<12.2f}x")


def print_overall_summary(results: List[BenchmarkResult]):
    """Print overall summary statistics."""
    speedups_full = [r.speedup_full for r in results]
    speedups_exec = [r.speedup_exec for r in results]

    print("\n" + "=" * 130)
    print("OVERALL SUMMARY")
    print("=" * 130)

    print(f"Total benchmarks:     {len(results)}")

    print("\nFull Pipeline (parse + compile + execute):")
    print(f"  Average speedup:    {statistics.mean(speedups_full):.2f}x")
    print(f"  Median speedup:     {statistics.median(speedups_full):.2f}x")
    print(f"  Min speedup:        {min(speedups_full):.2f}x")
    print(f"  Max speedup:        {max(speedups_full):.2f}x")
    print(f"  Std deviation:      {statistics.stdev(speedups_full):.2f}x")

    print("\nExecution Only (bytecode VM):")
    print(f"  Average speedup:    {statistics.mean(speedups_exec):.2f}x")
    print(f"  Median speedup:     {statistics.median(speedups_exec):.2f}x")
    print(f"  Min speedup:        {min(speedups_exec):.2f}x")
    print(f"  Max speedup:        {max(speedups_exec):.2f}x")
    print(f"  Std deviation:      {statistics.stdev(speedups_exec):.2f}x")

    # Categorize by speedup
    print("\nExecution Only Distribution:")
    spectacular = sum(1 for s in speedups_exec if s > 10.0)
    great = sum(1 for s in speedups_exec if 5.0 < s <= 10.0)
    good = sum(1 for s in speedups_exec if 2.0 < s <= 5.0)
    moderate = sum(1 for s in speedups_exec if 1.2 < s <= 2.0)
    similar = sum(1 for s in speedups_exec if 0.9 <= s <= 1.2)
    slower = sum(1 for s in speedups_exec if s < 0.9)

    print(f"  Spectacular (>10x): {spectacular:>3} ({spectacular/len(results)*100:.1f}%)")
    print(f"  Great (5-10x):      {great:>3} ({great/len(results)*100:.1f}%)")
    print(f"  Good (2-5x):        {good:>3} ({good/len(results)*100:.1f}%)")
    print(f"  Moderate (1.2-2x):  {moderate:>3} ({moderate/len(results)*100:.1f}%)")
    print(f"  Similar (0.9-1.2x): {similar:>3} ({similar/len(results)*100:.1f}%)")
    print(f"  Slower (<0.9x):     {slower:>3} ({slower/len(results)*100:.1f}%)")


def main():
    parser = argparse.ArgumentParser(description="Comprehensive bytecode vs interpreter benchmark")
    parser.add_argument('--quick', action='store_true', help='Run quick subset (first 30 benchmarks)')
    parser.add_argument('--category', type=str, help='Run only benchmarks in this category')
    parser.add_argument('--verbose', action='store_true', help='Show individual iteration times')
    args = parser.parse_args()

    # Filter benchmarks
    benchmarks = BENCHMARKS
    if args.category:
        benchmarks = [b for b in benchmarks if b.category.lower() == args.category.lower()]
        if not benchmarks:
            print(f"No benchmarks found for category: {args.category}")
            print(f"Available categories: {sorted(set(b.category for b in BENCHMARKS))}")
            return
    elif args.quick:
        benchmarks = BENCHMARKS[:30]

    print("=" * 110)
    print("COMPREHENSIVE AIFPL PERFORMANCE BENCHMARK")
    print("=" * 130)
    print()
    print(f"Running {len(benchmarks)} benchmarks...")
    if args.category:
        print(f"Category filter: {args.category}")
    print()

    results = []

    for i, bench in enumerate(benchmarks, 1):
        print(f"[{i}/{len(benchmarks)}] Running: {bench.name}...", end=" ", flush=True)

        try:
            # Run interpreter
            interp_mean, interp_median = benchmark_interpreter(bench.expression, bench.iterations)

            # Run bytecode full pipeline
            bc_full_mean, bc_full_median = benchmark_bytecode_full(bench.expression, bench.iterations)

            # Run bytecode
            bc_mean, bc_median = benchmark_bytecode(bench.expression, bench.iterations)

            # Calculate speedup
            speedup_full = interp_mean / bc_full_mean if bc_full_mean > 0 else 0
            speedup_exec = interp_mean / bc_mean if bc_mean > 0 else 0

            result = BenchmarkResult(
                name=bench.name,
                category=bench.category,
                interpreter_mean=interp_mean,
                interpreter_median=interp_median,
                bytecode_full_mean=bc_full_mean,
                bytecode_full_median=bc_full_median,
                bytecode_mean=bc_mean,
                bytecode_median=bc_median,
                speedup_full=speedup_full,
                speedup_exec=speedup_exec,
                iterations=bench.iterations
            )
            results.append(result)

            print(f"✓ Full: {speedup_full:.2f}x, Exec: {speedup_exec:.2f}x")

        except Exception as e:
            print(f"✗ ERROR: {e}")
            continue

    print()
    print("=" * 130)
    print("RESULTS")
    print("=" * 130)
    print()

    # Print detailed results
    print_results_table(results)

    # Print category summary
    print_category_summary(results)

    # Print overall summary
    print_overall_summary(results)

    print()
    print("=" * 130)


if __name__ == '__main__':
    main()
