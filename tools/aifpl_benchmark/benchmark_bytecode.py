#!/usr/bin/env python3
"""
Benchmark bytecode vs interpreter performance.

Compares the bytecode compiler/VM against the tree-walking interpreter
on the same benchmark suite.
"""

import argparse
import statistics
import sys
import time
from pathlib import Path
from typing import List, Tuple

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_evaluator import AIFPLEvaluator


class BenchmarkCase:
    """Single benchmark case."""
    
    def __init__(self, name: str, expression: str, iterations: int = 100):
        self.name = name
        self.expression = expression
        self.iterations = iterations


# Benchmark suite - same as regular benchmarks
BENCHMARKS = [
    # Arithmetic
    BenchmarkCase("Simple Addition", "(+ 1 2 3 4 5)", 2000),
    BenchmarkCase("Nested Arithmetic", "(* (+ 1 2 3) (- 10 5) (/ 20 4))", 2000),
    BenchmarkCase("Complex Math", "(sqrt (+ (* 3 3) (* 4 4)))", 2000),
    
    # Functions
    BenchmarkCase("Simple Lambda", "((lambda (x) (* x x)) 5)", 2000),
    BenchmarkCase("Lambda Multiple Args", "((lambda (x y z) (+ (* x y) z)) 3 4 5)", 2000),
    
    # Lists
    BenchmarkCase("List Creation (10)", "(list 1 2 3 4 5 6 7 8 9 10)", 2000),
    BenchmarkCase("List Append", "(append (list 1 2 3) (list 4 5 6))", 2000),
    BenchmarkCase("List Reverse (20)", "(reverse (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))", 2000),
    
    # Higher-order functions
    BenchmarkCase("Map (10)", "(map (lambda (x) (* x x)) (list 1 2 3 4 5 6 7 8 9 10))", 1000),
    BenchmarkCase("Map (100)", "(map (lambda (x) (* x x)) (range 1 101))", 100),
    BenchmarkCase("Filter (100)", "(filter (lambda (x) (> x 50)) (range 1 101))", 100),
    BenchmarkCase("Fold (100)", "(fold + 0 (range 1 101))", 100),
    BenchmarkCase("Map+Fold Pipeline", "(fold + 0 (map (lambda (x) (* x x)) (range 1 101)))", 100),
    
    # Let bindings
    BenchmarkCase("Simple Let", "(let ((x 5) (y 10)) (+ x y))", 2000),
    BenchmarkCase("Let Many Bindings", "(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) (+ a b c d e f g h i j))", 1000),
    
    # Recursion
    BenchmarkCase("Factorial (10)", """
        (let ((factorial (lambda (n)
                           (if (<= n 1) 1 (* n (factorial (- n 1)))))))
          (factorial 10))
    """, 200),
    
    BenchmarkCase("Tail Recursive Sum (100)", """
        (let ((sum-tail (lambda (n acc)
                          (if (<= n 0) acc (sum-tail (- n 1) (+ acc n))))))
          (sum-tail 100 0))
    """, 200),
    
    # Strings
    BenchmarkCase("String Append", '(string-append "hello" " " "world")', 2000),
    BenchmarkCase("String Manipulation", '(string-upcase (string-append "hello" " " "world"))', 2000),
    
    # Alists
    BenchmarkCase("Alist Creation", '(alist ("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5))', 2000),
    BenchmarkCase("Alist Get", '(alist-get (alist ("name" "Alice") ("age" 30)) "age")', 2000),
]


def benchmark_interpreter(expression: str, iterations: int) -> Tuple[float, float]:
    """Benchmark using tree-walking interpreter."""
    aifpl = AIFPL()
    
    # Warmup
    for _ in range(10):
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
    """Benchmark using bytecode compiler/VM."""
    tokenizer = AIFPLTokenizer()
    evaluator = AIFPLEvaluator()
    compiler = AIFPLCompiler()
    
    # Parse once
    tokens = tokenizer.tokenize(expression)
    ast = AIFPLParser(tokens, expression).parse()
    
    # Compile once
    code = compiler.compile(ast)
    
    # Setup VM
    vm = AIFPLVM(evaluator)
    globals_dict = {**evaluator.CONSTANTS, **evaluator._builtin_functions}
    vm.set_globals(globals_dict)
    
    # Warmup
    for _ in range(10):
        vm.execute(code)
    
    # Measure
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        vm.execute(code)
        elapsed = time.perf_counter() - start
        times.append(elapsed)
    
    return statistics.mean(times), statistics.median(times)


def benchmark_bytecode_with_compilation(expression: str, iterations: int) -> Tuple[float, float]:
    """Benchmark bytecode including compilation time (for one-shot scenarios)."""
    tokenizer = AIFPLTokenizer()
    evaluator = AIFPLEvaluator()
    compiler = AIFPLCompiler()
    
    # Warmup
    for _ in range(10):
        tokens = tokenizer.tokenize(expression)
        ast = AIFPLParser(tokens, expression).parse()
        code = compiler.compile(ast)
        vm = AIFPLVM(evaluator)
        globals_dict = {**evaluator.CONSTANTS, **evaluator._builtin_functions}
        vm.set_globals(globals_dict)
        vm.execute(code)
    
    # Measure
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        
        tokens = tokenizer.tokenize(expression)
        ast = AIFPLParser(tokens, expression).parse()
        code = compiler.compile(ast)
        vm = AIFPLVM(evaluator)
        globals_dict = {**evaluator.CONSTANTS, **evaluator._builtin_functions}
        vm.set_globals(globals_dict)
        vm.execute(code)
        
        elapsed = time.perf_counter() - start
        times.append(elapsed)
    
    return statistics.mean(times), statistics.median(times)


def main():
    parser = argparse.ArgumentParser(description="Benchmark bytecode vs interpreter")
    parser.add_argument('--quick', action='store_true', help='Run quick subset')
    parser.add_argument('--with-compilation', action='store_true', 
                       help='Include compilation time (one-shot scenario)')
    args = parser.parse_args()
    
    benchmarks = BENCHMARKS[:10] if args.quick else BENCHMARKS
    
    print("=" * 100)
    print("AIFPL BYTECODE vs INTERPRETER BENCHMARK")
    print("=" * 100)
    print()
    
    if args.with_compilation:
        print("Mode: Including compilation time (one-shot evaluation)")
    else:
        print("Mode: Execution only (pre-compiled)")
    print()
    
    print(f"{'Benchmark':<30} {'Interpreter':<15} {'Bytecode':<15} {'Speedup':<10}")
    print("-" * 100)
    
    speedups = []
    
    for bench in benchmarks:
        # Run interpreter
        try:
            interp_mean, interp_median = benchmark_interpreter(bench.expression, bench.iterations)
        except Exception as e:
            print(f"{bench.name:<30} ERROR: {e}")
            continue
        
        # Run bytecode
        try:
            if args.with_compilation:
                bc_mean, bc_median = benchmark_bytecode_with_compilation(bench.expression, bench.iterations)
            else:
                bc_mean, bc_median = benchmark_bytecode(bench.expression, bench.iterations)
        except Exception as e:
            print(f"{bench.name:<30} {interp_mean*1000:.3f}ms         ERROR: {e}")
            continue
        
        # Calculate speedup
        speedup = interp_mean / bc_mean if bc_mean > 0 else 0
        speedups.append(speedup)
        
        # Format output
        interp_str = f"{interp_mean*1000:.3f}ms"
        bc_str = f"{bc_mean*1000:.3f}ms"
        speedup_str = f"{speedup:.2f}x"
        
        # Add emoji for significant speedup
        if speedup > 2.0:
            speedup_str += " 🚀"
        elif speedup > 1.5:
            speedup_str += " ✨"
        elif speedup < 0.9:
            speedup_str += " ⚠️"
        
        print(f"{bench.name:<30} {interp_str:<15} {bc_str:<15} {speedup_str:<10}")
    
    # Summary
    print()
    print("=" * 100)
    print("SUMMARY")
    print("=" * 100)
    
    if speedups:
        avg_speedup = statistics.mean(speedups)
        median_speedup = statistics.median(speedups)
        min_speedup = min(speedups)
        max_speedup = max(speedups)
        
        print(f"Average speedup:  {avg_speedup:.2f}x")
        print(f"Median speedup:   {median_speedup:.2f}x")
        print(f"Min speedup:      {min_speedup:.2f}x")
        print(f"Max speedup:      {max_speedup:.2f}x")
        
        # Count significant improvements
        fast = sum(1 for s in speedups if s > 2.0)
        moderate = sum(1 for s in speedups if 1.5 <= s <= 2.0)
        similar = sum(1 for s in speedups if 0.9 <= s < 1.5)
        slower = sum(1 for s in speedups if s < 0.9)
        
        print()
        print(f"Significantly faster (>2x):   {fast}")
        print(f"Moderately faster (1.5-2x):   {moderate}")
        print(f"Similar (0.9-1.5x):           {similar}")
        print(f"Slower (<0.9x):               {slower}")


if __name__ == '__main__':
    main()
