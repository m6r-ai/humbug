#!/usr/bin/env python3
"""Benchmark the AIFPL compiler to measure performance improvements."""

import time
from pathlib import Path
import sys

# Add src to path
src_path = Path(__file__).parent / "src"
sys.path.insert(0, str(src_path))

from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_compiler import AIFPLCompiler


def benchmark_compilation(test_cases: dict[str, str], iterations: int = 1000):
    """Benchmark compilation of various test cases."""
    compiler = AIFPLCompiler()
    
    # Warmup
    for name, code in test_cases.items():
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize(code)
        parser = AIFPLParser(tokens, code)
        ast = parser.parse()
        compiler.compile(ast, name=name)
    
    # Actual benchmark
    start = time.perf_counter()
    
    for _ in range(iterations):
        for name, code in test_cases.items():
            tokenizer = AIFPLTokenizer()
            tokens = tokenizer.tokenize(code)
            parser = AIFPLParser(tokens, code)
            ast = parser.parse()
            compiler.compile(ast, name=name)
    
    end = time.perf_counter()
    total_time = end - start
    
    total_compilations = len(test_cases) * iterations
    avg_time_ms = (total_time / total_compilations) * 1000
    
    return total_time, total_compilations, avg_time_ms


def main():
    """Run benchmarks."""
    
    # Test cases covering different compiler features
    test_cases = {
        "simple_arithmetic": "(+ 1 2 3 4 5)",
        "nested_arithmetic": "(+ (* 2 3) (- 10 5) (/ 20 4))",
        "many_constants": "(list 1 2 3 4 5 6 7 8 9 10 \"a\" \"b\" \"c\" \"d\" \"e\")",
        "simple_lambda": "(lambda (x) (* x x))",
        "nested_lambda": "(lambda (x) (lambda (y) (lambda (z) (+ x y z))))",
        "simple_let": "(let ((x 5) (y 10)) (+ x y))",
        "recursive_let": "(let ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))",
        "simple_if": "(if (> 5 3) \"yes\" \"no\")",
        "simple_match": "(match x (42 \"found\") (_ \"default\"))",
        "map": "(map (lambda (x) (* x 2)) (list 1 2 3 4 5))",
        "fibonacci": """
            (let ((fib (lambda (n)
                         (if (<= n 1)
                             n
                             (+ (fib (- n 1)) (fib (- n 2)))))))
              (fib 10))
        """,
    }
    
    print("=" * 80)
    print("AIFPL Compiler Benchmark")
    print("=" * 80)
    print(f"\nTest cases: {len(test_cases)}")
    print("Iterations: 1000")
    print("Total compilations: 11,000")
    print("\nRunning benchmark...\n")
    
    total_time, total_compilations, avg_time_ms = benchmark_compilation(test_cases, iterations=1000)
    
    print(f"Total time:          {total_time:.3f} seconds")
    print(f"Total compilations:  {total_compilations:,}")
    print(f"Average per compile: {avg_time_ms:.4f} ms")
    print(f"Compilations/sec:    {total_compilations/total_time:.0f}")
    
    print("\n" + "=" * 80)
    print("Performance Breakdown:")
    print("=" * 80)
    
    # Benchmark individual test cases
    for name, code in test_cases.items():
        compiler = AIFPLCompiler()
        tokenizer = AIFPLTokenizer()
        tokens = tokenizer.tokenize(code)
        parser = AIFPLParser(tokens, code)
        ast = parser.parse()
        
        # Warmup
        for _ in range(10):
            compiler.compile(ast, name=name)
        
        # Benchmark
        iterations = 1000
        start = time.perf_counter()
        for _ in range(iterations):
            compiler.compile(ast, name=name)
        end = time.perf_counter()
        
        time_per_compile = ((end - start) / iterations) * 1000000  # microseconds
        print(f"{name:25s}: {time_per_compile:7.2f} μs/compile")


if __name__ == "__main__":
    main()
