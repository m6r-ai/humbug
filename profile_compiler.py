#!/usr/bin/env python3
"""Profile the AIFPL compiler to identify performance bottlenecks."""

import cProfile
import pstats
import io
from pathlib import Path
import sys

# Add src to path
src_path = Path(__file__).parent / "src"
sys.path.insert(0, str(src_path))

from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_compiler import AIFPLCompiler


def profile_compilation(test_cases: dict[str, str], iterations: int = 100):
    """Profile compilation of various test cases."""
    compiler = AIFPLCompiler()
    
    def run_tests():
        for name, code in test_cases.items():
            for _ in range(iterations):
                # Tokenize
                tokenizer = AIFPLTokenizer()
                tokens = tokenizer.tokenize(code)
                
                # Parse
                parser = AIFPLParser(tokens, code)
                ast = parser.parse()
                
                # Compile
                compiler.compile(ast, name=name)
    
    # Profile it
    profiler = cProfile.Profile()
    profiler.enable()
    run_tests()
    profiler.disable()
    
    return profiler


def print_profile_stats(profiler: cProfile.Profile, sort_by: str = 'cumulative', limit: int = 30):
    """Print formatted profile statistics."""
    s = io.StringIO()
    ps = pstats.Stats(profiler, stream=s)
    ps.strip_dirs()
    ps.sort_stats(sort_by)
    ps.print_stats(limit)
    
    print(s.getvalue())


def main():
    """Run profiling with various test cases."""
    
    # Test cases covering different compiler features
    test_cases = {
        # Simple expressions
        "simple_arithmetic": "(+ 1 2 3 4 5)",
        "nested_arithmetic": "(+ (* 2 3) (- 10 5) (/ 20 4))",
        
        # Variables and constants
        "many_constants": "(list 1 2 3 4 5 6 7 8 9 10 \"a\" \"b\" \"c\" \"d\" \"e\")",
        
        # Lambda expressions
        "simple_lambda": "(lambda (x) (* x x))",
        "nested_lambda": "(lambda (x) (lambda (y) (lambda (z) (+ x y z))))",
        "lambda_with_closure": "(lambda (x) (lambda (y) (+ x y)))",
        
        # Let bindings
        "simple_let": "(let ((x 5) (y 10)) (+ x y))",
        "nested_let": "(let ((x 1)) (let ((y 2)) (let ((z 3)) (+ x y z))))",
        "recursive_let": "(let ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))",
        "mutual_recursion": """
            (let ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
              (even? 10))
        """,
        
        # Conditionals
        "simple_if": "(if (> 5 3) \"yes\" \"no\")",
        "nested_if": "(if (> x 0) (if (< x 10) \"small\" \"big\") \"negative\")",
        
        # Pattern matching
        "simple_match": "(match x (42 \"found\") (_ \"default\"))",
        "type_match": "(match x ((number? n) (* n 2)) ((string? s) (string-upcase s)) (_ \"unknown\"))",
        "list_match": "(match lst (() \"empty\") ((x) \"singleton\") ((x y) \"pair\") (_ \"many\"))",
        "cons_match": "(match lst ((head . tail) head) (_ \"empty\"))",
        
        # Higher-order functions
        "map": "(map (lambda (x) (* x 2)) (list 1 2 3 4 5))",
        "filter": "(filter (lambda (x) (> x 0)) (list -2 -1 0 1 2))",
        "fold": "(fold + 0 (list 1 2 3 4 5))",
        
        # Complex real-world example
        "fibonacci": """
            (let ((fib (lambda (n)
                         (if (<= n 1)
                             n
                             (+ (fib (- n 1)) (fib (- n 2)))))))
              (fib 10))
        """,
        
        "quicksort": """
            (let ((quicksort (lambda (lst)
                              (if (null? lst)
                                  ()
                                  (let ((pivot (first lst))
                                        (rest (rest lst)))
                                    (append
                                      (quicksort (filter (lambda (x) (< x pivot)) rest))
                                      (list pivot)
                                      (quicksort (filter (lambda (x) (>= x pivot)) rest))))))))
              (quicksort (list 3 1 4 1 5 9 2 6)))
        """,
    }
    
    print("=" * 80)
    print("AIFPL Compiler Profiling")
    print("=" * 80)
    print(f"\nRunning {len(test_cases)} test cases, 100 iterations each...")
    print("(This includes tokenizing + parsing + compiling)")
    print()
    
    # Run profiling
    profiler = profile_compilation(test_cases, iterations=100)
    
    # Print results sorted by cumulative time
    print("\n" + "=" * 80)
    print("TOP FUNCTIONS BY CUMULATIVE TIME")
    print("=" * 80)
    print_profile_stats(profiler, sort_by='cumulative', limit=40)
    
    # Print results sorted by time per call
    print("\n" + "=" * 80)
    print("TOP FUNCTIONS BY TIME PER CALL")
    print("=" * 80)
    print_profile_stats(profiler, sort_by='time', limit=40)
    
    # Print results sorted by call count
    print("\n" + "=" * 80)
    print("MOST CALLED FUNCTIONS")
    print("=" * 80)
    print_profile_stats(profiler, sort_by='calls', limit=40)
    
    # Generate detailed stats file
    output_file = "compiler_profile_stats.txt"
    with open(output_file, 'w') as f:
        ps = pstats.Stats(profiler, stream=f)
        ps.strip_dirs()
        ps.sort_stats('cumulative')
        ps.print_stats()
    
    print(f"\n\nDetailed stats written to: {output_file}")
    print("\nKey metrics to look for:")
    print("  - Functions with high 'cumtime' (cumulative time)")
    print("  - Functions with high 'ncalls' (call count)")
    print("  - Functions with high 'percall' (time per call)")
    print("\nNext steps:")
    print("  1. Identify the hottest functions")
    print("  2. Check if they're doing unnecessary work")
    print("  3. Look for O(n) operations that could be O(1)")


if __name__ == "__main__":
    main()
