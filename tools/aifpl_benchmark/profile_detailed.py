#!/usr/bin/env python3
"""
Detailed profiling for specific AIFPL workloads to identify bottlenecks.
"""

import cProfile
import pstats
from io import StringIO
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from aifpl import AIFPL


def profile_workload(name, expression, iterations=100):
    """Profile a specific workload."""
    print(f"\n{'='*80}")
    print(f"PROFILING: {name}")
    print(f"{'='*80}")
    print(f"Expression: {expression[:100]}...")
    print(f"Iterations: {iterations}")
    
    aifpl = AIFPL()
    profiler = cProfile.Profile()
    
    # Warmup
    for _ in range(5):
        aifpl.evaluate(expression)
    
    # Profile
    profiler.enable()
    for _ in range(iterations):
        aifpl.evaluate(expression)
    profiler.disable()
    
    # Print stats
    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats('cumulative')
    stats.print_stats(40)
    
    print(s.getvalue())


def main():
    """Profile key workloads."""
    
    # Workload 1: Map with 100 elements (higher-order function overhead)
    profile_workload(
        "Map (100 elements)",
        "(map (lambda (x) (* x x)) (range 1 101))",
        iterations=50
    )
    
    # Workload 2: Let with many bindings (environment overhead)
    profile_workload(
        "Let with Many Bindings (10)",
        "(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) (+ a b c d e f g h i j))",
        iterations=100
    )
    
    # Workload 3: Tail recursive sum (recursion + environment overhead)
    profile_workload(
        "Tail Recursive Sum (100)",
        """(let ((sum-tail (lambda (n acc)
                            (if (<= n 0)
                                acc
                                (sum-tail (- n 1) (+ acc n))))))
             (sum-tail 100 0))""",
        iterations=50
    )
    
    # Workload 4: Map + Fold pipeline (combined overhead)
    profile_workload(
        "Map + Fold Pipeline",
        "(fold + 0 (map (lambda (x) (* x x)) (range 1 101)))",
        iterations=50
    )


if __name__ == '__main__':
    main()
