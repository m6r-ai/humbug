#!/usr/bin/env python3
"""
Profiler benchmark for AIFPL map/range expression.
Focuses on runtime (VM execution) performance only.

This runs the expression for several seconds to get good profiling statistics.
"""

import argparse
import cProfile
import pstats
import sys
import time
from io import StringIO
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))

from aifpl import AIFPL
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM


def dump_bytecode(code, indent=0):
    """
    Recursively dump bytecode for a CodeObject and all nested code objects.

    Args:
        code: CodeObject to dump
        indent: Indentation level for nested code objects
    """
    prefix = "  " * indent
    print(f"{prefix}{'=' * 80}")
    print(f"{prefix}CodeObject: {code.name}")
    print(f"{prefix}{'=' * 80}")
    print(f"{prefix}Parameters: {code.param_count}")
    print(f"{prefix}Locals: {code.local_count}")
    print(f"{prefix}Free vars: {code.free_vars}")
    print()

    # Constants
    if code.constants:
        print(f"{prefix}Constants ({len(code.constants)}):")
        for i, const in enumerate(code.constants):
            print(f"{prefix}  {i}: {const}")
        print()

    # Names
    if code.names:
        print(f"{prefix}Names ({len(code.names)}):")
        for i, name in enumerate(code.names):
            print(f"{prefix}  {i}: {name}")
        print()

    # Instructions
    print(f"{prefix}Bytecode ({len(code.instructions)} instructions):")
    for i, instr in enumerate(code.instructions):
        print(f"{prefix}  {i:3d}: {instr}")
    print()

    # Nested code objects
    if code.code_objects:
        print(f"{prefix}Nested Code Objects ({len(code.code_objects)}):")
        print()
        for i, nested in enumerate(code.code_objects):
            print(f"{prefix}[{i}]")
            dump_bytecode(nested, indent + 1)


def profile_runtime_only(expression: str, duration_seconds: float = 3.0, dump_bc: bool = False):
    """
    Profile VM execution only (no compilation) for the given expression.

    Args:
        expression: AIFPL expression to profile
        duration_seconds: How long to run the benchmark
        dump_bc: Whether to dump bytecode

    Returns:
        The cProfile.Profile object with profiling data
    """
    print(f"Expression: {expression}")
    print(f"Target duration: {duration_seconds} seconds")
    print("=" * 100)

    # Setup - compile once
    compiler = AIFPLCompiler()
    vm = AIFPLVM()

    # Get prelude and constants
    aifpl = AIFPL()
    prelude = aifpl._prelude
    constants = AIFPL.CONSTANTS

    # Compile the expression once
    print("\nCompiling expression...")
    code = compiler.compile(expression)
    print(f"Compiled to {len(code.instructions)} bytecode instructions")
    print(f"Constants: {len(code.constants)}, Names: {len(code.names)}, Code objects: {len(code.code_objects)}")

    # Dump bytecode if requested
    if dump_bc:
        print("\n" + "=" * 100)
        print("BYTECODE DUMP")
        print("=" * 100)
        print()
        dump_bytecode(code)
        return None  # Don't run profiling if just dumping bytecode

    # Warmup
    print("\nWarming up...")
    for _ in range(10):
        vm.execute(code, constants, prelude)

    # Estimate iterations needed for target duration
    print("\nEstimating iterations...")
    start = time.perf_counter()
    test_iterations = 100
    for _ in range(test_iterations):
        vm.execute(code, constants, prelude)
    elapsed = time.perf_counter() - start

    time_per_iteration = elapsed / test_iterations
    target_iterations = int(duration_seconds / time_per_iteration)

    print(f"Time per iteration: {time_per_iteration * 1_000_000:.2f} µs")
    print(f"Target iterations: {target_iterations:,}")
    print(f"Estimated duration: {target_iterations * time_per_iteration:.2f} seconds")

    # Profile the VM execution
    print("\nProfiling VM execution...")
    print("=" * 100)

    profiler = cProfile.Profile()
    profiler.enable()

    start_time = time.perf_counter()
    actual_iterations = 0

    for i in range(target_iterations):
        vm.execute(code, constants, prelude)
        actual_iterations += 1

    elapsed_time = time.perf_counter() - start_time

    profiler.disable()

    # Print statistics
    print(f"\nCompleted {actual_iterations:,} iterations in {elapsed_time:.3f} seconds")
    print(f"Average time per iteration: {elapsed_time / actual_iterations * 1_000_000:.2f} µs")
    print(f"Operations per second: {actual_iterations / elapsed_time:.1f}")

    # Print profiling results
    print("\n" + "=" * 100)
    print("PROFILING RESULTS - Sorted by Cumulative Time")
    print("=" * 100)

    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats('cumulative')
    stats.print_stats(50)  # Top 50 functions
    print(s.getvalue())

    # Also print sorted by total time
    print("\n" + "=" * 100)
    print("PROFILING RESULTS - Sorted by Total Time")
    print("=" * 100)

    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats('tottime')
    stats.print_stats(50)  # Top 50 functions
    print(s.getvalue())

    # Print some key statistics
    print("\n" + "=" * 100)
    print("KEY STATISTICS")
    print("=" * 100)

    stats = pstats.Stats(profiler)

    # Get stats for specific VM functions
    vm_stats = {}
    for func_key in stats.stats:
        filename, line, func_name = func_key
        if 'aifpl_vm' in filename or 'execute' in func_name.lower():
            vm_stats[func_name] = stats.stats[func_key]

    if vm_stats:
        print("\nVM-related functions:")
        for func_name, stat in sorted(vm_stats.items(), key=lambda x: x[1][2], reverse=True)[:10]:
            cc, nc, tt, ct, callers = stat
            print(f"  {func_name:40s} calls: {nc:8,}  tottime: {tt:8.3f}s  cumtime: {ct:8.3f}s")

    return profiler


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="AIFPL Runtime Profiler - Profile VM execution performance",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        'duration',
        nargs='?',
        type=float,
        default=3.0,
        help='Duration in seconds to run the benchmark (default: 3.0)'
    )
    parser.add_argument(
        '--dump-bytecode',
        action='store_true',
        help='Dump compiled bytecode and exit (no profiling)'
    )
    parser.add_argument(
        '--expression',
        '-e',
        default='(map (lambda (x) (* x x)) (range 1 101))',
        help='AIFPL expression to profile (default: map square over range 1-100)'
    )
    parser.add_argument(
        '--save',
        action='store_true',
        help='Save detailed stats to file'
    )

    args = parser.parse_args()

    profiler = profile_runtime_only(args.expression, args.duration, args.dump_bytecode)

    # Save detailed stats to file if profiling was run
    if profiler and args.save:
        output_file = "profile_stats.txt"
        print(f"\nSaving detailed stats to {output_file}...")
        with open(output_file, 'w') as f:
            stats = pstats.Stats(profiler, stream=f)
            stats.sort_stats('cumulative')
            stats.print_stats()
        print(f"Detailed stats saved to {output_file}")


if __name__ == '__main__':
    main()
