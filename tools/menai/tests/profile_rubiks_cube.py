#!/usr/bin/env python3
"""
Profile test-rubiks-cube.menai

This script profiles the execution of the Rubik's cube test to identify
performance bottlenecks in the Menai compiler and VM.

Usage:
    python profile_rubiks_cube.py                    # Run with cProfile
    python profile_rubiks_cube.py --output stats.prof # Save profile data
    python profile_rubiks_cube.py --top 50           # Show top 50 functions
    python profile_rubiks_cube.py --sort time        # Sort by time instead of cumulative
"""

import argparse
import cProfile
import pstats
import sys
from io import StringIO
from pathlib import Path

# Import Menai
from menai import Menai


def profile_rubiks_cube(
    test_file: str = "test-rubiks-cube.menai",
    output_file: str = None,
    top_n: int = 80,
    sort_by: str = "cumulative"
):
    """
    Profile the Rubik's cube test.

    Args:
        test_file: Path to the Menai test file
        output_file: Optional file to save profile stats
        top_n: Number of top functions to display
        sort_by: Sort key for stats (cumulative, time, calls, etc.)
    """
    # Read the test file
    test_path = Path(test_file)
    if not test_path.exists():
        print(f"Error: Test file not found: {test_file}")
        sys.exit(1)

    with open(test_path, 'r', encoding='utf-8') as f:
        test_expression = f.read()

    print(f"Profiling: {test_file}")
    print("=" * 100)
    print(f"Expression length: {len(test_expression)} characters")
    print("=" * 100)

    # Create Menai instance
    menai = Menai()

    # Create profiler
    profiler = cProfile.Profile()

    # Run with profiling
    print("\nRunning test with profiler enabled...")
    profiler.enable()
    
    try:
        result = menai.evaluate(test_expression)
        print(f"\n✓ Test completed successfully")
        print(f"Result: {result}")
    except Exception as e:
        print(f"\n✗ Test failed with error: {e}")
        raise
    finally:
        profiler.disable()

    # Print stats
    print("\n" + "=" * 100)
    print(f"PROFILING RESULTS (Top {top_n} functions, sorted by {sort_by})")
    print("=" * 100)

    s = StringIO()
    stats = pstats.Stats(profiler, stream=s)
    stats.sort_stats(sort_by)
    stats.print_stats(top_n)

    print(s.getvalue())

    # Print callers for key functions (optional)
    print("\n" + "=" * 100)
    print("KEY FUNCTION CALLERS")
    print("=" * 100)
    
    s_callers = StringIO()
    stats_callers = pstats.Stats(profiler, stream=s_callers)
    
    # Look for specific Menai functions
    key_patterns = [
        'compile',
        'execute', 
        'evaluate',
        '_lex',
        '_parse',
        'semantic',
        'desugar',
    ]
    
    for pattern in key_patterns:
        print(f"\nCallers of functions matching '{pattern}':")
        stats_callers.print_callers(pattern)

    # Save to file if requested
    if output_file:
        output_path = Path(output_file)
        profiler.dump_stats(str(output_path))
        print(f"\n✓ Profile data saved to: {output_path}")
        print(f"  View with: python -m pstats {output_path}")

    # Print summary statistics
    print("\n" + "=" * 100)
    print("SUMMARY")
    print("=" * 100)
    
    # Get total time
    stats_obj = pstats.Stats(profiler)
    stats_obj.calc_callees()
    
    print(f"\nTo analyze further:")
    print(f"  1. Save profile data: python profile_rubiks_cube.py --output rubiks.prof")
    print(f"  2. View interactively: python -m pstats rubiks.prof")
    print(f"  3. In pstats shell, try: sort cumulative, stats 20, callers <function_name>")
    print(f"  4. Use snakeviz for visualization: pip install snakeviz && snakeviz rubiks.prof")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Profile Menai Rubik's Cube Test",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        '--test-file',
        default='test-rubiks-cube.menai',
        help='Path to Menai test file (default: test-rubiks-cube.menai)'
    )
    parser.add_argument(
        '--output',
        metavar='FILE',
        help='Save profile data to file (for later analysis with pstats or snakeviz)'
    )
    parser.add_argument(
        '--top',
        type=int,
        default=80,
        metavar='N',
        help='Show top N functions (default: 80)'
    )
    parser.add_argument(
        '--sort',
        default='cumulative',
        choices=['cumulative', 'time', 'calls', 'name', 'filename'],
        help='Sort results by this metric (default: cumulative)'
    )

    args = parser.parse_args()

    profile_rubiks_cube(
        test_file=args.test_file,
        output_file=args.output,
        top_n=args.top,
        sort_by=args.sort
    )


if __name__ == '__main__':
    main()
