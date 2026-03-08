#!/usr/bin/env python3
"""
Menai Profiler - Compile and profile any Menai source file.

Reads a Menai source file, compiles it, runs it under cProfile, and prints
(or saves) the resulting profile data.  Module paths are resolved the same
way as the disassembler: the file's own directory first, then the current
working directory.

Usage:
    python menai_profiler.py <file.menai>
    python menai_profiler.py <file.menai> --output stats.prof
    python menai_profiler.py <file.menai> --top 50
    python menai_profiler.py <file.menai> --sort time
"""

import argparse
import cProfile
import pstats
import sys
import traceback
from io import StringIO
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent / "src"))

from menai import Menai
from menai.menai_compiler import MenaiCompiler
from menai.menai_vm import MenaiVM


def build_module_path(source_path: Path) -> list[str]:
    """
    Build a deduplicated module search path for the given source file.

    Mirrors the strategy used by the disassembler:
      1. The file's own directory (so bare module names resolve next to the file)
      2. The current working directory (so project-root-relative import paths work)
    """
    file_dir = str(source_path.parent.absolute())
    cwd = str(Path.cwd())
    seen: set[str] = set()
    return [d for d in [file_dir, cwd] if not (d in seen or seen.add(d))]  # type: ignore[func-returns-value]


def compile_source(source_path: Path) -> object:
    """
    Compile a Menai source file and return the CodeObject.

    Args:
        source_path: Path to the .menai file.

    Returns:
        Compiled CodeObject.

    Raises:
        SystemExit: If compilation fails.
    """
    source = source_path.read_text(encoding="utf-8")
    module_path = build_module_path(source_path)
    menai = Menai(module_path=module_path)

    compiler = MenaiCompiler(module_loader=menai)
    try:
        return compiler.compile(source, name=str(source_path))

    except Exception as exc:
        print(f"Compilation error: {exc}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)


def run_profile(
    source_path: Path,
    output_file: str | None,
    top_n: int,
    sort_by: str,
) -> int:
    """
    Compile the file, execute it under cProfile, and display results.

    Args:
        source_path:     Path to the .menai source file.
        output_file:     Optional path to save raw profile data (.prof).
        top_n:           Number of top functions to print.
        sort_by:         pstats sort key.

    Returns:
        Exit code (0 = success, 1 = execution error).
    """
    source = source_path.read_text(encoding="utf-8")
    module_path = build_module_path(source_path)

    # Build a fresh Menai instance with the correct module path.
    # The Menai constructor compiles and caches the prelude, so compilation
    # cost is separated from the user program's execution cost.
    print(f"Initialising Menai (compiling prelude)...", file=sys.stderr)
    menai = Menai(module_path=module_path)

    print(f"Compiling: {source_path}", file=sys.stderr)
    compiler = MenaiCompiler(module_loader=menai)
    try:
        code = compiler.compile(source, name=str(source_path))

    except Exception as exc:
        print(f"Compilation error: {exc}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        return 1

    print(f"Running with profiler enabled...", file=sys.stderr)
    print("=" * 100)

    profiler = cProfile.Profile()
    profiler.enable()

    result = None
    exec_error = None
    try:
        result = menai.vm.execute(code, Menai.CONSTANTS, menai._prelude)

    except Exception as exc:
        exec_error = exc

    finally:
        profiler.disable()

    if exec_error is not None:
        print(f"\n✗ Execution failed: {exec_error}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        return 1

    print(f"\n✓ Execution completed successfully")
    print(f"Result: {result.describe() if result is not None else '<none>'}")

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
        print(f"\n✓ Profile data saved to: {output_file}")
        print(f"  View with: python -m pstats {output_file}")
        print(f"  Or:        snakeviz {output_file}")

    return 0


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Compile and profile a Menai source file",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("file", help="Menai source file to profile")
    parser.add_argument(
        "--output", "-o",
        metavar="FILE",
        help="Save raw profile data to FILE (viewable with pstats or snakeviz)",
    )
    parser.add_argument(
        "--top",
        type=int,
        default=40,
        metavar="N",
        help="Show top N functions in the profile output (default: 40)",
    )
    parser.add_argument(
        "--sort",
        default="cumulative",
        choices=["cumulative", "time", "calls", "name", "filename"],
        help="Sort profile results by this metric (default: cumulative)",
    )

    args = parser.parse_args()

    source_path = Path(args.file)
    if not source_path.exists():
        print(f"Error: file not found: {args.file}", file=sys.stderr)
        return 1

    return run_profile(
        source_path=source_path,
        output_file=args.output,
        top_n=args.top,
        sort_by=args.sort,
    )


if __name__ == "__main__":
    sys.exit(main())
