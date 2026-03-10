#!/usr/bin/env python3
"""Command-line interface for the pipeline engine."""

import argparse
import cProfile
import io
import pstats
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from pipeline_engine import PipelineResult, execute_pipeline
from pipeline_optimizer import optimize_pipeline
from pipeline_parser import PipelineParseError, load_pipeline
from pipeline_step import MenaiStep, ToolStep


def _format_elapsed(seconds: float) -> str:
    """Format an elapsed time value for display."""
    if seconds >= 1.0:
        return f"{seconds:.3f}s"

    return f"{seconds * 1000:.1f}ms"


def _print_step_summary(result: PipelineResult, verbose: bool, timings: bool) -> None:
    """Print a per-step execution summary."""
    id_width = max((len(r.step_id) for r in result.step_results), default=8)

    for step_result in result.step_results:
        status = "OK" if step_result.success else "FAIL"
        timing_str = f"  {_format_elapsed(step_result.elapsed_s):>8}" if timings else ""
        print(f"  [{status}] {step_result.step_id:<{id_width}}{timing_str}")

        if not step_result.success:
            print(f"         {step_result.error}")

        elif verbose and step_result.value:
            truncated = step_result.value
            if len(truncated) > 120:
                truncated = truncated[:117] + "..."

            print(f"         {truncated}")


def _print_timings_bar(result: PipelineResult) -> None:
    """Print a simple proportional timing bar for each step."""
    total = sum(r.elapsed_s for r in result.step_results)
    if total == 0:
        return

    bar_width = 40
    id_width = max((len(r.step_id) for r in result.step_results), default=8)

    print("  Timing breakdown:")
    for step_result in result.step_results:
        proportion = step_result.elapsed_s / total
        filled = round(proportion * bar_width)
        bar = "█" * filled + "░" * (bar_width - filled)
        pct = proportion * 100
        print(
            f"    {step_result.step_id:<{id_width}}  [{bar}]  "
            f"{_format_elapsed(step_result.elapsed_s):>8}  ({pct:4.1f}%)"
        )


def _print_pipeline_summary(
    pipeline_path: Path,
    optimized: bool,
    step_count_before: int,
    step_count_after: int
) -> None:
    """Print a summary of the pipeline structure."""
    print(f"  Pipeline: {pipeline_path}")
    print(f"  Steps:    {step_count_before}", end="")
    if optimized and step_count_after < step_count_before:
        collapsed = step_count_before - step_count_after
        print(f" -> {step_count_after} ({collapsed} Menai step(s) collapsed)", end="")

    print()


def _run_with_profile(pipeline, sort_key: str, lines: int) -> tuple:
    """
    Execute a pipeline under cProfile and return (result, profile_stats_string).

    Args:
        pipeline: The pipeline to execute
        sort_key: pstats sort key (e.g. 'cumulative', 'tottime')
        lines: Number of functions to show in the profile output

    Returns:
        Tuple of (PipelineResult, formatted profile string)
    """
    profiler = cProfile.Profile()
    profiler.enable()
    result = execute_pipeline(pipeline)
    profiler.disable()

    buf = io.StringIO()
    stats = pstats.Stats(profiler, stream=buf)
    stats.strip_dirs()
    stats.sort_stats(sort_key)
    stats.print_stats(lines)

    return result, buf.getvalue()


def main() -> int:
    """
    Entry point for the pipeline CLI.

    Returns:
        Exit code (0 for success, non-zero for failure)
    """
    parser = argparse.ArgumentParser(
        description="Execute a Menai pipeline from a JSON definition file.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python pipeline_cli.py examples/hello-timestamp/pipeline.json
  python pipeline_cli.py --no-optimize examples/adjacent-collapse/pipeline.json
  python pipeline_cli.py --verbose examples/file-transform/pipeline.json
  python pipeline_cli.py --timings examples/clock-and-file/pipeline.json
  python pipeline_cli.py --profile examples/file-transform/pipeline.json
        """
    )

    parser.add_argument(
        "pipeline",
        type=Path,
        help="Path to the pipeline JSON file"
    )

    parser.add_argument(
        "--no-optimize",
        action="store_true",
        help="Disable adjacent Menai step collapsing"
    )

    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show truncated output for each step"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Parse and validate the pipeline without executing it"
    )

    parser.add_argument(
        "--timings", "-t",
        action="store_true",
        help="Show per-step elapsed time and a proportional timing bar"
    )

    parser.add_argument(
        "--profile", "-p",
        action="store_true",
        help="Run the pipeline under cProfile and print the top hotspots"
    )

    parser.add_argument(
        "--profile-lines",
        type=int,
        default=30,
        metavar="N",
        help="Number of functions to show in profile output (default: 30)"
    )

    parser.add_argument(
        "--profile-sort",
        default="cumulative",
        choices=["cumulative", "tottime", "calls", "filename"],
        help="Sort key for profile output (default: cumulative)"
    )

    args = parser.parse_args()

    pipeline_path: Path = args.pipeline

    try:
        pipeline = load_pipeline(pipeline_path)

    except PipelineParseError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    step_count_before = len(pipeline.steps)
    optimized = not args.no_optimize

    if optimized:
        pipeline = optimize_pipeline(pipeline)

    step_count_after = len(pipeline.steps)

    if args.dry_run:
        _print_pipeline_summary(pipeline_path, optimized, step_count_before, step_count_after)
        print()
        for step in pipeline.steps:
            if isinstance(step, MenaiStep):
                print(f"  menai    {step.step_id}")
                if step.module:
                    print(f"             module: {step.module}")
                for name, src in step.inputs.items():
                    print(f"             input:  {name} <- {src}")
                for key, target in step.outputs.items():
                    print(f"             output: {key} -> {target}")
            elif isinstance(step, ToolStep):
                print(f"  {step.tool:<8} {step.step_id}  ({step.operation})")
                if step.value_from:
                    print(f"             value_from: {step.value_from}")

        print()
        print("Pipeline is valid.")
        return 0

    _print_pipeline_summary(pipeline_path, optimized, step_count_before, step_count_after)
    print()

    profile_output = None
    start = time.monotonic()

    if args.profile:
        result, profile_output = _run_with_profile(pipeline, args.profile_sort, args.profile_lines)
    else:
        result = execute_pipeline(pipeline)

    elapsed = time.monotonic() - start

    _print_step_summary(result, args.verbose, args.timings)

    if args.timings and result.step_results:
        print()
        _print_timings_bar(result)

    if profile_output:
        print()
        print(f"  Profile (top {args.profile_lines} by {args.profile_sort}):")
        for line in profile_output.splitlines():
            print(f"  {line}")

    print()

    if result.success:
        print(f"Pipeline completed successfully in {_format_elapsed(elapsed)}")
        return 0

    print(f"Pipeline failed after {_format_elapsed(elapsed)}: {result.error}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
