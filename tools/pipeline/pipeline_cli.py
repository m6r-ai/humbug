#!/usr/bin/env python3
"""Command-line interface for the pipeline engine."""

import argparse
import cProfile
import io
import pstats
import sys
import time
from pathlib import Path
from typing import Callable, Optional, Tuple

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from pipeline_engine import PipelineResult, StepResult, execute_pipeline
from pipeline_optimizer import optimize_pipeline
from pipeline_parser import PipelineParseError, load_pipeline
from pipeline_step import MenaiStep, Pipeline, ToolStep


_ANSI_CYAN  = "\033[36m"
_ANSI_GREEN = "\033[32m"
_ANSI_RED   = "\033[31m"
_ANSI_GREY  = "\033[90m"
_ANSI_RESET = "\033[0m"


def _use_color(no_color: bool) -> bool:
    """Return True if ANSI colour output should be used."""
    return not no_color and sys.stdout.isatty()


def _format_elapsed(seconds: float) -> str:
    """Format an elapsed time value for display."""
    if seconds >= 1.0:
        return f"{seconds:.3f}s"

    return f"{seconds * 1000:.1f}ms"


def _make_step_callbacks(
    pipeline: Pipeline,
    verbosity: int,
    timings: bool,
    color: bool,
) -> Tuple[Callable[[str], None], Callable[[StepResult], None], Callable[[], None]]:
    """
    Build live step callbacks for verbose pipeline execution.

    Returns a triple of (on_step_start, on_step_done, print_final_block).

    on_step_start prints a separator followed by the full status block: all previously
    completed steps with their final status, and the current step marked as Running.
    on_step_done records the completed result so it appears in the next status block,
    and at verbosity >= 2 appends the truncated debug value beneath the status block.
    print_final_block prints the closing separator and final status block after the
    last step completes.
    """
    step_ids = [s.step_id for s in pipeline.steps]
    num_width = len(str(len(step_ids)))
    id_width = max((len(s) for s in step_ids), default=8)
    _dashes = "-" * (num_width + 2 + id_width + 20)
    separator = f"{_ANSI_GREY}{_dashes}{_ANSI_RESET}" if color else _dashes

    step_index: dict[str, int] = {sid: i for i, sid in enumerate(step_ids)}
    console_step_ids: set[str] = {
        s.step_id for s in pipeline.steps
        if isinstance(s, ToolStep) and s.tool == "console"
    }
    pending: list[Optional[StepResult]] = [None]

    def _flush_pending() -> None:
        prev = pending[0]
        if prev is None:
            return

        i = step_index[prev.step_id]
        if prev.success:
            status = f"{_ANSI_GREEN}OK{_ANSI_RESET}" if color else "OK"

        else:
            status = f"{_ANSI_RED}FAIL{_ANSI_RESET}" if color else "FAIL"

        timing_str = f"  {_format_elapsed(prev.elapsed_s):>8}" if timings else ""

        if not prev.success:
            error_text = f"{_ANSI_RED}{prev.error}{_ANSI_RESET}" if color else prev.error
            print(error_text)
            print(separator)
        elif verbosity >= 2 and prev.value:
            if prev.step_id in console_step_ids:
                print(separator)
            truncated = prev.value
            if len(truncated) > 120:
                truncated = truncated[:117] + "..."
            value_text = f"{_ANSI_GREEN}{truncated}{_ANSI_RESET}" if color else truncated
            print(value_text)
            print(separator)
        elif prev.step_id in console_step_ids:
            print(separator)

        print(f"{i + 1:{num_width}}. {prev.step_id} <- {status}{timing_str}")

        pending[0] = None

    def on_step_start(step_id: str) -> None:
        _flush_pending()
        print(separator)
        i = step_index[step_id]
        running = f"{_ANSI_CYAN}Running{_ANSI_RESET}" if color else "Running"
        print(f"{i + 1:{num_width}}. {step_id} <- {running}")
        print(separator)

    def on_step_done(step_result: StepResult) -> None:
        pending[0] = step_result

    def print_final_block() -> None:
        _flush_pending()
        print(separator)

    return on_step_start, on_step_done, print_final_block


def _print_timings_bar(result: PipelineResult) -> None:
    """Print a simple proportional timing bar for each step."""
    total = sum(r.elapsed_s for r in result.step_results)
    if total == 0:
        return

    bar_width = 40
    id_width = max((len(r.step_id) for r in result.step_results), default=8)

    print("Timing breakdown:")
    for step_result in result.step_results:
        proportion = step_result.elapsed_s / total
        filled = round(proportion * bar_width)
        text_bar = "█" * filled + "░" * (bar_width - filled)
        pct = proportion * 100
        print(
            f"  {step_result.step_id:<{id_width}}  [{text_bar}]  "
            f"{_format_elapsed(step_result.elapsed_s):>8}  ({pct:4.1f}%)"
        )


def _print_pipeline_summary(
    pipeline_path: Path,
    optimized: bool,
    step_count_before: int,
    step_count_after: int
) -> None:
    """Print a summary of the pipeline structure."""
    print(f"Pipeline: {pipeline_path}")
    print(f"Steps: {step_count_before}", end="")
    if optimized and step_count_after < step_count_before:
        collapsed = step_count_before - step_count_after
        print(f" -> {step_count_after} ({collapsed} Menai step(s) collapsed)", end="")

    print()


def _run_with_profile(
    pipeline: Pipeline,
    sort_key: str,
    lines: int,
    on_step_start: Optional[Callable[[str], None]],
    on_step_done: Optional[Callable[[StepResult], None]],
) -> Tuple[PipelineResult, str]:
    """
    Execute a pipeline under cProfile and return (result, profile_stats_string).

    Args:
        pipeline: The pipeline to execute
        sort_key: pstats sort key (e.g. 'cumulative', 'tottime')
        lines: Number of functions to show in the profile output
        on_step_start: Optional step-start callback forwarded to execute_pipeline
        on_step_done: Optional step-done callback forwarded to execute_pipeline

    Returns:
        Tuple of (PipelineResult, formatted profile string)
    """
    profiler = cProfile.Profile()
    profiler.enable()
    result = execute_pipeline(pipeline, on_step_start=on_step_start, on_step_done=on_step_done)
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
  python pipeline_cli.py -v examples/file-transform/pipeline.json
  python pipeline_cli.py -v --timings examples/clock-and-file/pipeline.json
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
        "-v", "--verbose",
        action="count",
        default=0,
        dest="verbosity",
        help="Increase verbosity: -v shows live step status, -vv also shows truncated step output"
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

    parser.add_argument(
        "--no-color",
        action="store_true",
        help="Disable ANSI colour output"
    )

    args = parser.parse_args()

    color = _use_color(args.no_color)

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

    if args.verbosity >= 1:
        _print_pipeline_summary(pipeline_path, optimized, step_count_before, step_count_after)
        print()

    profile_output = None
    start = time.monotonic()

    on_step_start = None
    on_step_done = None
    print_final_block = None

    if args.verbosity >= 1:
        on_step_start, on_step_done, print_final_block = _make_step_callbacks(
            pipeline, args.verbosity, args.timings, color
        )

    if args.profile:
        result, profile_output = _run_with_profile(
            pipeline, args.profile_sort, args.profile_lines, on_step_start, on_step_done
        )
    else:
        result = execute_pipeline(pipeline, on_step_start=on_step_start, on_step_done=on_step_done)

    elapsed = time.monotonic() - start

    if print_final_block is not None:
        print_final_block()

    if args.timings and result.step_results:
        print()
        _print_timings_bar(result)

    if profile_output:
        print()
        print(f"Profile (top {args.profile_lines} by {args.profile_sort}):")
        for line in profile_output.splitlines():
            print(line)

    if result.success:
        if args.verbosity >= 1:
            print()
            print(f"Pipeline completed successfully in {_format_elapsed(elapsed)}")
        return 0

    print(f"Pipeline failed: {result.error}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
