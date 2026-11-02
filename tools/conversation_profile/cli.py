"""Command line entry point for the conversation view profiler."""

import argparse
import cProfile
import os
import pstats
import shutil
import tempfile

from tools.conversation_profile.bench import Phase, ProfilingApplication, run, write_trimmed_transcript

_TOP_EVENTS = 6


def _parse_args() -> argparse.Namespace:
    """Parse the command line arguments."""
    parser = argparse.ArgumentParser(
        prog="python -m tools.conversation_profile",
        description="Measure where a conversation view spends its time.",
    )
    parser.add_argument("transcript", help="path to a .conv transcript to display")
    parser.add_argument(
        "--messages", type=int, default=None,
        help="display only the most recent N messages of the transcript",
    )
    parser.add_argument(
        "--resize-steps", type=int, default=10,
        help="number of width changes to measure (default: 10)",
    )
    parser.add_argument(
        "--scroll-steps", type=int, default=50,
        help="number of scrollbar positions to measure (default: 50)",
    )
    parser.add_argument(
        "--profile", action="store_true",
        help="also run the benchmark under cProfile and print the hottest Python frames",
    )
    return parser.parse_args()


def _prepare_transcript(source_path: str, work_dir: str, limit: int | None) -> str:
    """Copy the transcript into the work directory, trimming it if a limit is given."""
    destination_path = os.path.join(work_dir, "transcript.conv")
    if limit is None:
        shutil.copyfile(source_path, destination_path)

    else:
        write_trimmed_transcript(source_path, destination_path, limit)

    return destination_path


def _report(phases: list[Phase]) -> None:
    """Print the measured cost of each phase and the events it is made of."""
    for phase in phases:
        print(f"{phase.name}: {phase.seconds * 1000:.1f} ms  ({phase.detail})")
        for name, count, seconds in phase.top_events(_TOP_EVENTS):
            share = 100.0 * seconds / phase.seconds if phase.seconds else 0.0
            print(f"    {name:<24}{seconds * 1000:9.1f} ms  {count:8d} events  {share:5.1f}%")

        print()

    total = sum(phase.seconds for phase in phases)
    print(f"total: {total * 1000:.1f} ms")


def main() -> int:
    """Run the profiler."""
    args = _parse_args()

    # The benchmark must run without a display and without the user's own
    # mindspace, so that a run is repeatable and leaves no trace behind.
    os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

    work_dir = tempfile.mkdtemp(prefix="humbug-conversation-profile-")
    try:
        app = ProfilingApplication([])
        transcript_path = _prepare_transcript(args.transcript, work_dir, args.messages)

        def benchmark() -> None:
            phases = run(app, transcript_path, args.resize_steps, args.scroll_steps)
            _report(phases)

        if args.profile:
            profiler = cProfile.Profile()
            profiler.runcall(benchmark)
            pstats.Stats(profiler).sort_stats("cumulative").print_stats(40)

        else:
            benchmark()

    finally:
        shutil.rmtree(work_dir, ignore_errors=True)

    return 0
