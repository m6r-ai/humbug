#!/usr/bin/env python3
from __future__ import annotations

import argparse
import importlib.util
import sys
from pathlib import Path

_REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
sys.path.insert(0, str(_REPO_ROOT / "src"))

_BENCHMARK_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(_BENCHMARK_DIR))

from menai import Menai  # noqa: E402  pylint: disable=wrong-import-position

from benchmark import (  # noqa: E402  pylint: disable=wrong-import-position
    BenchmarkReporter,
    BenchmarkRunner,
    BenchmarkSuite,
)

_SUITES_DIR = _BENCHMARK_DIR / "suites"
_MENAI_MODULES_DIR = _REPO_ROOT / "menai_modules"


def discover_suites() -> list[tuple[Path, type[BenchmarkSuite]]]:
    """Discover all suite classes by scanning suites/*/suite.py.

    Each suite module must contain a class named ``Suite`` that subclasses
    ``BenchmarkSuite``.  Suites are returned in alphabetical order by
    directory name.

    Returns:
        A list of (suite_directory, Suite class) pairs.
    """
    found: list[tuple[Path, type[BenchmarkSuite]]] = []

    for suite_file in sorted(_SUITES_DIR.glob("*/suite.py")):
        suite_dir = suite_file.parent
        module_name = f"suite_{suite_dir.name}"

        spec = importlib.util.spec_from_file_location(module_name, suite_file)
        if spec is None or spec.loader is None:
            print(
                f"Warning: could not load spec for {suite_file}, skipping.",
                file=sys.stderr,
            )
            continue

        module = importlib.util.module_from_spec(spec)
        try:
            spec.loader.exec_module(module)  # type: ignore[union-attr]
        except Exception as exc:
            print(
                f"Warning: error importing {suite_file}: {exc}, skipping.",
                file=sys.stderr,
            )
            continue

        suite_class = getattr(module, "Suite", None)
        if suite_class is None:
            print(
                f"Warning: {suite_file} has no 'Suite' class, skipping.",
                file=sys.stderr,
            )
            continue

        found.append((suite_dir, suite_class))

    return found


def filter_suites(
    all_suites: list[tuple[Path, type[BenchmarkSuite]]],
    names: list[str],
) -> list[tuple[Path, type[BenchmarkSuite]]]:
    """Return only the suites whose name contains any of the given substrings.

    Matching is case-insensitive.  A suite's name is taken from its
    ``BenchmarkSuite.name`` class attribute.

    Args:
        all_suites: The full list of discovered (directory, class) pairs.
        names:      Substrings to match against suite names.

    Returns:
        The filtered subset, preserving discovery order.
    """
    lowered = [n.lower() for n in names]
    return [
        (suite_dir, cls)
        for suite_dir, cls in all_suites
        if any(fragment in cls.name.lower() for fragment in lowered)
    ]


def build_parser() -> argparse.ArgumentParser:
    """Build and return the argument parser for the benchmark runner."""
    parser = argparse.ArgumentParser(
        description="Run Menai benchmark suites and report timing results.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "python run.py                        # run all suites\n"
            "python run.py --suite sort           # run only the sort suite\n"
            "python run.py --suite sort sudoku    # run multiple\n"
            "python run.py --iterations 5         # override iteration count\n"
            "python run.py --no-validate          # skip correctness checks"
        ),
    )
    parser.add_argument(
        "--suite",
        metavar="NAME",
        dest="suites",
        nargs="+",
        action="append",
        default=None,
        help=(
            "Run only suites whose name contains NAME (case-insensitive substring "
            "match).  May be repeated or given multiple values.  Omit to run all."
        ),
    )
    parser.add_argument(
        "--iterations",
        metavar="N",
        type=int,
        default=None,
        help="Override the iteration count on every BenchmarkCase.",
    )
    parser.add_argument(
        "--no-validate",
        action="store_true",
        dest="no_validate",
        help=(
            "Skip result validation: mark all results as valid and suppress "
            "results_equal calls.  Useful when you only care about timing."
        ),
    )
    return parser


def run_suite(
    suite_dir: Path,
    suite_class: type[BenchmarkSuite],
    iterations: int | None,
    no_validate: bool,
) -> None:
    """Instantiate, run, and report a single benchmark suite.

    Args:
        suite_dir:    Directory containing the suite's ``suite.py`` (and any
                      ``.menai`` files it imports).
        suite_class:  The ``Suite`` subclass to instantiate.
        iterations:   If given, override ``BenchmarkCase.iterations`` on every
                      case before running.
        no_validate:  If ``True``, patch all ``CaseResult`` objects so that
                      ``valid=True`` and ``error=None`` before reporting.
    """
    suite = suite_class()

    if iterations is not None:
        for case in suite.cases():
            case.iterations = iterations

    module_path = [str(suite_dir), str(_MENAI_MODULES_DIR)]
    menai = Menai(module_path=module_path)

    runner = BenchmarkRunner(suite, menai)
    results = runner.run()

    if no_validate:
        for result in results:
            result.valid = True
            result.error = None

    reporter = BenchmarkReporter()
    reporter.report(suite.name, results, suite.implementations(menai))


def main() -> None:
    """Entry point for the benchmark CLI."""
    parser = build_parser()
    args = parser.parse_args()

    all_suites = discover_suites()

    if not all_suites:
        print("No suites found under suites/*/suite.py.", file=sys.stderr)
        sys.exit(1)

    if args.suites is not None:
        flat_names = [name for group in args.suites for name in group]
        selected = filter_suites(all_suites, flat_names)
        if not selected:
            joined = ", ".join(flat_names)
            print(
                f"No suites matched the filter(s): {joined}",
                file=sys.stderr,
            )
            sys.exit(1)
    else:
        selected = all_suites

    for suite_dir, suite_class in selected:
        run_suite(
            suite_dir=suite_dir,
            suite_class=suite_class,
            iterations=args.iterations,
            no_validate=args.no_validate,
        )

    if len(selected) > 1:
        print(f"\n{len(selected)} suite(s) completed.")


if __name__ == "__main__":
    main()
