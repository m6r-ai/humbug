"""
Core logic for the code checker tool.
"""

import subprocess
import sys
from dataclasses import dataclass


@dataclass
class CheckResult:
    """Result of running a single checker."""

    label: str
    exit_code: int


@dataclass
class Check:
    """Definition of a single checker to run."""

    label: str
    argv: list[str]


def _run_check(check: Check) -> CheckResult:
    """Run a single check, streaming its output, and return the result."""
    width = 72
    header = f" Running: {check.label} "
    print("\n" + header.center(width, "━"))

    result = subprocess.run(check.argv, check=False)

    return CheckResult(label=check.label, exit_code=result.returncode)


def _print_summary(results: list[CheckResult]) -> None:
    """Print a summary table of all check results."""
    width = 72
    failures = sum(1 for r in results if r.exit_code != 0)
    label_width = max(len(r.label) for r in results)

    print("\n" + "Results".center(width))
    print("─" * width)
    for result in results:
        status = "✓ passed" if result.exit_code == 0 else "✗ FAILED"
        print(f"  {result.label:<{label_width}}  {status}")

    print("─" * width)

    if failures == 0:
        print("All checks passed.")

    elif failures == 1:
        print("1 check failed.")

    else:
        print(f"{failures} of {len(results)} checks failed.")


def run_all_checks(checks: list[Check]) -> int:
    """Run all checks, print a summary, and return an overall exit code."""
    results = [_run_check(check) for check in checks]
    _print_summary(results)
    return 0 if all(r.exit_code == 0 for r in results) else 1


CHECKS: list[Check] = [
    Check(
        label="dependency checker",
        argv=[sys.executable, "-m", "tools.dependency_checker", "check"],
    ),
    Check(
        label="mypy",
        argv=[sys.executable, "-m", "mypy", "src", "tools"],
    ),
    Check(
        label="pylint",
        argv=[sys.executable, "-m", "pylint", "src"],
    ),
]
