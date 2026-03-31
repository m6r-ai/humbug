from __future__ import annotations

import time
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable

import sys

_REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
sys.path.insert(0, str(_REPO_ROOT / "src"))

from menai import Menai


@dataclass
class BenchmarkCase:
    """A single parameterised scenario to benchmark across all implementations."""

    name: str
    input: Any
    iterations: int


@dataclass
class Implementation:
    """A named callable that can be timed against a BenchmarkCase."""

    name: str
    run: Callable


@dataclass
class CaseResult:
    """Timing and validation outcome for one implementation on one case."""

    case: BenchmarkCase
    impl_name: str
    mean_s: float
    min_s: float
    valid: bool
    error: str | None


class BenchmarkSuite(ABC):
    """Abstract base class for a family of related benchmarks.

    Subclasses declare the cases to run, the implementations to compare, and
    the equality predicate used to validate results against the reference
    implementation.
    """

    name: str
    description: str

    @abstractmethod
    def cases(self) -> list[BenchmarkCase]:
        """Return the list of cases that every implementation will be run against."""

    @abstractmethod
    def implementations(self, menai: Menai) -> list[Implementation]:
        """Return the ordered list of implementations to benchmark.

        The first entry is treated as the reference; all others are validated
        against it.  The supplied *menai* instance is already warmed up.
        """

    @abstractmethod
    def results_equal(self, a: Any, b: Any) -> bool:
        """Return True if two results should be considered equivalent."""


class BenchmarkRunner:
    """Runs a BenchmarkSuite and collects CaseResult objects.

    The caller is responsible for warming up the Menai instance before
    passing it in.  No additional warmup is performed here.
    """

    def __init__(self, suite: BenchmarkSuite, menai: Menai) -> None:
        """Initialise the runner with a suite and a warmed-up Menai instance."""
        self._suite = suite
        self._menai = menai

    def run(self) -> list[CaseResult]:
        """Execute every (case, implementation) combination and return the results."""
        suite = self._suite
        impls = suite.implementations(self._menai)
        results: list[CaseResult] = []

        for case in suite.cases():
            reference_result: Any = None
            reference_set = False

            for idx, impl in enumerate(impls):
                times: list[float] = []
                result: Any = None
                error: str | None = None

                try:
                    for _ in range(case.iterations):
                        t0 = time.perf_counter()
                        result = impl.run(case.input)
                        t1 = time.perf_counter()
                        times.append(t1 - t0)
                except Exception as exc:
                    error = str(exc)

                if error is not None:
                    mean_s = 0.0
                    min_s = 0.0
                    valid = False
                else:
                    mean_s = sum(times) / len(times)
                    min_s = min(times)
                    if idx == 0:
                        reference_result = result
                        reference_set = True
                        valid = True
                    else:
                        valid = reference_set and suite.results_equal(reference_result, result)

                results.append(
                    CaseResult(
                        case=case,
                        impl_name=impl.name,
                        mean_s=mean_s,
                        min_s=min_s,
                        valid=valid,
                        error=error,
                    )
                )

        return results


class BenchmarkReporter:
    """Formats and prints a comparison table for a completed benchmark run."""

    _SEPARATOR = "─" * 120
    _MS = 1_000.0
    _COL_CASE = 24
    _COL_MEAN = 8
    _COL_MIN = 8
    _COL_VS = 13

    def report(
        self,
        suite_name: str,
        results: list[CaseResult],
        implementations: list[Implementation],
    ) -> None:
        """Print a formatted comparison table to stdout.

        The first implementation is the reference.  Subsequent implementations
        show a "vs ref" speedup/slowdown ratio and a validity marker.
        """
        if not implementations:
            return

        impl_names = [i.name for i in implementations]
        cases: list[BenchmarkCase] = []
        seen: set[str] = set()
        for result in results:
            if result.case.name not in seen:
                cases.append(result.case)
                seen.add(result.case.name)

        by_key: dict[tuple[str, str], CaseResult] = {
            (result.case.name, result.impl_name): result for result in results
        }

        print()
        print(suite_name.upper())
        print(self._SEPARATOR)

        header1_parts = [f"{'Case':<{self._COL_CASE}}"]
        header2_parts = [" " * self._COL_CASE]
        for idx, name in enumerate(impl_names):
            col_width = self._COL_MEAN + self._COL_MIN + 3
            if idx > 0:
                col_width += self._COL_VS + 2
            header1_parts.append(f"{name:<{col_width}}")
            sub = f"{'mean (ms)':>{self._COL_MEAN}}  {'min (ms)':<{self._COL_MIN}}"
            if idx > 0:
                sub += f"  {'vs ref':<{self._COL_VS}}"
            header2_parts.append(sub)

        print("  ".join(header1_parts))
        print("  ".join(header2_parts))
        print(self._SEPARATOR)

        validation_counts: dict[str, int] = {n: 0 for n in impl_names}

        for case in cases:
            ref_result = by_key.get((case.name, impl_names[0]))
            ref_mean = ref_result.mean_s if ref_result and not ref_result.error else None

            row_parts = [f"{case.name:<{self._COL_CASE}}"]

            for idx, name in enumerate(impl_names):
                r: CaseResult | None = by_key.get((case.name, name))
                if r is None:
                    cell = f"{'N/A':>{self._COL_MEAN}}  {'N/A':<{self._COL_MIN}}"
                    if idx > 0:
                        cell += f"  {'':>{self._COL_VS}}"
                    row_parts.append(cell)
                    continue

                if r.error is not None:
                    mean_str = "ERROR"
                    min_str = ""
                else:
                    mean_str = f"{r.mean_s * self._MS:.3f}"
                    min_str = f"{r.min_s * self._MS:.3f}"
                    if r.valid:
                        validation_counts[name] += 1

                validity = "✓" if r.valid else "✗"
                cell = f"{mean_str:>{self._COL_MEAN}}  {min_str:<{self._COL_MIN}}"

                if idx == 0:
                    cell += f" {validity}"
                else:
                    vs_str = self._vs_ref(ref_mean, r.mean_s if not r.error else None)
                    cell += f"  {vs_str + ' ' + validity:<{self._COL_VS}}"

                row_parts.append(cell)

            print("  ".join(row_parts))

        print(self._SEPARATOR)

        summary_parts: list[str] = []
        for name in impl_names:
            count = validation_counts[name]
            total = len(cases)
            marker = "✓" if count == total else "✗"
            summary_parts.append(f"{name} {count}/{total} {marker}")
        print("Validation: " + "  |  ".join(summary_parts))
        print()

    def _vs_ref(self, ref_mean_s: float | None, impl_mean_s: float | None) -> str:
        """Return a human-readable speedup/slowdown string relative to the reference.

        Returns an empty string when either value is unavailable.
        """
        if ref_mean_s is None or impl_mean_s is None or impl_mean_s == 0.0:
            return ""
        if ref_mean_s == 0.0:
            return ""
        ratio = ref_mean_s / impl_mean_s
        if ratio >= 1.0:
            return f"{ratio:.0f}x faster"
        else:
            return f"{1.0 / ratio:.0f}x slower"
