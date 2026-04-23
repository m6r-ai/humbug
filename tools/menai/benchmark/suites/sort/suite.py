from __future__ import annotations

import random
from pathlib import Path
from typing import Any

from benchmark import BenchmarkCase, BenchmarkSuite, Implementation
from menai import Menai

_SIZES = [10, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
_ITERATIONS = 5
_SUITE_DIR = Path(__file__).resolve().parent

_rng = random.Random(42)
_INPUTS: dict[int, list[int]] = {
    size: _rng.sample(range(size * 10), size) for size in _SIZES
}


def _merge_sort(lst: list[int]) -> list[int]:
    """Return a new sorted list using a pure-functional recursive merge sort."""
    if len(lst) <= 1:
        return lst
    mid = len(lst) // 2
    left = _merge_sort(lst[:mid])
    right = _merge_sort(lst[mid:])
    return _merge(left, right)


def _merge(left: list[int], right: list[int]) -> list[int]:
    """Merge two sorted lists into a single sorted list without mutation."""
    result: list[int] = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    result.extend(left[i:])
    result.extend(right[j:])
    return result


class Suite(BenchmarkSuite):
    """Benchmark suite comparing Menai, idiomatic Python, and functional Python sort."""

    name = "sort"
    description = "Sort a list of random integers at various sizes."

    def cases(self) -> list[BenchmarkCase]:
        """Return one case per input size."""
        return [
            BenchmarkCase(
                name=f"n={size}",
                input=_INPUTS[size],
                iterations=_ITERATIONS,
            )
            for size in _SIZES
        ]

    def implementations(self, menai: Menai) -> list[Implementation]:
        """Return Menai, idiomatic Python, and functional Python implementations."""
        sort_expr = (_SUITE_DIR / "list-sort.menai").read_text(encoding="utf-8").strip()

        def prepare_menai(lst: list[int]) -> Any:
            """Build the expression string and compile to bytecode (untimed)."""
            items = " ".join(str(n) for n in lst)
            expr = f"({sort_expr} (list {items}))"
            return menai.compile(expr)

        def run_menai(code: Any) -> Any:
            """Execute pre-compiled bytecode (timed)."""
            return menai.execute_raw(code)

        def run_python_idiomatic(lst: list[int]) -> list[int]:
            """Sort using Python's built-in sorted()."""
            return sorted(lst)

        def run_python_functional(lst: list[int]) -> list[int]:
            """Sort using a pure-functional recursive merge sort."""
            return _merge_sort(lst)

        return [
            Implementation(name="Menai", run=run_menai, prepare=prepare_menai),
            Implementation(name="Python (idiomatic)", run=run_python_idiomatic),
            Implementation(name="Python (functional)", run=run_python_functional),
        ]

    def results_equal(self, a: Any, b: Any) -> bool:
        """Return True if both results are equal sorted lists."""
        return a == b
