from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Any
from typing import cast

from benchmark import BenchmarkCase, BenchmarkSuite, Implementation
from menai import Menai

_SUITE_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(_SUITE_DIR))

from json_parser import parse as _parse_functional  # noqa: E402

_LONG_STRING = '"' + ("abcdefghij" * 200) + '"'
_DEEP_ARRAY = ("[" * 500) + "0" + ("]" * 500)

_CASES: list[tuple[str, str, int]] = [
    ("object",       '{"name": "Alice", "age": 30, "active": true, "score": 9.5, "tags": ["admin", "user"], "address": {"city": "Wonderland", "zip": null}}', 10),
    ("integer",      "42",                    10),
    ("float",        "-3.14",                 10),
    ("true",         "true",                  10),
    ("false",        "false",                 10),
    ("null",         "null",                  10),
    ("string_esc",   '"hello\\nworld"',        10),
    ("empty_array",  "[]",                    10),
    ("empty_object", "{}",                    10),
    ("nested",       "[1, [2, [3]]]",         10),
    ("long_string",  _LONG_STRING,            10),
    ("deep_array",   _DEEP_ARRAY,             10),
]


def _to_menai_expr(json_str: str) -> str:
    """Wrap a JSON string in a Menai parse call, escaping for Menai string syntax."""
    escaped = json_str.replace("\\", "\\\\").replace('"', '\\"')
    return f'(let ((json (import "json_parser"))) ((dict-get json "parse") "{escaped}"))'


class Suite(BenchmarkSuite):
    """Benchmark suite comparing Menai, idiomatic Python, and functional Python JSON parsers."""

    name = "json_parser"
    description = "Parse JSON strings of varying structure and size."

    def cases(self) -> list[BenchmarkCase]:
        """Return one case per JSON input."""
        return [
            BenchmarkCase(name=name, input=json_str, iterations=iters)
            for name, json_str, iters in _CASES
        ]

    def implementations(self, menai: Menai) -> list[Implementation]:
        """Return Menai, idiomatic Python, and functional Python parser implementations."""
        def run_menai(json_str: str) -> Any:
            """Parse using the Menai json_parser module."""
            return menai.evaluate(_to_menai_expr(json_str))

        def run_python_idiomatic(json_str: str) -> Any:
            """Parse using Python's stdlib json.loads()."""
            return json.loads(json_str)

        def run_python_functional(json_str: str) -> Any:
            """Parse using the pure-functional explicit-stack Python parser."""
            return _parse_functional(json_str)

        return [
            Implementation(name="Menai",               run=run_menai),
            Implementation(name="Python (idiomatic)",  run=run_python_idiomatic),
            Implementation(name="Python (functional)", run=run_python_functional),
        ]

    def results_equal(self, a: Any, b: Any) -> bool:
        """Return True if both results are equal parsed values."""
        return a == b
