#!/usr/bin/env python3
"""Menai test runner — discovers and executes *_test.menai test suites."""

import argparse
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

# Resolve src/ relative to this file so the runner works from any working directory
_REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
sys.path.insert(0, str(_REPO_ROOT / "src"))

from menai import Menai, MenaiError  # noqa: E402  pylint: disable=wrong-import-position
from menai.menai_value import MenaiDict, MenaiFunction, MenaiList, MenaiString  # noqa: E402  pylint: disable=wrong-import-position


# Directory containing menai_test.menai — always prepended to the module path
_RUNNER_DIR = str(Path(__file__).resolve().parent)

# Standard module library directory
_MENAI_MODULES_DIR = str(_REPO_ROOT / "menai_modules")


@dataclass
class TestResult:
    """Outcome of a single leaf test."""
    path: list[str]
    passed: bool
    error: Optional[str] = None


@dataclass
class RunStats:
    """Accumulated pass/fail counts."""
    passed: int = 0
    failed: int = 0

    @property
    def total(self) -> int:
        return self.passed + self.failed

    def add(self, other: "RunStats") -> None:
        self.passed += other.passed
        self.failed += other.failed


@dataclass
class NodeTree:
    """Parsed representation of a test node-list."""
    name: str
    thunk_path: Optional[list[str]] = None   # set for leaves
    children: list["NodeTree"] = field(default_factory=list)  # set for branches

    @property
    def is_leaf(self) -> bool:
        return self.thunk_path is not None


def _make_menai(test_file_dir: str) -> Menai:
    """Create a Menai instance with the runner's support module on the path."""
    return Menai(module_path=[_RUNNER_DIR, test_file_dir, _MENAI_MODULES_DIR])


def _parse_node_list(value: MenaiList, path: list[str]) -> list[NodeTree]:
    """
    Recursively parse a Menai node-list value into NodeTree objects.

    Each element must be a two-element list: (name thing) where thing is
    either a MenaiFunction (leaf) or a MenaiList (branch).

    Raises:
        ValueError: If the structure does not match the expected format.
    """
    nodes = []
    for element in value.elements:
        if not isinstance(element, MenaiList) or len(element.elements) != 2:
            raise ValueError(
                f"Each node must be a 2-element list (name, thing), "
                f"got: {element.describe()}"
            )

        name_val, thing_val = element.elements

        if not isinstance(name_val, MenaiString):
            raise ValueError(
                f"Node name must be a string, got: {name_val.describe()}"
            )

        name = name_val.value
        node_path = path + [name]

        if isinstance(thing_val, MenaiFunction):
            nodes.append(NodeTree(name=name, thunk_path=node_path))
        elif isinstance(thing_val, MenaiList):
            children = _parse_node_list(thing_val, node_path)
            nodes.append(NodeTree(name=name, children=children))
        else:
            raise ValueError(
                f"Node '{name}': second element must be a function (leaf) "
                f"or list (branch), got: {thing_val.type_name()}"
            )

    return nodes


def _load_test_module(menai: Menai, module_name: str) -> list[NodeTree]:
    """
    Evaluate a test module and return its parsed node tree.

    Raises:
        MenaiError: If the module fails to evaluate.
        ValueError: If the module structure is invalid.
    """
    result = menai._evaluate_raw(f'(import "{module_name}")')

    if not isinstance(result, MenaiDict):
        raise ValueError(f"Test module must export a dict, got: {result.type_name()}")

    tests_entry = result.lookup.get(MenaiDict.to_hashable_key(MenaiString("tests")))
    tests_val = tests_entry[1] if tests_entry is not None else None
    if tests_val is None:
        raise ValueError('Test module dict must have a "tests" key')

    if not isinstance(tests_val, MenaiList):
        raise ValueError(
            f'"tests" value must be a list of nodes, got: {tests_val.type_name()}'
        )

    return _parse_node_list(tests_val, [])


def _menai_path_literal(path: list[str]) -> str:
    """Format a Python list of strings as a Menai list literal."""
    quoted = " ".join(f'"{segment}"' for segment in path)
    return f"(list {quoted})"


def _run_leaf(
    module_name: str,
    test_file_dir: str,
    path: list[str],
) -> TestResult:
    """
    Execute a single leaf thunk in a fresh Menai VM.

    The expression evaluated is:
        ((test-find (import "module") (list "seg1" "seg2" ...)))

    A clean return (any value) is a pass. Any MenaiError is a failure.
    """
    path_literal = _menai_path_literal(path)
    expression = (
        f'(let ((t (import "menai_test")))'
        f'  (let ((thunk ((dict-get t "test-find") (import "{module_name}") {path_literal})))'
        f'    (thunk)))'
    )

    menai = _make_menai(test_file_dir)
    try:
        menai._evaluate_raw(expression)
        return TestResult(path=path, passed=True)
    except MenaiError as exc:
        return TestResult(path=path, passed=False, error=str(exc))


def _run_tree(
    nodes: list[NodeTree],
    module_name: str,
    test_file_dir: str,
    name_filter: Optional[str],
    results: list[TestResult],
) -> None:
    """Recursively walk the node tree, executing all matching leaves."""
    for node in nodes:
        if node.is_leaf:
            assert node.thunk_path is not None
            if name_filter and name_filter.lower() not in " > ".join(node.thunk_path).lower():
                continue
            result = _run_leaf(module_name, test_file_dir, node.thunk_path)
            results.append(result)
        else:
            _run_tree(node.children, module_name, test_file_dir, name_filter, results)


def _print_results(
    results: list[TestResult],
    module_name: str,
    verbose: bool,
) -> RunStats:
    """Print per-test results and return summary stats."""
    stats = RunStats()

    for result in results:
        path_str = " > ".join(result.path)
        if result.passed:
            stats.passed += 1
            if verbose:
                print(f"  ✓  {path_str}")
        else:
            stats.failed += 1
            print(f"  ✗  {path_str}")
            if result.error:
                # Indent error lines for readability
                for line in result.error.splitlines():
                    print(f"       {line}")

    return stats


def _run_file(
    test_file: Path,
    name_filter: Optional[str],
    verbose: bool,
) -> RunStats:
    """Discover, execute, and report all tests in a single test file."""
    module_name = test_file.stem
    test_file_dir = str(test_file.parent.resolve())

    print(f"\n{test_file}")

    menai = _make_menai(test_file_dir)
    try:
        nodes = _load_test_module(menai, module_name)
    except (MenaiError, ValueError) as exc:
        print(f"  ERROR loading module: {exc}")
        return RunStats(failed=1)

    results: list[TestResult] = []
    _run_tree(nodes, module_name, test_file_dir, name_filter, results)

    if not results:
        print("  (no tests matched)")
        return RunStats()

    stats = _print_results(results, module_name, verbose)
    status = "passed" if stats.failed == 0 else "FAILED"
    print(f"  {stats.passed}/{stats.total} {status}")
    return stats


def _discover_test_files(paths: list[Path]) -> list[Path]:
    """
    Find all *_test.menai files under the given paths.

    Paths may be files or directories. Directories are searched recursively.
    Results are sorted for deterministic ordering.
    """
    found: list[Path] = []
    for path in paths:
        if path.is_file():
            if path.name.endswith("_test.menai"):
                found.append(path)
        elif path.is_dir():
            found.extend(sorted(path.rglob("*_test.menai")))
        else:
            print(f"Warning: path not found: {path}", file=sys.stderr)
    return sorted(set(found))


def main() -> None:
    """Entry point for the Menai test runner."""
    parser = argparse.ArgumentParser(
        description="Menai test runner — executes *_test.menai test suites",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s menai_modules/            # run all tests under menai_modules/
  %(prog)s menai_modules/json_parser_test.menai  # run a single file
  %(prog)s menai_modules/ --filter "parse-string"  # filter by name
  %(prog)s menai_modules/ --verbose  # show passing tests too
""",
    )
    parser.add_argument(
        "paths",
        nargs="+",
        metavar="PATH",
        help="Files or directories to search for *_test.menai files",
    )
    parser.add_argument(
        "--filter",
        metavar="TEXT",
        dest="name_filter",
        default=None,
        help="Only run tests whose full path contains TEXT (case-insensitive)",
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show passing tests as well as failures",
    )

    args = parser.parse_args()

    test_files = _discover_test_files([Path(p) for p in args.paths])

    if not test_files:
        print("No *_test.menai files found.")
        sys.exit(0)

    total_stats = RunStats()
    for test_file in test_files:
        stats = _run_file(test_file, args.name_filter, args.verbose)
        total_stats.add(stats)

    print(f"\n{'='*60}")
    print(f"Total: {total_stats.passed}/{total_stats.total} passed", end="")
    if total_stats.failed:
        print(f", {total_stats.failed} FAILED")
    else:
        print()

    sys.exit(1 if total_stats.failed else 0)


if __name__ == "__main__":
    main()
