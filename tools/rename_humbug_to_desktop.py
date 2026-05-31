#!/usr/bin/env python3
"""Migration script: rename the ``humbug`` GUI package to ``desktop``.

The product is still called "Humbug". This script ONLY renames the Python
import package / module-layout name (``src/humbug`` -> ``src/desktop``) and the
references that point at it as a *module*. It deliberately leaves all of the
following untouched:

  * Branding strings ("system called Humbug", "Humbug v...", HumbugApplication)
  * The PyPI project name (``name = "humbug"`` in pyproject.toml)
  * User-data paths (``~/.humbug/...``)
  * Drag-and-drop MIME types (``application/x-humbug-tab`` etc.)
  * The GitHub project URL

By default the script runs in dry-run mode and only reports what it *would*
do. Pass ``--apply`` to make changes. Pass ``--git`` to use ``git mv`` for the
directory move so history is preserved.
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from dataclasses import dataclass, field
from typing import List, Tuple


# Repository-relative paths.
SRC_OLD_DIR = os.path.join("src", "humbug")
SRC_NEW_DIR = os.path.join("src", "desktop")
DEPENDENCY_RULES = "dependency-rules.yaml"
PYPROJECT = "pyproject.toml"
SPEC_FILE = "humbug.spec"

OLD_MODULE = "humbug"
NEW_MODULE = "desktop"

# Import-statement rewrite rules. These are deliberately anchored to the start
# of a line (allowing leading whitespace) and require ``humbug`` to be the
# module token, immediately followed by ``.`` or import/whitespace. This means
# string literals such as "~/.humbug/logs" or "application/x-humbug-tab" can
# never match, because those occurrences are not import statements.
#
# Each entry is (compiled_regex, replacement).
IMPORT_RULES: List[Tuple[re.Pattern, str]] = [
    # from humbug.X import Y  /  from humbug import Y
    (
        re.compile(r"^(?P<indent>\s*)from\s+humbug(?P<rest>(\.[\w\.]+)?\s+import\b)"),
        r"\g<indent>from " + NEW_MODULE + r"\g<rest>",
    ),
    # import humbug.X  /  import humbug.X as Z  /  import humbug
    (
        re.compile(r"^(?P<indent>\s*)import\s+humbug(?P<rest>(\.[\w\.]+)?(\s+as\s+\w+)?\s*)$"),
        r"\g<indent>import " + NEW_MODULE + r"\g<rest>",
    ),
]


@dataclass
class FileChange:
    """Records the planned edits for a single file."""

    path: str
    edits: List[Tuple[int, str, str]] = field(default_factory=list)  # (lineno, old, new)


def _iter_python_files(root: str) -> List[str]:
    """Return all ``.py`` files under ``root`` (skipping cache dirs)."""
    found: List[str] = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in ("__pycache__", ".mypy_cache")]
        for name in filenames:
            if name.endswith(".py"):
                found.append(os.path.join(dirpath, name))

    return found


def _rewrite_line(line: str) -> str:
    """Apply the import rewrite rules to a single line."""
    for pattern, replacement in IMPORT_RULES:
        new_line, count = pattern.subn(replacement, line)
        if count:
            return new_line

    return line


def _plan_python_edits(scan_root: str) -> List[FileChange]:
    """Scan Python files and plan import-statement rewrites.

    ``scan_root`` is the directory that currently holds the source. We scan the
    *old* directory because that is where the files physically live until the
    move happens. The move and the edits are independent operations.
    """
    changes: List[FileChange] = []
    for path in _iter_python_files(scan_root):
        try:
            with open(path, "r", encoding="utf-8") as handle:
                lines = handle.readlines()

        except (OSError, UnicodeDecodeError) as exc:
            print(f"WARNING: could not read {path}: {exc}", file=sys.stderr)
            continue

        file_change = FileChange(path=path)
        for index, line in enumerate(lines):
            new_line = _rewrite_line(line)
            if new_line != line:
                file_change.edits.append((index + 1, line.rstrip("\n"), new_line.rstrip("\n")))

        if file_change.edits:
            changes.append(file_change)

    return changes


def _apply_python_edits(changes: List[FileChange]) -> None:
    """Apply planned edits in place."""
    for change in changes:
        with open(change.path, "r", encoding="utf-8") as handle:
            lines = handle.readlines()

        for lineno, _old, new in change.edits:
            # Preserve the original line ending.
            original = lines[lineno - 1]
            ending = ""
            if original.endswith("\r\n"):
                ending = "\r\n"
            elif original.endswith("\n"):
                ending = "\n"

            lines[lineno - 1] = new + ending

        with open(change.path, "w", encoding="utf-8") as handle:
            handle.writelines(lines)


def _plan_dependency_rules() -> List[Tuple[int, str, str]]:
    """Plan the single module-key rename in dependency-rules.yaml.

    Only the top-level ``humbug:`` mapping key is renamed. No other module
    lists ``humbug`` as an internal dependency, so no dependents need editing.
    The script verifies that assumption and warns if it is violated.
    """
    if not os.path.exists(DEPENDENCY_RULES):
        print(f"WARNING: {DEPENDENCY_RULES} not found; skipping.", file=sys.stderr)
        return []

    with open(DEPENDENCY_RULES, "r", encoding="utf-8") as handle:
        lines = handle.readlines()

    planned: List[Tuple[int, str, str]] = []
    # Top-level module keys are indented by exactly two spaces under "modules:".
    key_pattern = re.compile(r"^(?P<indent>  )humbug:\s*$")
    dep_pattern = re.compile(r"^\s*-\s*humbug\s*$")

    for index, line in enumerate(lines):
        match = key_pattern.match(line)
        if match:
            planned.append((index + 1, line.rstrip("\n"), f"{match.group('indent')}{NEW_MODULE}:"))
            continue

        if dep_pattern.match(line):
            print(
                f"WARNING: {DEPENDENCY_RULES}:{index + 1} lists 'humbug' as a "
                "dependency; this would also need renaming. Review manually.",
                file=sys.stderr,
            )

    return planned


def _apply_line_edits(path: str, planned: List[Tuple[int, str, str]]) -> None:
    """Apply a list of (lineno, old, new) edits to a single file."""
    with open(path, "r", encoding="utf-8") as handle:
        lines = handle.readlines()

    for lineno, _old, new in planned:
        original = lines[lineno - 1]
        ending = "\r\n" if original.endswith("\r\n") else ("\n" if original.endswith("\n") else "")
        lines[lineno - 1] = new + ending

    with open(path, "w", encoding="utf-8") as handle:
        handle.writelines(lines)


def _plan_pyproject() -> List[Tuple[int, str, str]]:
    """Plan packaging-glob edits in pyproject.toml.

    Renames the package discovery glob and the wheel package path, but NOT the
    project ``name`` (which is the product name and must stay "humbug").
    """
    if not os.path.exists(PYPROJECT):
        print(f"WARNING: {PYPROJECT} not found; skipping.", file=sys.stderr)
        return []

    with open(PYPROJECT, "r", encoding="utf-8") as handle:
        lines = handle.readlines()

    planned: List[Tuple[int, str, str]] = []
    for index, line in enumerate(lines):
        stripped = line.strip()

        # Never touch the product name.
        if stripped.startswith("name") and "humbug" in stripped:
            continue

        # include = [..., "humbug*"]  -> "desktop*"
        if '"humbug*"' in line:
            new = line.replace('"humbug*"', '"desktop*"')
            planned.append((index + 1, line.rstrip("\n"), new.rstrip("\n")))
            continue

        # "src/humbug" -> "src/desktop"
        if '"src/humbug"' in line:
            new = line.replace('"src/humbug"', '"src/desktop"')
            planned.append((index + 1, line.rstrip("\n"), new.rstrip("\n")))

    return planned


def _plan_spec() -> List[Tuple[int, str, str]]:
    """Plan the PyInstaller entry-point path edit in humbug.spec.

    Only the source path into the moved package is changed
    (``src/humbug/__main__.py`` -> ``src/desktop/__main__.py``). All branding
    in the spec (app name, icons, bundle identifier) is product-level and is
    left untouched. The spec *filename* is also left unchanged because it names
    the product, not the source package.
    """
    if not os.path.exists(SPEC_FILE):
        print(f"WARNING: {SPEC_FILE} not found; skipping.", file=sys.stderr)
        return []

    with open(SPEC_FILE, "r", encoding="utf-8") as handle:
        lines = handle.readlines()

    planned: List[Tuple[int, str, str]] = []
    needle = "src/humbug/__main__.py"
    replacement = "src/desktop/__main__.py"
    for index, line in enumerate(lines):
        if needle in line:
            new = line.replace(needle, replacement)
            planned.append((index + 1, line.rstrip("\n"), new.rstrip("\n")))

    return planned


def _move_directory(use_git: bool, apply: bool) -> bool:
    """Move src/humbug -> src/desktop. Returns True if a move is planned/done."""
    if not os.path.isdir(SRC_OLD_DIR):
        print(f"WARNING: {SRC_OLD_DIR} not found; skipping directory move.", file=sys.stderr)
        return False

    if os.path.exists(SRC_NEW_DIR):
        print(f"ERROR: target {SRC_NEW_DIR} already exists; aborting move.", file=sys.stderr)
        return False

    if not apply:
        how = "git mv" if use_git else "os.rename"
        print(f"  [{how}] {SRC_OLD_DIR} -> {SRC_NEW_DIR}")
        return True

    if use_git:
        subprocess.run(["git", "mv", SRC_OLD_DIR, SRC_NEW_DIR], check=True)
    else:
        os.rename(SRC_OLD_DIR, SRC_NEW_DIR)

    return True


def _report_python_changes(changes: List[FileChange]) -> int:
    total = 0
    for change in changes:
        print(f"\n{change.path}")
        for lineno, old, new in change.edits:
            print(f"  {lineno}: - {old}")
            print(f"  {lineno}: + {new}")
            total += 1

    return total


def main() -> int:
    parser = argparse.ArgumentParser(description="Rename the humbug GUI package to desktop.")
    parser.add_argument("--apply", action="store_true", help="Apply changes (default: dry-run).")
    parser.add_argument("--git", action="store_true", help="Use 'git mv' for the directory move.")
    args = parser.parse_args()

    mode = "APPLY" if args.apply else "DRY-RUN"
    print(f"=== humbug -> desktop migration ({mode}) ===")

    # Plan everything first (against current on-disk layout).
    py_changes = _plan_python_edits(SRC_OLD_DIR if os.path.isdir(SRC_OLD_DIR) else SRC_NEW_DIR)
    dep_changes = _plan_dependency_rules()
    pyproject_changes = _plan_pyproject()
    spec_changes = _plan_spec()

    print("\n--- Python import rewrites ---")
    total_imports = _report_python_changes(py_changes)
    print(f"\n  ({total_imports} import line(s) across {len(py_changes)} file(s))")

    print(f"\n--- {DEPENDENCY_RULES} ---")
    for lineno, old, new in dep_changes:
        print(f"  {lineno}: - {old}")
        print(f"  {lineno}: + {new}")

    print(f"\n--- {PYPROJECT} ---")
    for lineno, old, new in pyproject_changes:
        print(f"  {lineno}: - {old}")
        print(f"  {lineno}: + {new}")

    print(f"\n--- {SPEC_FILE} ---")
    for lineno, old, new in spec_changes:
        print(f"  {lineno}: - {old}")
        print(f"  {lineno}: + {new}")

    print("\n--- directory move ---")
    move_planned = _move_directory(use_git=args.git, apply=False)

    if not args.apply:
        print("\nDry-run complete. Re-run with --apply to make changes.")
        return 0

    # Apply order: edit files first (while old paths still exist), then move dir.
    _apply_python_edits(py_changes)
    if dep_changes:
        _apply_line_edits(DEPENDENCY_RULES, dep_changes)

    if pyproject_changes:
        _apply_line_edits(PYPROJECT, pyproject_changes)

    if spec_changes:
        _apply_line_edits(SPEC_FILE, spec_changes)

    if move_planned:
        _move_directory(use_git=args.git, apply=True)

    print("\nApplied. Recommended follow-up: run the dependency checker, mypy, and the test suite.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
