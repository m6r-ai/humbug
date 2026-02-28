#!/usr/bin/env python3
"""
Rename Menai higher-order functions to follow the new convention:
  map-list    → map-list
  filter-list → filter-list
  fold-list   → fold-list
  find-list   → find-list
  any-list?   → any-list?
  all-list?   → all-list?
  sort-list   → sort-list
  zip-list    → zip-list
  list-unzip  → (removed — flags occurrences for manual review)
  map-dict    → map-dict
  filter-dict → filter-dict

Usage:
  python tools/rename_higher_order.py [--dry-run] [--path PATH]

Options:
  --dry-run   Show what would change without writing files
  --path      Root path to search (default: current directory)
"""

import argparse
import os
import re
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Rename mapping — order matters: longer names first to avoid partial matches
# ---------------------------------------------------------------------------
RENAMES = [
    ("filter-list", "filter-list"),
    ("fold-list",   "fold-list"),
    ("find-list",   "find-list"),
    ("any-list?",   "any-list?"),
    ("all-list?",   "all-list?"),
    ("sort-list",   "sort-list"),
    ("zip-list",    "zip-list"),
    ("map-list",    "map-list"),
    ("filter-dict", "filter-dict"),
    ("map-dict",    "map-dict"),
]

# list-unzip is being removed — flag occurrences rather than rename
REMOVED = ["list-unzip"]

# File extensions to process
EXTENSIONS = {".py", ".menai", ".md", ".txt"}

# Directories to skip entirely
SKIP_DIRS = {".git", ".mypy_cache", "__pycache__", ".pytest_cache",
             "venv", "build", "dist", "htmlcov", ".humbug"}


def make_pattern(old_name: str) -> re.Pattern:
    """
    Match old_name only when it is NOT preceded or followed by an identifier
    character (letters, digits, hyphens, underscores, ?, !).
    This prevents partial matches like map-list matching list-mapping.
    """
    escaped = re.escape(old_name)
    return re.compile(r'(?<![A-Za-z0-9\-_?!])' + escaped + r'(?![A-Za-z0-9\-_?!])')


def process_file(path: Path, dry_run: bool) -> tuple[int, int, list[str]]:
    """
    Process a single file.
    Returns (rename_count, flag_count, warnings).
    """
    try:
        original = path.read_text(encoding="utf-8")
    except (UnicodeDecodeError, PermissionError) as e:
        return 0, 0, [f"  SKIP (read error: {e})"]

    content = original
    rename_count = 0
    flag_count = 0
    warnings = []

    # Apply renames
    for old, new in RENAMES:
        pattern = make_pattern(old)
        matches = pattern.findall(content)
        if matches:
            content = pattern.sub(new, content)
            rename_count += len(matches)

    # Flag removals
    for old in REMOVED:
        pattern = make_pattern(old)
        matches = list(pattern.finditer(content))
        if matches:
            flag_count += len(matches)
            for m in matches:
                line_num = content[:m.start()].count('\n') + 1
                warnings.append(f"  ⚠️  MANUAL REVIEW NEEDED: '{old}' at line {line_num} — this function is being removed")

    if content != original:
        if not dry_run:
            path.write_text(content, encoding="utf-8")

    return rename_count, flag_count, warnings


def walk_files(root: Path):
    """Yield all files under root that should be processed."""
    for dirpath, dirnames, filenames in os.walk(root):
        # Prune skip dirs in-place
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for filename in filenames:
            p = Path(dirpath) / filename
            if p.suffix in EXTENSIONS:
                yield p


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dry-run", action="store_true",
                        help="Show changes without writing files")
    parser.add_argument("--path", default=".",
                        help="Root path to search (default: current directory)")
    args = parser.parse_args()

    root = Path(args.path).resolve()
    dry_run = args.dry_run

    print(f"{'DRY RUN — ' if dry_run else ''}Scanning: {root}")
    print()

    total_renames = 0
    total_flags = 0
    changed_files = 0

    for file_path in sorted(walk_files(root)):
        rel = file_path.relative_to(root)
        rename_count, flag_count, warnings = process_file(file_path, dry_run)

        if rename_count > 0 or flag_count > 0:
            action = "Would update" if dry_run else "Updated"
            print(f"{action}: {rel}  ({rename_count} rename(s), {flag_count} flag(s))")
            for w in warnings:
                print(w)
            changed_files += 1
            total_renames += rename_count
            total_flags += flag_count

    print()
    print("=" * 60)
    print(f"Files {'that would be ' if dry_run else ''}changed: {changed_files}")
    print(f"Total renames: {total_renames}")
    print(f"Total manual reviews needed (list-unzip): {total_flags}")

    if total_flags > 0:
        print()
        print("⚠️  list-unzip occurrences need manual replacement:")
        print("   Use (map-list list-first pairs) / (map-list list-last pairs) instead")

    if dry_run:
        print()
        print("Run without --dry-run to apply changes.")


if __name__ == "__main__":
    main()
