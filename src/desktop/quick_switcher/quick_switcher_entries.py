"""
Gathering of Quick Switcher candidates from a mindspace.

Pure data gathering: given the open contexts and the mindspace's paths this
builds the candidate list the overlay filters.  It holds no UI state and takes
no Qt objects, so it can be exercised directly in tests.
"""

import os

from context.context_info import ContextInfo
from mindspace.mindspace_ignored_dirs import IGNORED_DIRS

from desktop.quick_switcher.quick_switcher_widget import QuickSwitcherEntry


def _relative_directory(path: str, mindspace_path: str) -> str:
    """Return the mindspace-relative parent directory of a path, or "." when it is at the root."""
    relative_dir = os.path.dirname(os.path.relpath(path, mindspace_path))
    return relative_dir or "."


def build_quick_switcher_entries(
    contexts: list[ContextInfo],
    mindspace_path: str,
    conversations_dir: str,
    max_files: int,
) -> tuple[list[QuickSwitcherEntry], bool]:
    """
    Build the Quick Switcher's candidate list for a mindspace.

    Anything already open as a tab is listed only as that tab, so picking it
    focuses what is open rather than offering a second, near-identical row.

    Args:
        contexts: Every currently open context.
        mindspace_path: Absolute path to the mindspace root.
        conversations_dir: Absolute path to the mindspace's conversations directory.
        max_files: Upper bound on files gathered from the mindspace tree.

    Returns:
        The candidate entries, and True if the file walk hit max_files.
    """
    entries: list[QuickSwitcherEntry] = []
    open_paths: set[str] = set()

    for context in contexts:
        if not context.path:
            continue

        open_paths.add(os.path.normpath(context.path))
        entries.append(QuickSwitcherEntry(
            entry_id=f"tab:{context.context_id}",
            kind="tab",
            title=context.title,
            subtitle=_relative_directory(context.path, mindspace_path),
            icon_name=context.context_type,
        ))

    for dirpath, _dirnames, filenames in os.walk(conversations_dir):
        for filename in filenames:
            if not filename.lower().endswith(".conv"):
                continue

            path = os.path.join(dirpath, filename)
            if os.path.normpath(path) in open_paths:
                continue

            entries.append(QuickSwitcherEntry(
                entry_id=f"conversation:{path}",
                kind="conversation",
                title=os.path.splitext(filename)[0],
                subtitle=_relative_directory(path, mindspace_path),
                icon_name="conversation",
            ))

    truncated = False
    file_count = 0
    for root, dirs, files in os.walk(mindspace_path):
        dirs[:] = [
            directory for directory in dirs
            if directory not in IGNORED_DIRS and not directory.startswith(".")
        ]
        for filename in files:
            if file_count >= max_files:
                truncated = True
                break

            path = os.path.join(root, filename)
            if os.path.normpath(path) in open_paths:
                continue

            file_count += 1
            entries.append(QuickSwitcherEntry(
                entry_id=f"file:{path}",
                kind="file",
                title=filename,
                subtitle=_relative_directory(path, mindspace_path),
                icon_name="files",
            ))

        if truncated:
            break

    return entries, truncated
