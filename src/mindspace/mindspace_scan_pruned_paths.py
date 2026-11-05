"""The directories and paths the change scanner does not descend."""

from collections.abc import Iterable

from mindspace.mindspace_ignored_dirs import IGNORED_DIRS

# The scanner prunes what is not the user's content: tool caches and generated
# output are rebuilt from sources that are themselves watched, so recording them
# adds no coverage and buries the changes that matter.  A coverage report is the
# clearest case — a single test run rewrites hundreds of files that no one would
# ever want to revert, and the source change that produced them is already
# recorded against the file it was made in.
#
# These are the defaults a mindspace's settings start from, and what a mindspace
# created with no saved setting is scanned with.  The value a mindspace saves is
# used in place of this list rather than added to it, so a mindspace can prune
# less than this as well as more.
#
# This is a separate set from IGNORED_DIRS because that set also decides what
# global search and the Quick Switcher show.  The two deliberately differ: search
# should not hide generated output a user might want to read, while the change
# list should not fill with it.
DEFAULT_SCAN_PRUNED_PATHS = IGNORED_DIRS | frozenset({
    ".cache",
    ".hypothesis",
    ".mypy_cache",
    ".nox",
    ".pytest_cache",
    ".ruff_cache",
    ".tox",
    "htmlcov",
})

# Paths that are never scanned, whatever a mindspace's settings say.  .humbug/
# holds the scanner's own baseline and change log, so descending into it would
# record the act of watching as a change and could never settle.  .git/ is
# rewritten wholesale by git itself, and retaining the pre-image of a pack file
# is a worse outcome than not reporting the change at all.
MANDATORY_SCAN_PRUNED_PATHS = frozenset({".git", ".humbug"})


def scan_pruned_paths(configured: Iterable[str]) -> frozenset[str]:
    """
    Return the paths a scan must not descend, given a mindspace's own list.

    An entry is a directory name, which matches a directory at any depth, or a
    path from the mindspace root, which matches only that directory and what is
    below it.  A leading or trailing separator is ignored, as are entries that
    cannot name a directory inside the mindspace: an absolute path is read as a
    relative one, and any entry containing a '..' segment is dropped.

    Args:
        configured: The entries a user saved for the mindspace.

    Returns:
        The effective set, which always includes the mandatory paths.
    """
    normalised = {
        path for path in (_normalise_scan_pruned_path(entry) for entry in configured) if path
    }
    return frozenset(normalised) | MANDATORY_SCAN_PRUNED_PATHS


def _normalise_scan_pruned_path(configured: str) -> str:
    """
    Return a configured entry in the form the scanner compares against.

    Args:
        configured: One line as the user typed it.

    Returns:
        The entry with separators normalised, or an empty string when the entry
        cannot name a directory inside the mindspace.
    """
    path = configured.strip().replace("\\", "/").strip("/")
    if not path or path == ".":
        return ""

    if any(segment in ("", ".", "..") for segment in path.split("/")):
        return ""

    return path
