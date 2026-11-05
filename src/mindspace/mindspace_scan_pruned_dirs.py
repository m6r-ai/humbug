"""Directories the change scanner does not descend."""

from mindspace.mindspace_ignored_dirs import IGNORED_DIRS

# The scanner prunes what is not the user's content: tool caches and generated
# output are rebuilt from sources that are themselves watched, so recording them
# adds no coverage and buries the changes that matter.  A coverage report is the
# clearest case — a single test run rewrites hundreds of files that no one would
# ever want to revert, and the source change that produced them is already
# recorded against the file it was made in.
#
# This is a separate constant from IGNORED_DIRS because that set also decides what
# global search and the Quick Switcher show.  The two deliberately differ: search
# should not hide generated output a user might want to read, while the change
# list should not fill with it.
SCAN_PRUNED_DIRS = IGNORED_DIRS | frozenset({
    ".cache",
    ".hypothesis",
    ".mypy_cache",
    ".nox",
    ".pytest_cache",
    ".ruff_cache",
    ".tox",
    "htmlcov",
})
