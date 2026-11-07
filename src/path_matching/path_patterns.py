"""
Glob matching of a path against a list of entries.

The rule is the one the filesystem tool's access lists have always used.  A path
matches an entry when any of three things holds:

``fnmatch`` of the whole path against the entry, which lets ``*`` cross a separator;
``Path.match``, which compares the trailing components and so matches a single name
at any depth;
an entry ending ``/**``, which additionally matches the base itself rather than only
what is below it.

What that means in practice is worth stating, because it is not what a plain glob
means: ``venv`` matches any path whose last component is exactly ``venv``, never
``venvity``; ``src/generated`` matches trailing components, so it matches a
``src/generated`` anywhere in the tree and never ``src/generatedly``; ``**`` matches
everything.

Two things are deliberately not handled here, because they belong to a caller's own
entries rather than to the rule.  Paths and entries are compared with '/' separators,
and neither is expanded: a caller that accepts '~' expands it before matching.
"""

import fnmatch
import os
from collections.abc import Iterable
from pathlib import Path

_WILDCARD_CHARACTERS = frozenset("*?[")


class PathPatterns:
    """
    A list of entries, matched against paths.

    An entry without a wildcard character is matched by comparing components, which
    is cheap enough to run over every file of a mindspace on every checkpoint.  That
    is not a second rule: it is the same answer as the three tests above, worked out
    in advance.  An entry that needs a glob goes through the three tests themselves,
    because ``fnmatch`` and ``Path.match`` differ from each other in ways that would
    be easy to get wrong by hand — an absolute pattern, for instance, does not match
    in the middle of a path while a relative one does.
    """

    def __init__(self, entries: Iterable[str]) -> None:
        """
        Build a matcher for a list of entries.

        Args:
            entries: The glob entries.  Empty entries are skipped, as the access lists
                have always skipped them, so a blank line in a text area costs nothing.
        """
        names: set[str] = set()
        paths: list[str] = []
        patterns: list[str] = []

        for entry in entries:
            if not entry:
                continue

            folded = _fold(entry)
            if _needs_glob(entry):
                patterns.append(folded)

            elif "/" in folded:
                paths.append(folded)

            else:
                names.add(folded)

        self._names = frozenset(names)
        self._paths = tuple(paths)
        self._patterns = tuple(patterns)

    def matches(self, path: str) -> bool:
        """
        Return True if a path matches any of the entries.

        Args:
            path: The path to test, with '/' separators.

        Returns:
            True if the path matches.
        """
        folded = _fold(path)

        if folded.rsplit("/", 1)[-1] in self._names:
            return True

        for literal in self._paths:
            if folded == literal or folded.endswith("/" + literal):
                return True

        return any(_matches_pattern(folded, pattern) for pattern in self._patterns)


def _fold(path: str) -> str:
    """
    Return a path in the case the platform's matching compares in.

    ``fnmatch`` and ``Path.match`` both fold case on Windows and neither folds it
    anywhere else, so comparing components directly has to fold to reach the same
    answer.  Only the case is folded: ``os.path.normcase`` would also rewrite the
    separators, which these paths do not use.

    Args:
        path: The path or entry to fold.
    """
    return path.lower() if os.name == "nt" else path


def _needs_glob(entry: str) -> bool:
    """
    Return True if an entry has to go through the glob tests themselves.

    An entry ending in a separator is included deliberately: it means the same as the
    same entry without the separator to ``Path.match``, and sending it through the
    glob tests keeps that the reference's answer rather than this module's opinion.

    Args:
        entry: One entry.
    """
    if entry.endswith("/"):
        return True

    return not _WILDCARD_CHARACTERS.isdisjoint(entry)


def _matches_pattern(path: str, pattern: str) -> bool:
    """
    Return True if a path matches one entry, by the three tests the access lists use.

    Args:
        path: The path to test, folded and with '/' separators.
        pattern: The entry, folded.
    """
    if pattern.endswith("/**"):
        base = pattern[:-3]
        if path == base or path.startswith(base + "/"):
            return True

    if fnmatch.fnmatch(path, pattern):
        return True

    try:
        return Path(path).match(pattern)

    except Exception:  # pylint: disable=broad-except
        # A pattern Path.match cannot interpret matches nothing rather than failing
        # the caller, which is how the access lists have always treated one.
        return False
