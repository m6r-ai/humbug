"""Diff context model."""

from collections.abc import Callable
from dataclasses import dataclass
import os
from typing import Any


@dataclass(frozen=True)
class JournalDiffTarget:
    """
    Marks a diff whose old side comes from the change journal.

    Passed as a context's initial model when opening the tab, and retained in the
    context so a restored tab still compares against the recorded version rather
    than against HEAD.
    """

    sequence: int


class DiffContext:
    """
    Model-layer context for a diff tab.

    Owns the path of the file being diffed.  The diff content itself is derived
    from the working tree and HEAD by the git layer, so only the path is
    persisted.

    The visualisation side-effect (scrolling the viewport to a working-tree line)
    is emitted as a callback so the front end can react without the context
    needing to know about widgets.
    """

    context_type = "diff"

    def __init__(
        self,
        context_id: str,
        path: str,
        on_scroll_to_line: Callable | None = None,
        journal_sequence: int | None = None,
    ) -> None:
        """
        Initialise the diff context.

        Args:
            context_id: Stable identifier issued by the ContextRegistry.
            path: Absolute path to the file being diffed.
            on_scroll_to_line: Optional callable(line) invoked when a working-tree
                line should be scrolled into view (e.g. when opened from the
                editor).
            journal_sequence: When set, the old side of the diff is the version
                the change journal recorded for that entry.
        """
        self._context_id = context_id
        self._path = path
        self._on_scroll_to_line = on_scroll_to_line
        self._journal_sequence = journal_sequence

    def context_id(self) -> str:
        """Return the stable context identifier."""
        return self._context_id

    def save_content_state(self) -> dict[str, Any]:
        """
        Return this diff's content state as a JSON-safe dictionary.

        The diff is regenerated from the working tree and HEAD when the context
        is restored, so only the path is persisted.

        Returns:
            Dictionary with a path key, plus the journal entry the diff compares
            against when it compares against one.
        """
        state: dict[str, Any] = {"path": self._path}
        if self._journal_sequence is not None:
            state["journal_sequence"] = self._journal_sequence

        return state

    def get_info(self) -> dict[str, Any]:
        """
        Return high-level metadata about the diff.

        Returns:
            Dictionary with path and filename.
        """
        return {
            "path": self._path,
            "filename": os.path.basename(self._path),
        }

    def scroll_to_line(self, line: int) -> bool:
        """
        Request that the frontend scroll the given working-tree line into view.

        Fires the on_scroll_to_line callback if one was supplied.  A CLI frontend
        would supply None and this becomes a no-op.

        Args:
            line: 1-based working-tree line to bring into view.

        Returns:
            True if the scroll was requested successfully, False if no callback
            was supplied.
        """
        if self._on_scroll_to_line is None:
            return False

        return bool(self._on_scroll_to_line(line))
