"""Line-list diff matching and application, stdlib only."""

import logging
from typing import Any

from diff import DiffApplicationError, DiffApplicationResult, DiffHunk, DiffMatcher, DiffApplier


class EditorContextDiffMatcher(DiffMatcher):
    """Diff matcher for line-list documents (``list[str]``)."""

    def _get_document_lines(
        self,
        document: list[str],
        start_line: int,
        count: int
    ) -> list[str]:
        """
        Get lines from the list.

        Args:
            document: Line list to read from.
            start_line: Starting line number (1-indexed).
            count: Number of lines to retrieve.

        Returns:
            List of line contents (may be shorter than *count* if the
            document is shorter).
        """
        idx = max(start_line - 1, 0)

        return document[idx:idx + count]

    def _get_document_line_count(self, document: list[str]) -> int:
        """
        Get the number of content lines in the document.

        A trailing empty string represents a trailing newline, not a
        content line, and is excluded.
        """
        if not document:
            return 0

        if len(document) == 1 and document[0] == '':
            return 1

        if document[-1] == '':
            return len(document) - 1

        return len(document)


class EditorContextDiffApplier(DiffApplier):
    """Diff applier for line-list documents (``list[str]``)."""

    def __init__(
        self,
        confidence_threshold: float = 0.75,
        search_window: int = 50
    ):
        """
        Initialise the diff applier.

        Args:
            confidence_threshold: Minimum confidence required for matches.
            search_window: Lines to search above/below expected position.
        """
        super().__init__(confidence_threshold, search_window)
        self._logger = logging.getLogger("EditorContextDiffApplier")

    def _create_matcher(self) -> DiffMatcher:
        """Create a line-list matcher."""
        return EditorContextDiffMatcher(self._confidence_threshold, self._search_window)

    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: list[str],
        context: Any
    ) -> None:
        """
        Apply a single hunk to the line list.

        Hunks are applied bottom-to-top (sorted by the base class), so
        earlier line numbers remain valid.  Within a hunk, context lines
        advance the document index, deletion lines are removed at the
        current index, and addition lines are inserted at the current
        index.

        Only content lines are processed here; trailing newline state is
        handled by ``_adjust_trailing_newline`` after all hunks apply.

        Args:
            hunk: The hunk to apply.
            location: Line number where to apply (1-indexed, or 0 for empty file).
            document: Line list to modify (mutated in place).
            context: Unused (present for DiffApplier contract compatibility).
        """
        if location == 0:
            new_lines = [line.content for line in hunk.lines if line.type == '+']
            document.clear()
            document.extend(new_lines)
            return

        doc_idx = location - 1

        for line in hunk.lines:
            if line.type == ' ':
                doc_idx += 1

            elif line.type == '-':
                if doc_idx >= len(document):
                    raise DiffApplicationError(
                        f"Deletion at line {doc_idx + 1} exceeds document length"
                    )

                del document[doc_idx]

            elif line.type == '+':
                document.insert(doc_idx, line.content)
                doc_idx += 1

    def apply_diff(
        self,
        diff_text: str,
        document: list[str],
        dry_run: bool = False,
        **_kwargs: Any
    ) -> DiffApplicationResult:
        """
        Apply a unified diff to a line-list document.

        Extends the base class by adjusting the trailing newline state
        after all hunks are applied.

        Args:
            diff_text: Unified diff format text.
            document: Line list to modify (mutated in place).
            dry_run: If True, validate but don't apply changes.

        Returns:
            DiffApplicationResult with operation status.
        """
        newline_owner: DiffHunk | None = None

        if not dry_run:
            hunks = self._parser.parse(diff_text)
            newline_owner = self._find_newline_owner(hunks)

        result = super().apply_diff(diff_text, document, dry_run=dry_run)

        if not dry_run and result.success and newline_owner is not None:
            self._adjust_trailing_newline(document, newline_owner.new_no_newline)

        return result

    def _find_newline_owner(self, hunks: list[DiffHunk]) -> DiffHunk | None:
        """
        Find the hunk that owns the trailing newline state.

        The no-newline marker only appears on hunks touching EOF.  When
        multiple hunks carry it, the one with the highest ``new_start``
        (closest to EOF) wins.

        Args:
            hunks: Parsed diff hunks.

        Returns:
            The owning hunk, or None if no hunk carries the marker.
        """
        owner: DiffHunk | None = None
        owner_start = -1

        for hunk in hunks:
            if hunk.old_no_newline or hunk.new_no_newline:
                if hunk.new_start >= owner_start:
                    owner = hunk
                    owner_start = hunk.new_start

        return owner

    def _adjust_trailing_newline(
        self,
        document: list[str],
        no_newline: bool
    ) -> None:
        """
        Ensure the document's trailing newline state matches *no_newline*.

        A document ending with a trailing newline has an empty string as
        its final element; one without does not.

        Args:
            document: Line list to adjust (mutated in place).
            no_newline: True if the result should have no trailing newline.
        """
        has_trailing = bool(document) and document[-1] == ''

        if no_newline and has_trailing:
            document.pop()

        elif not no_newline and not has_trailing and document:
            document.append('')
