"""Qt-specific diff application for editor widgets."""

import logging
from typing import Any, List

from PySide6.QtGui import QTextCursor, QTextDocument

from diff import DiffApplier, DiffHunk, DiffMatcher, DiffApplicationError, DiffMatchError, DiffApplicationResult


class EditorDiffMatcher(DiffMatcher):
    """Diff matcher for Qt text documents."""

    def _get_document_lines(
        self,
        document: QTextDocument,
        start_line: int,
        count: int
    ) -> List[str]:
        """
        Get lines from Qt text document.

        Args:
            document: Qt text document to read from
            start_line: Starting line number (1-indexed)
            count: Number of lines to retrieve

        Returns:
            List of line contents
        """
        lines: List[str] = []

        for i in range(count):
            line_num = start_line + i - 1  # Convert to 0-indexed
            block = document.findBlockByLineNumber(line_num)

            if not block.isValid():
                break

            lines.append(block.text())

        return lines

    def _get_document_line_count(self, document: QTextDocument) -> int:
        """
        Get total number of lines in Qt text document.

        Args:
            document: Qt text document

        Returns:
            Number of lines in document
        """
        return document.blockCount()


class EditorDiffApplier(DiffApplier):
    """Diff applier for Qt text documents."""

    def __init__(
        self,
        confidence_threshold: float = 0.75,
        search_window: int = 50
    ):
        """
        Initialize the diff applier.

        Args:
            confidence_threshold: Minimum confidence required for matches
            search_window: Lines to search above/below expected position
        """
        super().__init__(confidence_threshold, search_window)
        self._logger = logging.getLogger("EditorDiffApplier")

    def _create_matcher(self) -> DiffMatcher:
        """
        Create Qt document matcher.

        Returns:
            EditorDiffMatcher instance
        """
        return EditorDiffMatcher(self._confidence_threshold, self._search_window)

    def apply_diff(
        self,
        diff_text: str,
        document: QTextDocument,
        dry_run: bool = False,
        **kwargs: Any
    ) -> Any:
        """
        Apply a unified diff to a Qt text document.

        This is an override that handles Qt-specific cursor management.

        Args:
            diff_text: Unified diff format text
            document: Qt text document to modify
            dry_run: If True, validate but don't apply changes
            **kwargs: Must include 'cursor' - QTextCursor for the document

        Returns:
            DiffApplicationResult with operation status
        """
        cursor = kwargs.get('cursor')
        if cursor is None:
            raise DiffApplicationError("'cursor' parameter is required for editor diff application")

        # Parse the diff
        hunks = self._parser.parse(diff_text)

        # Phase 1: Locate all hunks
        matcher = self._create_matcher()
        hunk_locations = []

        for idx, hunk in enumerate(hunks):
            match_result = matcher.find_match(hunk, document)

            if not match_result.success:
                # Build detailed error information

                expected_lines = [
                    line.content for line in hunk.lines
                    if line.type in (' ', '-')
                ]

                search_window = matcher.search_window()
                error_details = {
                    'phase': 'matching',
                    'failed_hunk': idx + 1,
                    'total_hunks': len(hunks),
                    'reason': 'Could not locate hunk with sufficient confidence',
                    'expected_location': hunk.old_start,
                    'expected_context': expected_lines,
                    'searched_range': [
                        max(1, hunk.old_start - search_window),
                        hunk.old_start + search_window
                    ],
                    'best_match': {
                        'location': match_result.location,
                        'confidence': round(match_result.confidence, 2),
                        'actual_context': match_result.actual_lines
                    },
                    'suggestion': 'Context lines do not match. Consider reading the current '
                        'content and regenerating the diff.'
                }

                raise DiffMatchError(
                    f'Could not locate hunk {idx + 1} with sufficient confidence',
                    error_details
                )

            hunk_locations.append((hunk, match_result))

        # Phase 2: Sort hunks by location (bottom to top) and check for overlaps
        # Before sorting, identify the bottommost hunk (highest line number) to position cursor there later
        bottommost_hunk = None
        bottommost_location = -1
        for hunk, match_result in hunk_locations:
            if match_result.location > bottommost_location:
                bottommost_location = match_result.location
                bottommost_hunk = hunk

        hunk_locations.sort(key=lambda x: x[1].location, reverse=True)
        self._check_for_overlaps(hunk_locations)

        # Calculate where the bottommost hunk will end after applying changes
        final_cursor_line = None
        if bottommost_hunk:
            # Count the number of lines that will exist after this hunk is applied
            lines_after_hunk = sum(1 for line in bottommost_hunk.lines if line.type in (' ', '+'))
            final_cursor_line = bottommost_location + lines_after_hunk

        # If dry run, return success without applying
        if dry_run:
            return DiffApplicationResult(
                success=True,
                message=f'Diff validation successful: {len(hunks)} hunk(s) can be applied',
                hunks_applied=len(hunks)
            )

        # Phase 3: Apply all hunks atomically
        cursor.beginEditBlock()
        try:
            for hunk, match_result in hunk_locations:
                self._apply_hunk(hunk, match_result.location, document, cursor)

            cursor.endEditBlock()

            # Move cursor to the end of the bottommost hunk
            if final_cursor_line is not None:
                block = document.findBlockByLineNumber(final_cursor_line - 1)
                if block.isValid():
                    cursor.setPosition(block.position())

            return DiffApplicationResult(
                success=True,
                message=f'Successfully applied {len(hunks)} hunk(s)',
                hunks_applied=len(hunks)
            )

        except Exception as e:
            cursor.endEditBlock()  # This will trigger undo
            self._logger.exception("Failed to apply diff: %s", str(e))
            raise DiffApplicationError(f"Failed to apply diff: {str(e)}") from e

    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: QTextDocument,
        context: QTextCursor
    ) -> None:
        """
        Apply a single hunk to Qt text document.

        Args:
            hunk: The hunk to apply
            location: Line number where to apply (1-indexed, or 0 for empty file)
            document: Qt text document to modify
            context: Qt text cursor for modifications
        """
        cursor = context

        # Handle empty file case (location == 0)
        if location == 0:
            # Position cursor at start of empty document
            cursor.setPosition(0)

        else:
            # Position cursor at the start of the hunk location
            block = document.findBlockByLineNumber(location - 1)
            if not block.isValid():
                raise DiffApplicationError(f"Invalid line number: {location}")

            cursor.setPosition(block.position())

        # Process each line in the hunk
        for line in hunk.lines:
            if line.type == ' ':
                # Context line - move past it
                cursor.movePosition(QTextCursor.MoveOperation.Down)

            elif line.type == '-':
                # Deletion - remove this line
                cursor.movePosition(QTextCursor.MoveOperation.StartOfBlock)
                cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock, QTextCursor.MoveMode.KeepAnchor)

                # If not at end of document, include the newline
                if not cursor.atEnd():
                    cursor.movePosition(QTextCursor.MoveOperation.Right, QTextCursor.MoveMode.KeepAnchor)

                # If at end but not at start of document, delete the newline before
                elif location > 1:
                    cursor.movePosition(QTextCursor.MoveOperation.StartOfBlock)
                    cursor.movePosition(QTextCursor.MoveOperation.Left, QTextCursor.MoveMode.KeepAnchor)
                    cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock, QTextCursor.MoveMode.KeepAnchor)

                cursor.removeSelectedText()

            elif line.type == '+':
                # Addition - insert this line
                cursor.movePosition(QTextCursor.MoveOperation.StartOfBlock)
                cursor.insertText(line.content + '\n')
