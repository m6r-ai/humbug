"""Qt-specific diff application for editor widgets."""

import logging
from typing import Any

from PySide6.QtGui import QTextCursor, QTextDocument

from diff import DiffApplier, DiffHunk, DiffMatcher, DiffApplicationError, DiffMatchError, DiffApplicationResult


class EditorDiffMatcher(DiffMatcher):
    """Diff matcher for Qt text documents."""

    def _get_document_lines(
        self,
        document: QTextDocument,
        start_line: int,
        count: int
    ) -> list[str]:
        """
        Get lines from Qt text document.

        Args:
            document: Qt text document to read from
            start_line: Starting line number (1-indexed)
            count: Number of lines to retrieve

        Returns:
            List of line contents
        """
        lines: list[str] = []

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

        A document whose last line ends with a trailing newline has an empty
        trailing block; that block is not counted as a line.

        Args:
            document: Qt text document

        Returns:
            Number of content lines in document
        """
        count = document.blockCount()

        # An empty trailing block represents a trailing newline, not a line
        last_block = document.lastBlock()
        if last_block.isValid() and last_block.text() == '' and last_block.blockNumber() > 0:
            return count - 1

        return count


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

                if match_result.out_of_range_location is not None:
                    error_details['exact_match_out_of_range'] = match_result.out_of_range_location
                    error_details['suggestion'] = (
                        f'Exact match found at line {match_result.out_of_range_location}, '
                        f'but hunk specifies line {hunk.old_start}. '
                        'The diff has stale line numbers. Regenerate the diff against the '
                        'current file content.'
                    )

                if match_result.ambiguous_locations is not None:
                    error_details['ambiguous_locations'] = match_result.ambiguous_locations
                    error_details['suggestion'] = (
                        f'Multiple exact matches found at lines '
                        f'{match_result.ambiguous_locations}. '
                        'Add more surrounding context lines to the hunk so that only '
                        'one location matches.'
                    )

                raise DiffMatchError(
                    f'Could not locate hunk {idx + 1} with sufficient confidence',
                    error_details
                )

            hunk_locations.append((hunk, match_result))

        # Phase 2: Sort hunks by location (bottom to top) and check for overlaps
        hunk_locations.sort(key=lambda x: x[1].location, reverse=True)
        self._check_for_overlaps(hunk_locations)

        # Identify the trailing-newline owner: the last hunk (lowest location
        # after reverse sort, i.e. the one closest to EOF) that carries a
        # no-newline marker.  Its new_no_newline flag determines the result's
        # trailing block state.
        newline_owner = self._find_newline_owner(hunk_locations)

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

            # Adjust the trailing block to match the newline state specified by
            # the EOF-touching hunk, if any.
            if newline_owner is not None:
                self._adjust_trailing_newline(document, cursor, newline_owner.new_no_newline)

            cursor.endEditBlock()

            return DiffApplicationResult(
                success=True,
                message=f'Successfully applied {len(hunks)} hunk(s)',
                hunks_applied=len(hunks)
            )

        except Exception as e:
            cursor.endEditBlock()  # This will trigger undo
            self._logger.exception("Failed to apply diff: %s", str(e))
            raise DiffApplicationError(f"Failed to apply diff: {str(e)}") from e

    def _find_newline_owner(
        self,
        hunk_locations: list[tuple[DiffHunk, Any]]
    ) -> DiffHunk | None:
        """
        Find the hunk that owns the trailing newline state.

        The no-newline marker only appears on hunks touching EOF, so any hunk
        carrying the marker owns the result's trailing newline.  When multiple
        hunks carry it, the last one (by location) wins.

        Args:
            hunk_locations: List of (hunk, match_result) tuples

        Returns:
            The owning hunk, or None if no hunk carries the marker
        """
        owner: DiffHunk | None = None
        owner_location = -1

        for hunk, match_result in hunk_locations:
            if hunk.old_no_newline or hunk.new_no_newline:
                if match_result.location >= owner_location:
                    owner = hunk
                    owner_location = match_result.location

        return owner

    def _adjust_trailing_newline(
        self,
        document: QTextDocument,
        cursor: QTextCursor,
        no_newline: bool
    ) -> None:
        """
        Ensure the document's trailing block state matches the desired newline.

        A document ending with a trailing newline has an empty trailing block;
        one without does not.  Add or remove that empty block as needed.

        Args:
            document: Qt text document to modify
            cursor: Qt text cursor for modifications
            no_newline: True if the result should have no trailing newline
        """
        last_block = document.lastBlock()
        has_trailing_empty = (
            last_block.isValid()
            and last_block.text() == ''
            and last_block.blockNumber() > 0
        )

        if no_newline and has_trailing_empty:
            # Remove the trailing empty block so the document ends with content
            cursor.setPosition(last_block.position())
            cursor.movePosition(QTextCursor.MoveOperation.StartOfBlock)
            cursor.movePosition(QTextCursor.MoveOperation.Left, QTextCursor.MoveMode.KeepAnchor)
            cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock, QTextCursor.MoveMode.KeepAnchor)
            cursor.removeSelectedText()

        elif not no_newline and not has_trailing_empty:
            # Add a trailing empty block so the document ends with a newline
            cursor.movePosition(QTextCursor.MoveOperation.End)
            cursor.insertText('\n')

    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: QTextDocument,
        context: QTextCursor
    ) -> None:
        """
        Apply a single hunk to Qt text document.

        Only content lines are processed here; trailing newline state is
        handled separately by _adjust_trailing_newline after all hunks apply.

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
                self._delete_line(cursor)

            elif line.type == '+':
                self._insert_line(cursor, line.content)

    def _delete_line(self, cursor: QTextCursor) -> None:
        """
        Delete the line at the cursor position.

        The block is removed entirely.  If it is the last block, the preceding
        newline is removed instead so the block disappears.

        Args:
            cursor: Qt text cursor positioned at the line to delete
        """
        # Determine the range to remove.  We capture explicit positions rather
        # than chaining movePosition calls with KeepAnchor, because moving Left
        # into the preceding block changes which block EndOfBlock refers to.
        block = cursor.block()
        start = block.position()
        block_end = start + block.length() - 1  # length includes the trailing newline

        next_block = block.next()
        if next_block.isValid():
            # Not the last block: remove this block's content plus its newline
            # so the next block moves up.
            cursor.setPosition(start, QTextCursor.MoveMode.MoveAnchor)
            cursor.setPosition(block_end + 1, QTextCursor.MoveMode.KeepAnchor)

        elif block.blockNumber() > 0:
            # Last block: remove the preceding newline plus this block's
            # content so the block disappears entirely.
            cursor.setPosition(start - 1, QTextCursor.MoveMode.MoveAnchor)
            cursor.setPosition(block_end, QTextCursor.MoveMode.KeepAnchor)

        else:
            # Only block in the document: clear its content.
            cursor.setPosition(start, QTextCursor.MoveMode.MoveAnchor)
            cursor.setPosition(block_end, QTextCursor.MoveMode.KeepAnchor)

        cursor.removeSelectedText()

    def _insert_line(self, cursor: QTextCursor, content: str) -> None:
        """
        Insert a line at the cursor position.

        At EOF on a non-empty final block, the line is appended after the
        current block.  In all other cases it is inserted before the current
        block so existing content is pushed down.

        Args:
            cursor: Qt text cursor positioned at the insertion point
            content: Line content to insert
        """
        # At EOF there is no trailing newline to push down, so append after the
        # last block.  Exclude the case where the cursor is on a trailing empty
        # block (the artifact of a trailing newline) - normal insertion fills
        # the empty block correctly.
        if cursor.atEnd() and not cursor.atStart() and cursor.block().text():
            cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock)
            cursor.insertText('\n' + content)

        else:
            cursor.movePosition(QTextCursor.MoveOperation.StartOfBlock)
            cursor.insertText(content + '\n')
