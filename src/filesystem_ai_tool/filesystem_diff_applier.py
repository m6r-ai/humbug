"""Filesystem-specific diff application."""

import logging
from typing import Any

from diff import DiffApplier, DiffApplicationError, DiffApplicationResult, DiffHunk, DiffMatcher


class FilesystemDiffMatcher(DiffMatcher):
    """Diff matcher for file content (list of strings)."""

    def _get_document_lines(
        self,
        document: list[str],
        start_line: int,
        count: int
    ) -> list[str]:
        """
        Get lines from file content.

        Args:
            document: File content as list of lines
            start_line: Starting line number (1-indexed)
            count: Number of lines to retrieve

        Returns:
            List of line contents
        """
        lines: list[str] = []

        for i in range(count):
            line_idx = start_line + i - 1  # Convert to 0-indexed

            if line_idx >= len(document):
                break

            lines.append(document[line_idx])

        return lines

    def _get_document_line_count(self, document: list[str]) -> int:
        """
        Get total number of lines in file content.

        Args:
            document: File content as list of lines

        Returns:
            Number of lines in document
        """
        return len(document)


class FilesystemDiffApplier(DiffApplier):
    """Diff applier for file content."""

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
        self._logger = logging.getLogger("FilesystemDiffApplier")

    def _create_matcher(self) -> DiffMatcher:
        """
        Create filesystem content matcher.

        Returns:
            FilesystemDiffMatcher instance
        """
        return FilesystemDiffMatcher(self._confidence_threshold, self._search_window)

    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: list[str],
        context: Any
    ) -> None:
        """
        Apply a single hunk to file content.

        Args:
            hunk: The hunk to apply
            location: Line number where to apply (1-indexed, or 0 for empty file)
            document: File content as list of lines (modified in place)
            context: Unused (for compatibility with base class)
        """
        current_line = location - 1 if location > 0 else 0  # Convert to 0-indexed

        # Process each line in the hunk
        for line in hunk.lines:
            if line.type == ' ':
                # Context line - move past it
                current_line += 1

            elif line.type == '-':
                # Deletion - remove this line
                if current_line >= len(document):
                    raise DiffApplicationError(
                        f"Cannot delete line {current_line + 1}: document only has {len(document)} lines"
                    )

                del document[current_line]
                # Don't increment current_line (next line moved into this position)

            elif line.type == '+':
                # Addition - insert this line
                document.insert(current_line, line.content)
                current_line += 1

    def apply_diff_to_file(
        self,
        diff_text: str,
        file_content: str,
        dry_run: bool = False
    ) -> tuple[DiffApplicationResult, str | None]:
        """
        Apply a unified diff to file content.

        Args:
            diff_text: Unified diff format text
            file_content: Current file content as string
            dry_run: If True, validate but don't return modified content

        Returns:
            Tuple of (DiffApplicationResult, modified content or None)
            Modified content is None if dry_run=True or if application failed
        """
        # Parse the diff up front to extract trailing-newline markers.  The
        # marker ("\ No newline at end of file") only ever appears on a hunk
        # that touches the end of the file, so its presence tells us the
        # result's trailing newline state.
        hunks = self._parser.parse(diff_text)

        # Detect line ending from file content
        line_ending = self._detect_line_ending(file_content)

        # Split into lines (stripping line endings; trailing newline state is
        # tracked separately via the original content and the hunk markers)
        lines = file_content.splitlines(keepends=False)

        # Apply diff using the base class
        result = self.apply_diff(diff_text, lines, dry_run=dry_run)

        if not result.success or dry_run:
            return result, None

        # Reconstruct file content with preserved line endings
        modified_content = line_ending.join(lines)

        # Determine whether the result should end with a trailing newline.
        # The "\ No newline" marker only appears on hunks touching EOF, so if
        # any hunk carries one it owns the result's trailing newline state.
        # Otherwise preserve the original file's trailing newline.
        original_has_trailing_newline = bool(file_content) and file_content.endswith(('\n', '\r\n', '\r'))
        result_no_newline = self._result_has_no_trailing_newline(hunks, original_has_trailing_newline)

        if lines and not result_no_newline:
            modified_content += line_ending

        return result, modified_content

    def _result_has_no_trailing_newline(
        self,
        hunks: list[DiffHunk],
        original_has_trailing_newline: bool
    ) -> bool:
        """
        Determine whether the result file should have no trailing newline.

        The no-newline marker is only valid on a hunk that
        touches the end of the file, so any hunk carrying the marker on its
        new side owns the result's trailing newline state.  When no hunk has a
        marker, the original trailing newline state is preserved.

        Args:
            hunks: Parsed diff hunks
            original_has_trailing_newline: Whether the original had a trailing newline

        Returns:
            True if the result should have no trailing newline, False if it should
        """
        for hunk in hunks:
            if hunk.new_no_newline or hunk.old_no_newline:
                return hunk.new_no_newline

        return not original_has_trailing_newline

    def _detect_line_ending(self, content: str) -> str:
        """
        Detect line ending style from file content.

        Args:
            content: File content

        Returns:
            Line ending string ('\n', '\r\n', or '\r')
        """
        # Check for Windows line ending first (most specific)
        if '\r\n' in content:
            return '\r\n'

        # Check for old Mac line ending
        if '\r' in content:
            return '\r'

        # Default to Unix line ending
        return '\n'
