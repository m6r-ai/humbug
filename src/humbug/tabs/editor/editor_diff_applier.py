"""Unified diff parsing and fuzzy application for editor buffers."""

import difflib
import logging
import re
from dataclasses import dataclass
from typing import List, Tuple, Dict, Any

from PySide6.QtGui import QTextCursor, QTextDocument


@dataclass
class DiffLine:
    """Represents a single line in a diff hunk."""
    type: str  # ' ' for context, '-' for deletion, '+' for addition
    content: str  # The actual line content (without the prefix character)


@dataclass
class DiffHunk:
    """Represents a single hunk from a unified diff."""
    old_start: int  # Starting line number in original file (1-indexed)
    old_count: int  # Number of lines in original file
    new_start: int  # Starting line number in new file (1-indexed)
    new_count: int  # Number of lines in new file
    lines: List[DiffLine]  # The actual diff lines


@dataclass
class MatchResult:
    """Result of attempting to match a hunk to document content."""
    success: bool
    location: int  # Line number where hunk should be applied (1-indexed)
    confidence: float  # 0.0 to 1.0
    actual_lines: List[str]  # The actual lines found at this location


class DiffParser:
    """Parser for unified diff format."""

    def __init__(self):
        """Initialize the diff parser."""
        self._logger = logging.getLogger("DiffParser")

    def parse(self, diff_text: str) -> Tuple[List[DiffHunk], str | None]:
        """
        Parse unified diff text into structured hunks.

        Args:
            diff_text: Unified diff format text

        Returns:
            Tuple of (list of hunks, error message if parsing failed)
        """
        if not diff_text or not diff_text.strip():
            return [], "Empty diff provided"

        lines = diff_text.splitlines()
        hunks: List[DiffHunk] = []

        # Skip file headers (--- and +++ lines) if present
        start_idx = 0
        while start_idx < len(lines):
            line = lines[start_idx]
            if line.startswith('---') or line.startswith('+++'):
                start_idx += 1
                continue
            break

        # Parse hunks
        i = start_idx
        while i < len(lines):
            line = lines[i]

            # Look for hunk header: @@ -old_start,old_count +new_start,new_count @@
            if line.startswith('@@'):
                hunk, error = self._parse_hunk(lines, i)
                if error:
                    return [], error

                if hunk:
                    hunks.append(hunk)
                    # Skip past the lines we just parsed
                    i += 1 + len(hunk.lines)

                else:
                    i += 1

            else:
                # Skip non-hunk lines (could be additional headers or context)
                i += 1

        if not hunks:
            return [], "No valid hunks found in diff"

        return hunks, None

    def _parse_hunk(self, lines: List[str], start_idx: int) -> Tuple[DiffHunk | None, str | None]:
        """
        Parse a single hunk starting at the given index.

        Args:
            lines: All lines from the diff
            start_idx: Index of the @@ line

        Returns:
            Tuple of (parsed hunk or None, error message if failed)
        """
        header = lines[start_idx]

        # Parse hunk header: @@ -old_start,old_count +new_start,new_count @@
        match = re.match(r'^@@\s+-(\d+)(?:,(\d+))?\s+\+(\d+)(?:,(\d+))?\s+@@', header)
        if not match:
            return None, f"Invalid hunk header format: {header}"

        old_start = int(match.group(1))
        old_count = int(match.group(2)) if match.group(2) else 1
        new_start = int(match.group(3))
        new_count = int(match.group(4)) if match.group(4) else 1

        # Parse hunk lines
        hunk_lines: List[DiffLine] = []
        i = start_idx + 1

        while i < len(lines):
            line = lines[i]

            # Stop at next hunk or end of diff
            if line.startswith('@@'):
                break

            # Skip empty lines at the end
            if not line:
                i += 1
                continue

            # Parse diff line
            if line.startswith(' '):
                hunk_lines.append(DiffLine(' ', line[1:]))

            elif line.startswith('-'):
                hunk_lines.append(DiffLine('-', line[1:]))

            elif line.startswith('+'):
                hunk_lines.append(DiffLine('+', line[1:]))

            elif line.startswith('\\'):
                # "\ No newline at end of file" - ignore for now
                pass

            else:
                # Treat as context line if it doesn't have a prefix
                # This handles cases where the AI might forget the space prefix
                hunk_lines.append(DiffLine(' ', line))

            i += 1

        return DiffHunk(old_start, old_count, new_start, new_count, hunk_lines), None


class FuzzyMatcher:
    """Fuzzy matching of diff hunks to document content."""

    def __init__(self, confidence_threshold: float = 0.75, search_window: int = 50):
        """
        Initialize the fuzzy matcher.

        Args:
            confidence_threshold: Minimum confidence (0.0-1.0) required for a match
            search_window: Number of lines to search above/below expected position
        """
        self._logger = logging.getLogger("FuzzyMatcher")
        self._confidence_threshold = confidence_threshold
        self._search_window = search_window

    def find_match(
        self,
        hunk: DiffHunk,
        document: QTextDocument
    ) -> MatchResult:
        """
        Find the best location to apply a hunk in the document.

        Args:
            hunk: The diff hunk to locate
            document: The Qt text document to search

        Returns:
            MatchResult with location and confidence
        """
        # Extract context and deletion lines from hunk
        expected_lines = [
            line.content for line in hunk.lines
            if line.type in (' ', '-')
        ]

        if not expected_lines:
            # No context or deletions to match against - this is a pure insertion
            # Apply at the specified location
            return MatchResult(
                success=True,
                location=hunk.old_start,
                confidence=1.0,
                actual_lines=[]
            )

        # Try exact match at expected location first
        exact_match = self._try_exact_match(hunk.old_start, expected_lines, document)
        if exact_match.success:
            return exact_match

        # Try fuzzy matching within search window
        return self._fuzzy_search(hunk.old_start, expected_lines, document)

    def _try_exact_match(
        self,
        line_num: int,
        expected_lines: List[str],
        document: QTextDocument
    ) -> MatchResult:
        """
        Try to match exactly at the specified line number.

        Args:
            line_num: Line number to check (1-indexed)
            expected_lines: Lines we expect to find
            document: Document to search

        Returns:
            MatchResult indicating success or failure
        """
        actual_lines = self._get_document_lines(document, line_num, len(expected_lines))

        if self._lines_match_exactly(expected_lines, actual_lines):
            return MatchResult(
                success=True,
                location=line_num,
                confidence=1.0,
                actual_lines=actual_lines
            )

        return MatchResult(
            success=False,
            location=line_num,
            confidence=0.0,
            actual_lines=actual_lines
        )

    def _fuzzy_search(
        self,
        expected_line: int,
        expected_lines: List[str],
        document: QTextDocument
    ) -> MatchResult:
        """
        Search for best match within the search window.

        Args:
            expected_line: Expected line number (1-indexed)
            expected_lines: Lines we expect to find
            document: Document to search

        Returns:
            MatchResult with best match found
        """
        total_lines = document.blockCount()

        # Calculate search range
        start_line = max(1, expected_line - self._search_window)
        end_line = min(total_lines, expected_line + self._search_window)

        best_match = MatchResult(
            success=False,
            location=expected_line,
            confidence=0.0,
            actual_lines=[]
        )

        # Search each potential position
        for line_num in range(start_line, end_line + 1):
            actual_lines = self._get_document_lines(document, line_num, len(expected_lines))

            if len(actual_lines) != len(expected_lines):
                continue

            confidence = self._calculate_confidence(expected_lines, actual_lines)

            # Prefer matches closer to expected location if confidence is equal
            distance_penalty = abs(line_num - expected_line) * 0.001
            adjusted_confidence = confidence - distance_penalty

            if adjusted_confidence > best_match.confidence:
                best_match = MatchResult(
                    success=confidence >= self._confidence_threshold,
                    location=line_num,
                    confidence=confidence,
                    actual_lines=actual_lines
                )

        return best_match

    def _get_document_lines(
        self,
        document: QTextDocument,
        start_line: int,
        count: int
    ) -> List[str]:
        """
        Get lines from document.

        Args:
            document: Document to read from
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

    def _lines_match_exactly(self, expected: List[str], actual: List[str]) -> bool:
        """
        Check if lines match exactly (after trimming whitespace).

        Args:
            expected: Expected line contents
            actual: Actual line contents

        Returns:
            True if all lines match
        """
        if len(expected) != len(actual):
            return False

        for exp, act in zip(expected, actual):
            if exp.strip() != act.strip():
                return False

        return True

    def _calculate_confidence(self, expected: List[str], actual: List[str]) -> float:
        """
        Calculate confidence score for a potential match.

        Args:
            expected: Expected line contents
            actual: Actual line contents

        Returns:
            Confidence score from 0.0 to 1.0
        """
        if len(expected) != len(actual):
            return 0.0

        exact_matches = 0
        total_similarity = 0.0

        for exp, act in zip(expected, actual):
            exp_stripped = exp.strip()
            act_stripped = act.strip()

            if exp_stripped == act_stripped:
                exact_matches += 1
                total_similarity += 1.0
            else:
                # Calculate similarity using SequenceMatcher
                similarity = difflib.SequenceMatcher(None, exp_stripped, act_stripped).ratio()
                total_similarity += similarity

        # Calculate confidence as average of:
        # 1. Percentage of exact matches
        # 2. Average similarity score
        exact_match_ratio = exact_matches / len(expected)
        avg_similarity = total_similarity / len(expected)

        # Weight exact matches more heavily
        confidence = (exact_match_ratio * 0.6) + (avg_similarity * 0.4)

        return confidence


class DiffApplier:
    """Applies unified diffs to Qt text documents with fuzzy matching."""

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
        self._logger = logging.getLogger("DiffApplier")
        self._parser = DiffParser()
        self._matcher = FuzzyMatcher(confidence_threshold, search_window)

    def apply_diff(
        self,
        diff_text: str,
        document: QTextDocument,
        cursor: QTextCursor
    ) -> Dict[str, Any]:
        """
        Apply a unified diff to a document.

        This operation is atomic - either all hunks apply successfully or none do.

        Args:
            diff_text: Unified diff format text
            document: Qt text document to modify
            cursor: Qt text cursor for the document

        Returns:
            Dictionary with operation result:
            - success: bool
            - message: str (human-readable result)
            - error_details: dict (if failed, detailed error information)
        """
        # Parse the diff
        hunks, parse_error = self._parser.parse(diff_text)
        if parse_error:
            return {
                'success': False,
                'message': f'Failed to parse diff: {parse_error}',
                'error_details': {
                    'phase': 'parsing',
                    'reason': parse_error
                }
            }

        self._logger.debug("Parsed %d hunks from diff", len(hunks))

        # Phase 1: Locate all hunks
        hunk_locations: List[Tuple[DiffHunk, MatchResult]] = []

        for idx, hunk in enumerate(hunks):
            match_result = self._matcher.find_match(hunk, document)

            if not match_result.success:
                # Build detailed error response
                expected_lines = [
                    line.content for line in hunk.lines
                    if line.type in (' ', '-')
                ]

                return {
                    'success': False,
                    'message': f'Could not locate hunk {idx + 1} with sufficient confidence',
                    'error_details': {
                        'phase': 'matching',
                        'failed_hunk': idx + 1,
                        'total_hunks': len(hunks),
                        'reason': 'Could not locate hunk with sufficient confidence',
                        'expected_location': hunk.old_start,
                        'expected_context': expected_lines,
                        'searched_range': [
                            max(1, hunk.old_start - self._matcher._search_window),
                            min(document.blockCount(), hunk.old_start + self._matcher._search_window)
                        ],
                        'best_match': {
                            'location': match_result.location,
                            'confidence': round(match_result.confidence, 2),
                            'actual_context': match_result.actual_lines
                        },
                        'suggestion': 'Context lines do not match. Consider reading the current file content and regenerating the diff.'
                    }
                }

            hunk_locations.append((hunk, match_result))
            self._logger.debug(
                "Hunk %d matched at line %d with confidence %.2f",
                idx + 1, match_result.location, match_result.confidence
            )

        # Phase 2: Sort hunks by location (bottom to top) and check for overlaps
        hunk_locations.sort(key=lambda x: x[1].location, reverse=True)

        overlap_error = self._check_for_overlaps(hunk_locations)
        if overlap_error:
            return overlap_error

        # Phase 3: Apply all hunks atomically
        cursor.beginEditBlock()
        try:
            for hunk, match_result in hunk_locations:
                self._apply_hunk(hunk, match_result.location, document, cursor)

            cursor.endEditBlock()

            return {
                'success': True,
                'message': f'Successfully applied {len(hunks)} hunk(s)',
                'hunks_applied': len(hunks)
            }

        except Exception as e:
            cursor.endEditBlock()  # This will trigger undo
            self._logger.exception("Failed to apply diff: %s", str(e))
            return {
                'success': False,
                'message': f'Failed to apply diff: {str(e)}',
                'error_details': {
                    'phase': 'application',
                    'reason': str(e)
                }
            }

    def _check_for_overlaps(
        self,
        hunk_locations: List[Tuple[DiffHunk, MatchResult]]
    ) -> Dict[str, Any] | None:
        """
        Check if any hunks would overlap when applied.

        Args:
            hunk_locations: List of (hunk, match_result) tuples, sorted by location

        Returns:
            Error dictionary if overlaps found, None otherwise
        """
        for i in range(len(hunk_locations) - 1):
            hunk1, match1 = hunk_locations[i]
            hunk2, match2 = hunk_locations[i + 1]

            # Calculate the range each hunk affects
            hunk1_end = match1.location + hunk1.old_count - 1
            hunk2_start = match2.location

            if hunk1_end >= hunk2_start:
                return {
                    'success': False,
                    'message': 'Hunks would overlap when applied',
                    'error_details': {
                        'phase': 'validation',
                        'reason': 'Overlapping hunks detected',
                        'hunk1_range': [match1.location, hunk1_end],
                        'hunk2_range': [match2.location, match2.location + hunk2.old_count - 1],
                        'suggestion': 'Hunks affect overlapping line ranges. Regenerate diff with non-overlapping changes.'
                    }
                }

        return None

    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: QTextDocument,
        cursor: QTextCursor
    ) -> None:
        """
        Apply a single hunk to the document.

        Args:
            hunk: The hunk to apply
            location: Line number where to apply (1-indexed)
            document: Document to modify
            cursor: Cursor for modifications
        """
        # Position cursor at the start of the hunk location
        block = document.findBlockByLineNumber(location - 1)
        if not block.isValid():
            raise ValueError(f"Invalid line number: {location}")

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
