"""Abstract diff matcher with fuzzy matching support."""

import difflib
from abc import ABC, abstractmethod
from typing import Any, List

from diff.diff_types import DiffHunk, MatchResult


class DiffMatcher(ABC):
    """Abstract base class for diff hunk matching."""

    def __init__(self, confidence_threshold: float = 0.75, search_window: int = 50):
        """
        Initialize the matcher.

        Args:
            confidence_threshold: Minimum confidence (0.0-1.0) required for a match
            search_window: Number of lines to search above/below expected position
        """
        self._confidence_threshold = confidence_threshold
        self._search_window = search_window

    @abstractmethod
    def _get_document_lines(
        self,
        document: Any,
        start_line: int,
        count: int
    ) -> List[str]:
        """
        Get lines from document.

        Args:
            document: Document to read from (type varies by implementation)
            start_line: Starting line number (1-indexed)
            count: Number of lines to retrieve

        Returns:
            List of line contents
        """

    def search_window(self) -> int:
        """Get the current search window size."""
        return self._search_window

    def find_match(
        self,
        hunk: DiffHunk,
        document: Any
    ) -> MatchResult:
        """
        Find the best location to apply a hunk in the document.

        Args:
            hunk: The diff hunk to locate
            document: The document to search

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
        document: Any
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
        document: Any
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
        # Get total line count from document
        total_lines = self._get_document_line_count(document)

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

    def _get_document_line_count(self, document: Any) -> int:
        """
        Get total number of lines in document.

        Default implementation tries to get lines until exhausted.
        Subclasses can override for more efficient implementations.

        Args:
            document: Document to count lines in

        Returns:
            Number of lines in document
        """
        # Try to get a very large number of lines and see how many we get
        lines = self._get_document_lines(document, 1, 1000000)
        return len(lines)

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
