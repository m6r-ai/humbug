"""Shared fixtures and utilities for diff tests."""

import pytest
from typing import Any, List

from diff.diff_matcher import DiffMatcher
from diff.diff_applier import DiffApplier
from diff.diff_types import DiffHunk, MatchResult


class SimpleDiffMatcher(DiffMatcher):
    """Simple string-based diff matcher for testing."""

    def _get_document_lines(
        self,
        document: Any,
        start_line: int,
        count: int
    ) -> List[str]:
        """Get lines from a string document."""
        if isinstance(document, str):
            lines = document.splitlines()
        elif isinstance(document, list):
            lines = document
        else:
            raise TypeError(f"Unsupported document type: {type(document)}")

        # Convert to 0-indexed
        start_idx = start_line - 1
        end_idx = start_idx + count

        # Handle out-of-bounds gracefully
        if start_idx < 0:
            start_idx = 0
        if end_idx > len(lines):
            end_idx = len(lines)

        return lines[start_idx:end_idx]


class SimpleDiffApplier(DiffApplier):
    """Simple string-based diff applier for testing."""

    def _create_matcher(self) -> DiffMatcher:
        """Create a simple string matcher."""
        return SimpleDiffMatcher(
            confidence_threshold=self._confidence_threshold,
            search_window=self._search_window
        )

    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: Any,
        context: Any
    ) -> None:
        """Apply a hunk to a list of strings."""
        if not isinstance(document, list):
            raise TypeError("Document must be a list of strings")

        # Convert to 0-indexed
        start_idx = location - 1

        # Calculate how many lines to remove
        lines_to_remove = sum(1 for line in hunk.lines if line.type in (' ', '-'))

        # Remove old lines
        for _ in range(lines_to_remove):
            if start_idx < len(document):
                document.pop(start_idx)

        # Insert new lines
        insert_idx = start_idx
        for line in hunk.lines:
            if line.type in (' ', '+'):
                document.insert(insert_idx, line.content)
                insert_idx += 1


@pytest.fixture
def simple_matcher():
    """Create a simple diff matcher for testing."""
    return SimpleDiffMatcher()


@pytest.fixture
def simple_matcher_custom():
    """Factory for simple diff matchers with custom configuration."""
    def _create_matcher(confidence_threshold: float = 0.75, search_window: int = 50):
        return SimpleDiffMatcher(
            confidence_threshold=confidence_threshold,
            search_window=search_window
        )
    return _create_matcher


@pytest.fixture
def simple_applier():
    """Create a simple diff applier for testing."""
    return SimpleDiffApplier()


@pytest.fixture
def simple_applier_custom():
    """Factory for simple diff appliers with custom configuration."""
    def _create_applier(confidence_threshold: float = 0.75, search_window: int = 50):
        return SimpleDiffApplier(
            confidence_threshold=confidence_threshold,
            search_window=search_window
        )
    return _create_applier


class DiffTestHelpers:
    """Helper utilities for diff testing."""

    @staticmethod
    def create_simple_document(lines: List[str]) -> List[str]:
        """Create a simple document from lines."""
        return lines.copy()

    @staticmethod
    def document_to_string(document: List[str]) -> str:
        """Convert document to string."""
        return '\n'.join(document)

    @staticmethod
    def string_to_document(text: str) -> List[str]:
        """Convert string to document."""
        return text.splitlines()


@pytest.fixture
def helpers():
    """Provide test helper utilities."""
    return DiffTestHelpers
