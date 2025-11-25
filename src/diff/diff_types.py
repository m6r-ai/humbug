"""Shared dataclasses for diff operations."""

from dataclasses import dataclass
from typing import List


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


@dataclass
class DiffApplicationResult:
    """Result of applying a diff."""

    success: bool
    message: str
    hunks_applied: int = 0
    error_details: dict | None = None
