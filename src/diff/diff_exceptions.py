"""Custom exceptions for diff operations."""

from typing import Any


class DiffError(Exception):
    """Base exception for diff operations."""

    def __init__(self, message: str, error_details: dict[str, Any] | None = None):
        """
        Initialize the exception.

        Args:
            message: Error message
            error_details: Optional dictionary with detailed error information
        """
        super().__init__(message)
        self.error_details = error_details


class DiffParseError(DiffError):
    """Raised when diff parsing fails."""


class DiffMatchError(DiffError):
    """Raised when hunk matching fails."""


class DiffApplicationError(DiffError):
    """Raised when diff application fails."""


class DiffValidationError(DiffError):
    """Raised when diff validation fails (e.g., overlapping hunks)."""
