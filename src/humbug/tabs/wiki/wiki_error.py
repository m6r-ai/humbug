"""Exception classes for wiki operations."""

from typing import Dict


class WikiError(Exception):
    """Base class for wiki-related exceptions."""

    def __init__(self, message: str, details: Dict | None = None) -> None:
        """
        Initialize wiki error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}


class WikiIOError(WikiError):
    """Exception raised when wiki file operations fail."""
