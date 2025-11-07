"""Exception classes for preview operations."""

from typing import Dict


class PreviewError(Exception):
    """Base class for preview-related exceptions."""

    def __init__(self, message: str, details: Dict | None = None) -> None:
        """
        Initialize preview error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}


class PreviewIOError(PreviewError):
    """Exception raised when preview file operations fail."""
