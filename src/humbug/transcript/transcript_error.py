from typing import Dict, Optional


class TranscriptError(Exception):
    """Base exception class for transcript-related errors."""

    def __init__(self, message: str, details: Optional[Dict] = None):
        """
        Initialize transcript error.

        Args:
            message: Error description
            details: Optional dictionary with additional error details
        """
        super().__init__(message)
        self.details = details or {}


class TranscriptFormatError(TranscriptError):
    """Exception raised when transcript format is invalid."""
    pass


class TranscriptIOError(TranscriptError):
    """Exception raised when transcript file operations fail."""
    pass