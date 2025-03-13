from typing import Dict, Optional


class AITranscriptError(Exception):
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


class AITranscriptFormatError(AITranscriptError):
    """Exception raised when transcript format is invalid."""
    pass


class AITranscriptIOError(AITranscriptError):
    """Exception raised when transcript file operations fail."""
    pass
