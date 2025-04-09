from typing import Dict


class ConversationTranscriptError(Exception):
    """Base exception class for transcript-related errors."""

    def __init__(self, message: str, details: Dict | None = None):
        """
        Initialize transcript error.

        Args:
            message: Error description
            details: Optional dictionary with additional error details
        """
        super().__init__(message)
        self.details = details or {}


class ConversationTranscriptFormatError(ConversationTranscriptError):
    """Exception raised when transcript format is invalid."""
    pass


class ConversationTranscriptIOError(ConversationTranscriptError):
    """Exception raised when transcript file operations fail."""
    pass
