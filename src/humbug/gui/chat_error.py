"""Exception classes for chat operations."""


class ChatError(Exception):
    """Base class for chat-related exceptions."""

    def __init__(self, message: str, details: dict = None):
        """
        Initialize chat error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}
