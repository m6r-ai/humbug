"""Exception classes for conversation operations."""


class ConversationError(Exception):
    """Base class for conversation-related exceptions."""

    def __init__(self, message: str, details: dict | None = None) -> None:
        """
        Initialize conversation error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}
