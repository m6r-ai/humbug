"""Exception classes for system operations."""


class SystemTabError(Exception):
    """Base class for system-related exceptions."""

    def __init__(self, message: str, details: dict | None = None) -> None:
        """
        Initialize system error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}
