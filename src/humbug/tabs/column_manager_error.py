"""Exception classes for conversation operations."""


class ColumnManagerError(Exception):
    """Base class for column manager-related exceptions."""

    def __init__(self, message: str, details: dict | None = None) -> None:
        """
        Initialize column manager error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}
