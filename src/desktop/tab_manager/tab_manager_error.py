"""Exception classes for tab manager operations."""


class TabManagerError(Exception):
    """Base class for tab manager-related exceptions."""

    def __init__(self, message: str, details: dict | None = None) -> None:
        """
        Initialize tab manager error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}
