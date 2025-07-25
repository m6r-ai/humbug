"""Exception classes for shell operations."""


class ShellTabError(Exception):
    """Base class for shell-related exceptions."""

    def __init__(self, message: str, details: dict | None = None) -> None:
        """
        Initialize shell error.

        Args:
            message: Error message
            details: Optional dictionary of additional error details
        """
        super().__init__(message)
        self.details = details or {}
