"""Exception classes for AI tool framework."""


class AIToolAuthorizationDenied(Exception):
    """Exception raised when tool authorization is denied."""

    def __init__(self, message: str):
        """
        Initialize tool authorization denied error.

        Args:
            message: Error message
        """
        super().__init__(message)


class AIToolExecutionError(Exception):
    """Exception raised when tool execution fails."""

    def __init__(self, message: str):
        """
        Initialize tool execution error.

        Args:
            message: Error message
        """
        super().__init__(message)


class AIToolTimeoutError(Exception):
    """Exception raised when tool execution times out."""

    def __init__(self, message: str, timeout_duration: float):
        """
        Initialize tool timeout error.

        Args:
            message: Error message
            timeout_duration: How long the tool was allowed to run
        """
        super().__init__(message)
        self.timeout_duration = timeout_duration
