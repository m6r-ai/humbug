"""Exception classes for AI tool framework."""

from typing import Any, Dict


class AIToolAuthorizationDenied(Exception):
    """Exception raised when tool authorization is denied."""

    def __init__(self, message: str, tool_name: str, arguments: Dict[str, Any]):
        """
        Initialize tool authorization denied error.

        Args:
            message: Error message
            tool_name: Name of the tool that was denied
            arguments: Arguments that were passed to the tool
        """
        super().__init__(message)
        self.tool_name = tool_name
        self.arguments = arguments


class AIToolExecutionError(Exception):
    """Exception raised when tool execution fails."""

    def __init__(self, message: str, tool_name: str, arguments: Dict[str, Any]):
        """
        Initialize tool execution error.

        Args:
            message: Error message
            tool_name: Name of the tool that failed
            arguments: Arguments that were passed to the tool
        """
        super().__init__(message)
        self.tool_name = tool_name
        self.arguments = arguments


class AIToolTimeoutError(Exception):
    """Exception raised when tool execution times out."""

    def __init__(self, message: str, tool_name: str, arguments: Dict[str, Any], timeout_duration: float):
        """
        Initialize tool timeout error.

        Args:
            message: Error message
            tool_name: Name of the tool that timed out
            arguments: Arguments that were passed to the tool
            timeout_duration: How long the tool was allowed to run
        """
        super().__init__(message)
        self.tool_name = tool_name
        self.arguments = arguments
        self.timeout_duration = timeout_duration
