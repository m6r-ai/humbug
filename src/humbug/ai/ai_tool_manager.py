"""AI tool calling framework."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
import logging
from typing import Any, Dict, List, Callable, Awaitable


@dataclass
class AIToolParameter:
    """Definition of a tool parameter."""
    name: str
    type: str  # "string", "number", "boolean", "array", "object"
    description: str
    required: bool = True
    enum: List[str] | None = None
    properties: Dict[str, 'AIToolParameter'] | None = None  # For object types


@dataclass
class AIToolDefinition:
    """Definition of an available tool."""
    name: str
    description: str
    parameters: List[AIToolParameter]


@dataclass
class AIToolCall:
    """Represents a tool call request from the AI."""
    id: str  # Unique identifier for this tool call
    name: str
    arguments: Dict[str, Any]

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert the tool call to a dictionary.

        Returns:
            Dictionary representation of the tool call
        """
        return {
            'id': self.id,
            'name': self.name,
            'arguments': self.arguments
        }


@dataclass
class AIToolResult:
    """Result of a tool execution."""
    id: str
    name: str
    content: str
    error: str | None = None

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert the tool result to a dictionary.

        Returns:
            Dictionary representation of the tool result
        """
        return {
            'id': self.id,
            'name': self.name,
            'content': self.content,
            'error': self.error
        }


# Type alias for the authorization callback
AIToolAuthorizationCallback = Callable[[str, Dict[str, Any], str], Awaitable[bool]]


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


class AITool(ABC):
    """Abstract base class for AI tools."""

    @abstractmethod
    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition for registration.

        Returns:
            AIToolDefinition describing this tool's interface
        """

    @abstractmethod
    async def execute(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback | None = None
    ) -> str:
        """
        Execute the tool with given arguments.

        Args:
            arguments: Dictionary of tool arguments
            request_authorization: Optional callback for requesting authorization.

        Returns:
            String result of tool execution

        Raises:
            AIToolExecutionError: If tool execution fails
            AIToolAuthorizationDenied: If authorization is required but denied
            AIToolTimeoutError: If tool execution times out
        """


class AIToolManager:
    """Singleton manager for AI tools."""

    _instance: 'AIToolManager | None' = None

    def __new__(cls) -> 'AIToolManager':
        if cls._instance is None:
            cls._instance = super().__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        if not hasattr(self, '_initialized'):
            self._tools: Dict[str, AITool] = {}
            self._logger = logging.getLogger("AIToolManager")
            self._initialized = True

    def register_tool(self, tool: AITool) -> None:
        """
        Register a tool for use with AI models.

        Args:
            tool: The tool to register

        Raises:
            ValueError: If a tool with the same name is already registered
        """
        definition = tool.get_definition()

        if definition.name in self._tools:
            raise ValueError(f"Tool '{definition.name}' is already registered")

        self._tools[definition.name] = tool
        self._logger.info("Registered tool: %s", definition.name)

    def unregister_tool(self, name: str) -> None:
        """
        Unregister a tool.

        Args:
            name: Name of the tool to unregister
        """
        if name in self._tools:
            del self._tools[name]
            self._logger.info("Unregistered tool: %s", name)

    def get_tool_definitions(self) -> List[AIToolDefinition]:
        """
        Get definitions for all registered tools.

        Returns:
            List of tool definitions
        """
        return [tool.get_definition() for tool in self._tools.values()]

    async def execute_tool(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback | None = None
    ) -> AIToolResult:
        """
        Execute a tool call.

        Args:
            tool_call: The tool call to execute
            request_authorization: Optional callback for requesting authorization

        Returns:
            AIToolResult containing the execution result
        """
        if tool_call.name not in self._tools:
            error_msg = f"Unknown tool: {tool_call.name}"
            self._logger.error(error_msg)
            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg
            )

        tool = self._tools[tool_call.name]

        try:
            self._logger.debug(
                "Executing tool '%s' with args %s",
                tool_call.name,
                tool_call.arguments
            )

            result = await tool.execute(tool_call.arguments, request_authorization)

            self._logger.debug(
                "Tool '%s' executed successfully with args %s",
                tool_call.name,
                tool_call.arguments
            )

            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content=result
            )

        except AIToolAuthorizationDenied as e:
            error_msg = f"Tool authorization denied: {str(e)}"
            self._logger.warning(
                "Tool '%s' authorization denied with args %s: %s",
                tool_call.name,
                tool_call.arguments,
                str(e)
            )
            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg
            )

        except Exception as e:
            error_msg = f"Tool execution failed: {str(e)}"
            self._logger.exception(
                "Tool '%s' failed with args %s: %s",
                tool_call.name,
                tool_call.arguments,
                str(e)
            )

            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg
            )

    def get_tool_names(self) -> List[str]:
        """Get names of all registered tools."""
        return list(self._tools.keys())
