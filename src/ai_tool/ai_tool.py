"""Abstract base class for AI tools."""

from abc import ABC, abstractmethod
from typing import Any, Dict, Callable, Awaitable

from ai_tool.ai_tool_call import AIToolCall
from ai_tool.ai_tool_definition import AIToolDefinition
from ai_tool.ai_tool_operation_definition import AIToolOperationDefinition
from ai_tool.ai_tool_exceptions import AIToolExecutionError
from ai_tool.ai_tool_result import AIToolResult


# Type alias for the authorization callback
AIToolAuthorizationCallback = Callable[[str, Dict[str, Any], str, bool], Awaitable[bool]]


class AITool(ABC):
    """Abstract base class for AI tools."""

    @abstractmethod
    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition for registration.

        Returns:
            AIToolDefinition describing this tool's interface
        """

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions.
            Default implementation returns empty dict for tools without operations.
        """
        return {}

    def validate_operation_arguments(self, arguments: Dict[str, Any]) -> None:
        """
        Validate arguments for the specified operation.

        Args:
            arguments: Dictionary of tool arguments including 'operation'

        Raises:
            AIToolExecutionError: If arguments are invalid for the operation
        """
        operation_definitions = self.get_operation_definitions()

        # If no operations defined, skip validation
        if not operation_definitions:
            return

        # Extract operation name
        operation = arguments.get("operation")
        if not operation:
            raise AIToolExecutionError("No 'operation' argument provided")

        if not isinstance(operation, str):
            raise AIToolExecutionError("'operation' must be a string")

        # Check if operation is valid
        if operation not in operation_definitions:
            available_operations = ", ".join(sorted(operation_definitions.keys()))
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}. Available operations: {available_operations}"
            )

        operation_def = operation_definitions[operation]

        # Check for invalid parameters (parameters that exist but aren't allowed for this operation)
        provided_params = set(arguments.keys())
        # Always allow 'operation' parameter
        provided_params.discard("operation")

        invalid_params = provided_params - operation_def.allowed_parameters
        if invalid_params:
            invalid_list = ", ".join(sorted(invalid_params))
            raise AIToolExecutionError(
                f"Parameter(s) {invalid_list} not valid for operation '{operation}'"
            )

        # Check for missing required parameters
        missing_params = operation_def.required_parameters - provided_params
        if missing_params:
            missing_list = ", ".join(sorted(missing_params))
            raise AIToolExecutionError(
                f"Required parameter(s) {missing_list} missing for operation '{operation}'"
            )

    @abstractmethod
    async def execute(
        self,
        tool_call: AIToolCall,
        requester: str,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the tool with given arguments, potentially returning a continuation.

        Args:
            tool_call: Tool call containing arguments and metadata
            requester: AI model requesting the tool use
            request_authorization: Callback for requesting authorization

        Returns:
            AIToolResult containing the execution result and optional continuation

        Raises:
            AIToolExecutionError: If tool execution fails
            AIToolAuthorizationDenied: If authorization is required but denied
            AIToolTimeoutError: If tool execution times out
        """
