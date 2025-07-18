"""Abstract base class for AI tools."""

from abc import ABC, abstractmethod
from typing import Any, Dict, Callable, Awaitable

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
            raise AIToolExecutionError(
                "No 'operation' argument provided",
                self.get_definition().name,
                arguments
            )

        if not isinstance(operation, str):
            raise AIToolExecutionError(
                "'operation' must be a string",
                self.get_definition().name,
                arguments
            )

        # Check if operation is valid
        if operation not in operation_definitions:
            available_operations = ", ".join(sorted(operation_definitions.keys()))
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}. Available operations: {available_operations}",
                self.get_definition().name,
                arguments
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
                f"Parameter(s) {invalid_list} not valid for operation '{operation}'",
                self.get_definition().name,
                arguments
            )

        # Check for missing required parameters
        missing_params = operation_def.required_parameters - provided_params
        if missing_params:
            missing_list = ", ".join(sorted(missing_params))
            raise AIToolExecutionError(
                f"Required parameter(s) {missing_list} missing for operation '{operation}'",
                self.get_definition().name,
                arguments
            )

    @abstractmethod
    async def execute(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """
        Execute the tool with given arguments.

        Args:
            arguments: Dictionary of tool arguments
            request_authorization: Callback for requesting authorization

        Returns:
            String result of tool execution

        Raises:
            AIToolExecutionError: If tool execution fails
            AIToolAuthorizationDenied: If authorization is required but denied
            AIToolTimeoutError: If tool execution times out
        """

    def supports_continuations(self) -> bool:
        """
        Check if this tool supports returning continuations.

        Returns:
            True if the tool can return continuations, False otherwise
        """
        return False

    async def execute_with_continuation(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> 'AIToolResult':
        """
        Execute the tool with given arguments, potentially returning a continuation.

        This method should be overridden by tools that need to support parallel execution.
        The default implementation calls execute() and wraps the result.

        Args:
            arguments: Dictionary of tool arguments
            request_authorization: Callback for requesting authorization

        Returns:
            AIToolResult containing the execution result and optional continuation

        Raises:
            AIToolExecutionError: If tool execution fails
            AIToolAuthorizationDenied: If authorization is required but denied
            AIToolTimeoutError: If tool execution times out
        """
        # Import here to avoid circular imports
        from ai_tool.ai_tool_result import AIToolResult

        # Default implementation: call execute() and wrap result
        content = await self.execute(arguments, request_authorization)

        # Create a dummy tool call ID if not provided
        tool_call_id = arguments.get('_tool_call_id', 'unknown')

        return AIToolResult(
            id=tool_call_id,
            name=self.get_definition().name,
            content=content
        )
