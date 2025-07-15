"""Abstract base class for AI tools."""

from abc import ABC, abstractmethod
from typing import Any, Dict, Callable, Awaitable

from ai_tool.ai_tool_definition import AIToolDefinition
from ai_tool.ai_tool_operation_definition import AIToolOperationDefinition
from ai_tool.ai_tool_exceptions import AIToolExecutionError


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
