"""Abstract base class for AI tools."""

import logging
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Callable, Awaitable

from ai_tool.ai_tool_call import AIToolCall
from ai_tool.ai_tool_definition import AIToolDefinition
from ai_tool.ai_tool_parameter import AIToolParameter
from ai_tool.ai_tool_operation_definition import AIToolOperationDefinition
from ai_tool.ai_tool_exceptions import AIToolExecutionError, AIToolAuthorizationDenied
from ai_tool.ai_tool_result import AIToolResult


# Type alias for the authorization callback
AIToolAuthorizationCallback = Callable[[str, Dict[str, Any], str, str | None, bool], Awaitable[bool]]


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
        """
        return {}

    def extract_context(self, tool_call: AIToolCall) -> str | None:  # pylint: disable=unused-argument
        """
        Extract context information from the tool call.

        Args:
            tool_call: Tool call containing arguments and metadata

        Returns:
            Context string if available, otherwise None
        """
        operation_definitions = self.get_operation_definitions()
        if not operation_definitions:
            return None

        arguments = tool_call.arguments
        operation = arguments.get("operation")

        if not operation or not isinstance(operation, str):
            return None

        if operation not in operation_definitions:
            return None

        operation_def = operation_definitions[operation]
        extract_context = operation_def.extract_context

        if extract_context is None:
            return None

        try:
            return extract_context(arguments)
        except AIToolExecutionError:
            # Ignore errors during context extraction
            return None

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the tool with given arguments.

        Default implementation handles operation-based routing if operations are defined.
        Tools without operations must override this method.

        Args:
            tool_call: Tool call containing arguments and metadata
            requester_ref: Reference to the requester (e.g., user or system)
            request_authorization: Callback for requesting authorization

        Returns:
            AIToolResult containing the execution result and optional continuation

        Raises:
            AIToolExecutionError: If tool execution fails
            AIToolAuthorizationDenied: If authorization is required but denied
            AIToolTimeoutError: If tool execution times out
        """
        operation_definitions = self.get_operation_definitions()

        # If no operations defined, subclass must override execute()
        if not operation_definitions:
            raise NotImplementedError(
                f"{self.__class__.__name__} must either define operations or override execute()"
            )

        # Extract and validate operation
        arguments = tool_call.arguments
        operation = arguments.get("operation")

        if not operation:
            raise AIToolExecutionError("No 'operation' argument provided")

        if not isinstance(operation, str):
            raise AIToolExecutionError("'operation' must be a string")

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

        # Log operation
        logger = self.get_logger()
        logger.debug("%s operation requested: %s", self.get_tool_name(), operation)

        try:
            return await operation_def.handler(tool_call, requester_ref, request_authorization)

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            raise

        except Exception as e:
            logger.error(
                "Unexpected error in %s operation '%s': %s",
                self.get_tool_name(), operation, str(e), exc_info=True
            )
            raise AIToolExecutionError(f"{self.get_tool_name()} operation failed: {str(e)}") from e

    def get_logger(self) -> logging.Logger:
        """
        Get logger for this tool.

        Subclasses can override to provide custom logger.

        Returns:
            Logger instance for this tool
        """
        return logging.getLogger(self.__class__.__name__)

    def get_tool_name(self) -> str:
        """
        Get tool name for logging and error messages.

        Subclasses can override to provide custom tool name.

        Returns:
            Tool name string
        """
        # Default: remove "AITool" suffix and convert to lowercase
        class_name = self.__class__.__name__
        if class_name.endswith("AITool"):
            return class_name[:-6].lower()
        return class_name.lower()

    def _build_definition_from_operations(
        self,
        name: str,
        description_prefix: str,
        additional_parameters: List[AIToolParameter] | None = None
    ) -> AIToolDefinition:
        """
        Build tool definition from operation definitions.

        Helper for operation-based tools to reduce boilerplate.

        Args:
            name: Tool name
            description_prefix: Description text before operation list
            additional_parameters: Optional additional parameters beyond standard 'operation' parameter

        Returns:
            Complete tool definition
        """
        operations = self.get_operation_definitions()
        operation_names = list(operations.keys())

        # Build operation list
        operation_list = []
        for op_name, op_def in operations.items():
            operation_list.append(f"- {op_name}: {op_def.description}")

        description = f"{description_prefix}\n\nAvailable operations:\n\n" + "\n".join(operation_list)

        # Standard operation parameter
        parameters = [
            AIToolParameter(
                name="operation",
                type="string",
                description=f"{name.capitalize()} operation to perform",
                required=True,
                enum=operation_names
            )
        ]

        # Add any additional parameters
        if additional_parameters:
            parameters.extend(additional_parameters)

        return AIToolDefinition(
            name=name,
            description=description,
            parameters=parameters
        )

    def _get_str_value_from_key(self, key: str, arguments: Dict[str, Any]) -> str:
        """
        Extract string value from arguments dictionary.

        Common helper for operation handlers.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters

        Returns:
            String value for the given key

        Raises:
            AIToolExecutionError: If key is missing or value is not a string
        """
        if key not in arguments:
            raise AIToolExecutionError(f"No '{key}' argument provided")

        value = arguments[key]
        if not isinstance(value, str):
            raise AIToolExecutionError(f"'{key}' must be a string")

        return value
