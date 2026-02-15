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

    def cancel(self) -> None:
        """
        Request cancellation of any ongoing operations.

        This is an optional method that tools can override if they support
        cancellation. The default implementation does nothing.

        This method may be called from a different thread/task than the one
        executing the tool, so implementations must be thread-safe.

        Note: Cancellation is best-effort. Tools should respond to cancellation
        as quickly as possible, but there are no guarantees about timing.
        """

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

    def _get_required_str_value(self, key: str, arguments: Dict[str, Any]) -> str:
        """
        Extract required string value from arguments dictionary.

        Helper for mandatory string parameters in operation handlers.

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

    def get_brief_description(self) -> str:
        """
        Get brief one-line description for system prompt.

        This should be a concise summary (ideally < 100 chars) that helps
        the AI understand when to use this tool. By default, extracts the
        first sentence from the tool's description.

        Tools can override this to provide a more tailored brief description.

        Returns:
            Brief description string

        Example:
            "File and directory operations in mindspace"
        """
        definition = self.get_definition()
        description = definition.description

        # Extract first line or first sentence
        first_line = description.split('\n')[0]
        first_sentence = first_line.split('.')[0]

        # Return first sentence with period
        return first_sentence.strip() + '.' if not first_sentence.endswith('.') else first_sentence.strip()

    def get_operation_summary(self) -> Dict[str, str]:
        """
        Get brief summary of each operation.

        Returns a dictionary mapping operation names to one-line descriptions.
        By default, extracts the first sentence from each operation's description.

        Tools can override this to provide more tailored summaries.

        Returns:
            Dict mapping operation names to brief descriptions

        Example:
            {
                "read_file": "Read file contents",
                "write_file": "Write content to file (create or overwrite)",
                "list_directory": "List directory contents"
            }
        """
        operations = self.get_operation_definitions()
        summaries = {}

        for name, op_def in operations.items():
            # Extract first sentence
            description = op_def.description
            first_sentence = description.split('\n')[0].split('.')[0]
            summaries[name] = first_sentence.strip() + '.' if not first_sentence.endswith('.') else first_sentence.strip()

        return summaries

    def get_detailed_help(self, operation: str | None = None) -> str:
        """
        Get detailed documentation for tool or specific operation.

        This generates comprehensive markdown documentation that includes:
        - For full tool help: All operations with parameters
        - For operation help: Specific operation with detailed parameters

        Tools with complex documentation (like AIFPL) can override this
        to provide custom formatting, examples, or additional guidance.

        Args:
            operation: Optional operation name for operation-specific help.
                      If None, returns full tool documentation.

        Returns:
            Formatted markdown documentation string

        Example:
            # Full tool help
            help_text = tool.get_detailed_help()

            # Operation-specific help
            help_text = tool.get_detailed_help("read_file")
        """
        if operation is None:
            return self._get_tool_help()

        return self._get_operation_help(operation)

    def _get_tool_help(self) -> str:
        """
        Generate comprehensive tool documentation.

        This is the default implementation that generates markdown documentation
        from the tool's definition and operation definitions.

        Returns:
            Markdown-formatted documentation string
        """
        definition = self.get_definition()
        operations = self.get_operation_definitions()

        sections = []

        # Tool header and overview
        sections.append(f"# {definition.name.upper()} Tool\n")
        sections.append(definition.description)
        sections.append("")

        # If no operations, show parameters directly
        if not operations:
            sections.append("## Parameters\n")
            for param in definition.parameters:
                required = " (required)" if param.required else " (optional)"
                sections.append(f"### {param.name} ({param.type}){required}")
                sections.append(param.description)
                if param.enum:
                    sections.append(f"Valid values: {', '.join(param.enum)}")
                sections.append("")

            return "\n".join(sections)

        # Operations-based tool
        sections.append("## Operations\n")

        for name, op_def in sorted(operations.items()):
            sections.append(f"### {name}")
            sections.append(op_def.description)
            sections.append("")

            # Find parameters for this operation
            required_params = [
                param for param in definition.parameters
                if param.name in op_def.required_parameters
            ]
            optional_params = [
                param for param in definition.parameters
                if param.name in op_def.allowed_parameters
                and param.name not in op_def.required_parameters
            ]

            if required_params:
                sections.append("**Required Parameters:**")
                for param in required_params:
                    sections.append(f"- `{param.name}` ({param.type}): {param.description}")
                    if param.enum:
                        sections.append(f"  - Valid values: {', '.join(param.enum)}")

            if optional_params:
                sections.append("")
                sections.append("**Optional Parameters:**")
                for param in optional_params:
                    sections.append(f"- `{param.name}` ({param.type}): {param.description}")
                    if param.enum:
                        sections.append(f"  - Valid values: {', '.join(param.enum)}")

            sections.append("")

        return "\n".join(sections)

    def _get_operation_help(self, operation: str) -> str:
        """
        Generate operation-specific documentation.

        This provides focused documentation for a single operation, making it
        easier to understand the specific parameters and constraints.

        Args:
            operation: Name of the operation to document

        Returns:
            Markdown-formatted documentation string
        """
        definition = self.get_definition()
        operations = self.get_operation_definitions()

        # Validate operation exists
        if operation not in operations:
            available = ", ".join(sorted(operations.keys()))
            return f"Unknown operation '{operation}'. Available operations: {available}"

        op_def = operations[operation]
        sections = []

        sections.append(f"# {definition.name}.{operation}\n")
        sections.append(op_def.description)
        sections.append("")

        # Required parameters
        required_params = [
            param for param in definition.parameters
            if param.name in op_def.required_parameters
        ]

        if required_params:
            sections.append("## Required Parameters\n")
            for param in required_params:
                sections.append(f"### {param.name} ({param.type})")
                sections.append(param.description)
                if param.enum:
                    sections.append(f"\nValid values: {', '.join(param.enum)}")

                sections.append("")

        # Optional parameters
        optional_params = [
            param for param in definition.parameters
            if param.name in op_def.allowed_parameters
            and param.name not in op_def.required_parameters
        ]

        if optional_params:
            sections.append("## Optional Parameters\n")
            for param in optional_params:
                sections.append(f"### {param.name} ({param.type})")
                sections.append(param.description)
                if param.enum:
                    sections.append(f"\nValid values: {', '.join(param.enum)}")

                sections.append("")

        return "\n".join(sections)

    def _get_optional_str_value(
        self,
        key: str,
        arguments: Dict[str, Any],
        default: str | None = None
    ) -> str | None:
        """
        Extract optional string value from arguments dictionary.

        Helper for optional string parameters in operation handlers.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters
            default: Default value if key not present (defaults to None)

        Returns:
            String value, default value, or None

        Raises:
            AIToolExecutionError: If value exists but is not a string
        """
        if key not in arguments:
            return default

        value = arguments[key]
        if value is None:
            return None

        if not isinstance(value, str):
            raise AIToolExecutionError(f"'{key}' must be a string")

        return value

    def _get_optional_int_value(
        self,
        key: str,
        arguments: Dict[str, Any],
        default: int | None = None
    ) -> int | None:
        """
        Extract optional integer value from arguments dictionary.

        Helper for optional integer parameters in operation handlers.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters
            default: Default value if key not present (defaults to None)

        Returns:
            Integer value, default value, or None

        Raises:
            AIToolExecutionError: If value exists but is not an integer
        """
        if key not in arguments:
            return default

        value = arguments[key]
        if value is None:
            return None

        if not isinstance(value, int):
            raise AIToolExecutionError(f"'{key}' must be an integer")

        return value

    def _get_optional_bool_value(
        self,
        key: str,
        arguments: Dict[str, Any],
        default: bool = False
    ) -> bool:
        """
        Extract optional boolean value from arguments dictionary.

        Helper for optional boolean parameters in operation handlers.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters
            default: Default value if key not present (defaults to False)

        Returns:
            Boolean value or default

        Raises:
            AIToolExecutionError: If value exists but is not a boolean
        """
        if key not in arguments:
            return default

        value = arguments[key]
        if not isinstance(value, bool):
            raise AIToolExecutionError(f"'{key}' must be a boolean")

        return value

    def _get_optional_list_value(
        self,
        key: str,
        arguments: Dict[str, Any],
        default: list | None = None
    ) -> list | None:
        """
        Extract optional list value from arguments dictionary.

        Helper for optional list parameters in operation handlers.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters
            default: Default value if key not present (defaults to None)

        Returns:
            List value, default value, or None

        Raises:
            AIToolExecutionError: If value exists but is not a list
        """
        if key not in arguments:
            return default

        value = arguments[key]
        if value is None:
            return None

        if not isinstance(value, list):
            raise AIToolExecutionError(f"'{key}' must be a list")

        return value
