import asyncio
import json
import logging
import os
from typing import Dict, Any, List

from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_message import AIMessage
from ai.ai_message_source import AIMessageSource
from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback, AIToolOperationDefinition,
    AIToolResult
)
from humbug.column_manager import ColumnManager
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceNotFoundError, MindspaceError
from humbug.tabs.conversation.conversation_tab import ConversationTab
from humbug.user.user_manager import UserManager


class SystemAITool(AITool):
    """
    System operations tool for LLM interaction.

    Provides structured access to system operations like opening files, creating terminals,
    starting conversations, and accessing wiki views. All operations are restricted to
    the current mindspace and require user authorization.
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the system tool.

        Args:
            column_manager: Column manager for tab operations
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._user_manager = UserManager()
        self._logger = logging.getLogger("SystemAITool")

    def supports_continuations(self) -> bool:
        """
        Check if this tool supports returning continuations.

        Returns:
            True since this tool supports continuations for child conversations
        """
        return True

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        operations = self.get_operation_definitions()
        operation_names: List[str] = list(operations.keys())

        # Build description from operations
        base_description = (
            "The 'system' tool let's you (the AI) control the application user interface for the user. "
            "It does not give you the ability to control or use these tools directly."
        )

        # Generate operations list
        operation_list = []
        for name, op_def in operations.items():
            operation_list.append(f"- {name}: {op_def.description}")

        footer_description = (
            "All operations work within the current mindspace. "
            "Returns detailed information about created tabs, opened files, and operation results."
        )

        description = f"{base_description}\nAvailable operations are:\n" + "\n".join(operation_list) + f"\n{footer_description}"

        return AIToolDefinition(
            name="system",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="System operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="file_path",
                    type="string",
                    description="Path to file or directory (for open_editor, open_conversation, and open_wiki operations)",
                    required=False
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="ID of a tab (for tab_info, close_tab and move_tab operations)",
                    required=False
                ),
                AIToolParameter(
                    name="target_column",
                    type="integer",
                    description="Target column index (0-based) (for move_tab operation)",
                    required=False
                ),
                AIToolParameter(
                    name="model",
                    type="string",
                    description="AI model to use (for new_conversation and spawn_ai_child_conversation operations)",
                    required=False
                ),
                AIToolParameter(
                    name="temperature",
                    type="number",
                    description="Temperature setting 0.0-1.0 (for new_conversation and spawn_ai_child_conversation operations)",
                    required=False
                ),
                AIToolParameter(
                    name="message",
                    type="string",
                    description="Message to submit to conversation (for spawn_ai_child_conversation operation)",
                    required=False
                )
            ]
        )

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "open_editor_tab": AIToolOperationDefinition(
                name="open_editor_tab",
                handler=self._open_editor_tab,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="open a file in an editor tab"
            ),
            "new_terminal_tab": AIToolOperationDefinition(
                name="new_terminal_tab",
                handler=self._new_terminal_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="create a terminal tab for the user (you cannot run commands in this directly)"
            ),
            "open_conversation_tab": AIToolOperationDefinition(
                name="open_conversation_tab",
                handler=self._open_conversation_tab,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="open an existing conversation in a conversation tab"
            ),
            "new_conversation_tab": AIToolOperationDefinition(
                name="new_conversation_tab",
                handler=self._new_conversation_tab,
                allowed_parameters={"model", "temperature"},
                required_parameters=set(),
                description="start a new conversation in a conversation tab, with optional model/temperature - this conversation "
                    "cannot be used by you (the AI) to send messages, only the user can interact with it."
            ),
            "spawn_ai_child_conversation_tab": AIToolOperationDefinition(
                name="spawn_ai_child_conversation_tab",
                handler=self._spawn_ai_child_conversation_tab,
                allowed_parameters={"message", "model", "temperature"},
                required_parameters={"message"},
                description="start a new child conversation with an AI using a new prompt message - supports optional model/temperature, "
                    "the response will be the response from the child conversation"
            ),
            "show_system_shell_tab": AIToolOperationDefinition(
                name="show_system_shell_tab",
                handler=self._show_system_shell_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="open a system shell tab"
            ),
            "show_log_tab": AIToolOperationDefinition(
                name="show_log_tab",
                handler=self._show_log_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="open the mindspace log tab"
            ),
            "open_wiki_tab": AIToolOperationDefinition(
                name="open_wiki_tab",
                handler=self._open_wiki_tab,
                allowed_parameters={"file_path"},
                required_parameters=set(),
                description="open a file/directory in a wiki tab"
            ),
            "tab_info": AIToolOperationDefinition(
                name="tab_info",
                handler=self._tab_info,
                allowed_parameters={"tab_id"},
                required_parameters=set(),
                description="get information about a tab, given its ID (if no ID then gets the current tab info)"
            ),
            "close_tab": AIToolOperationDefinition(
                name="close_tab",
                handler=self._close_tab,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="close an existing tab"
            ),
            "list_tabs": AIToolOperationDefinition(
                name="list_tabs",
                handler=self._list_tabs,
                allowed_parameters=set(),
                required_parameters=set(),
                description="enumerate all currently open tabs across all columns"
            ),
            "move_tab": AIToolOperationDefinition(
                name="move_tab",
                handler=self._move_tab,
                allowed_parameters={"tab_id", "target_column"},
                required_parameters={"tab_id", "target_column"},
                description="move a tab to a specific column by index - there are a maximum of 6 columns"
            )
        }

    def _validate_mindspace_access(self) -> None:
        """
        Validate that a mindspace is currently open.

        Raises:
            AIToolExecutionError: If no mindspace is open
        """
        if not self._mindspace_manager.has_mindspace():
            raise AIToolExecutionError(
                "No mindspace is currently open. System operations require an active mindspace.",
                "system",
                {}
            )

    def _get_str_value_from_key(self, key: str, arguments: Dict[str, Any]) -> str:
        """
        Extract string value from arguments dictionary.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters

        Returns:
            String value for the given key

        Raises:
            AIToolExecutionError: If key is missing or value is not a string
        """
        if key not in arguments:
            raise AIToolExecutionError(
                f"No '{key}' argument provided",
                "system",
                arguments
            )

        value = arguments[key]
        if not isinstance(value, str):
            raise AIToolExecutionError(
                f"'{key}' must be a string",
                "system",
                arguments
            )

        return value

    def _get_int_value_from_key(self, key: str, arguments: Dict[str, Any]) -> int:
        """
        Extract integer value from arguments dictionary.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters

        Returns:
            Integer value for the given key

        Raises:
            AIToolExecutionError: If key is missing or value is not an integer
        """
        if key not in arguments:
            raise AIToolExecutionError(
                f"No '{key}' argument provided",
                "system",
                arguments
            )

        value = arguments[key]
        if not isinstance(value, int):
            raise AIToolExecutionError(
                f"'{key}' must be an integer",
                "system",
                arguments
            )

        return value

    def _validate_and_resolve_path(self, path_str: str, operation: str) -> str:
        """
        Validate path is within mindspace and resolve to absolute path.

        Args:
            path_str: String path to validate and resolve
            operation: Operation being performed (for error context)

        Returns:
            Resolved absolute path within mindspace

        Raises:
            AIToolExecutionError: If path is invalid or outside mindspace
        """
        if not path_str:
            raise AIToolExecutionError(
                "Path parameter is required",
                "system",
                {"operation": operation, "file_path": path_str}
            )

        try:
            # Check if our path starts with a separator.  If it does we'll assume it's for the root of the mindspace.
            if path_str.startswith(os.sep):
                path_str = path_str[1:]

            # Convert to absolute path via mindspace manager
            abs_path = self._mindspace_manager.get_absolute_path(path_str)

            # Verify the resolved path is still within mindspace
            relative_path = self._mindspace_manager.get_mindspace_relative_path(abs_path)
            if relative_path is None:
                raise AIToolExecutionError(
                    f"Path is outside mindspace boundaries: {path_str}",
                    "system",
                    {"operation": operation, "file_path": path_str}
                )

            return abs_path

        except MindspaceNotFoundError as e:
            raise AIToolExecutionError(
                f"Mindspace error: {str(e)}",
                "system",
                {"operation": operation, "file_path": path_str}
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Invalid path '{path_str}': {str(e)}",
                "system",
                {"operation": operation, "file_path": path_str}
            ) from e

    async def execute(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """
        Execute the system operation with proper validation and authorization.

        Args:
            arguments: Dictionary containing operation parameters
            request_authorization: Function to call for user authorization

        Returns:
            String result of the operation

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
        # Validate mindspace is open
        self._validate_mindspace_access()

        # Extract operation name
        operation = arguments.get("operation")
        if not operation:
            raise AIToolExecutionError(
                "No 'operation' argument provided",
                "system",
                arguments
            )

        if not isinstance(operation, str):
            raise AIToolExecutionError(
                "'operation' must be a string",
                "system",
                arguments
            )

        # Get operation definition
        operation_definitions = self.get_operation_definitions()
        if operation not in operation_definitions:
            available_operations = ", ".join(sorted(operation_definitions.keys()))
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}. Available operations: {available_operations}",
                "system",
                arguments
            )

        operation_def = operation_definitions[operation]

        self._logger.debug("System operation requested: %s", operation)

        try:
            result = await operation_def.handler(arguments, request_authorization)
            self._logger.info("System operation completed successfully: %s", operation)
            return result

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            # Re-raise our own errors
            raise

        except Exception as e:
            self._logger.error("Unexpected error in system operation '%s': %s", operation, str(e), exc_info=True)
            raise AIToolExecutionError(
                f"System operation failed: {str(e)}",
                "system",
                arguments
            ) from e

    async def execute_with_continuation(
        self,
        arguments: Dict[str, Any],
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the system operation with continuation support.

        Args:
            arguments: Dictionary containing operation parameters
            request_authorization: Function to call for user authorization

        Returns:
            AIToolResult containing the execution result and optional continuation

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
        # Get the tool call ID
        tool_call_id = arguments.get('_tool_call_id', 'unknown')

        # Check if this is the spawn_ai_child_conversation_tab operation
        operation = arguments.get("operation")
        if operation == "spawn_ai_child_conversation_tab":
            return await self._spawn_ai_child_conversation_tab_with_continuation(arguments, request_authorization, tool_call_id)

        # For all other operations, use the regular execute method
        content = await self.execute(arguments, request_authorization)
        return AIToolResult(
            id=tool_call_id,
            name="system",
            content=content
        )

    async def _wait_for_completion(self, conversation_tab: ConversationTab, message: str) -> str:
        """
        Wait for a conversation tab to complete and return the formatted result.

        Args:
            conversation_tab: The conversation tab to wait for
            message: The original message that was submitted

        Returns:
            Formatted result string containing the AI's response or error information
        """
        # Set up completion tracking
        completion_future: asyncio.Future[Dict[str, Any]] = asyncio.Future()

        def on_completion(result_dict: Dict[str, Any]) -> None:
            """Handle conversation completion."""
            if not completion_future.done():
                completion_future.set_result(result_dict)

        try:
            # Connect to completion signal
            conversation_tab.conversation_completed.connect(on_completion)

            # Wait for completion
            result = await completion_future

            # Log the sub-conversation completion
            tab_id = conversation_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Child conversation completed, tab ID: {tab_id}"
            )

            # Return appropriate result
            if result.get("success", False):
                response_content = result.get("content", "")
                usage_info = result.get("usage")

                # Create a formatted response
                result_parts = [f"Child conversation completed successfully:\n{response_content}"]

                if usage_info:
                    result_parts.append(f"Token usage: {usage_info['prompt_tokens']} prompt + {usage_info['completion_tokens']} completion = {usage_info['total_tokens']} total")

                return "\n\n".join(result_parts)

            error_msg = result.get("error", "Unknown error")
            self._logger.warning("Child conversation failed: %s", error_msg)
            return f"Child conversation failed: {error_msg}"

        finally:
            # Clean up signal connection
            conversation_tab.conversation_completed.disconnect(on_completion)

    async def _spawn_ai_child_conversation_tab_with_continuation(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback,
        tool_call_id: str
    ) -> AIToolResult:
        """
        Submit a message to a new AI conversation and return a continuation for waiting.

        Args:
            arguments: Dictionary containing operation parameters
            _request_authorization: Authorization callback (unused for this operation)
            tool_call_id: ID of the tool call

        Returns:
            AIToolResult with continuation for waiting on completion

        Raises:
            AIToolExecutionError: If operation fails
        """
        # Extract required message parameter
        message = self._get_str_value_from_key("message", arguments)

        # Extract optional parameters
        model = arguments.get("model")
        temperature = arguments.get("temperature")

        # Validate model if provided
        if model and not isinstance(model, str):
            raise AIToolExecutionError(
                "'model' must be a string",
                "system",
                arguments
            )

        # Validate temperature if provided
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError(
                    "'temperature' must be a number",
                    "system",
                    arguments
                )

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError(
                    "'temperature' must be between 0.0 and 1.0",
                    "system",
                    arguments
                )

        # Validate model exists if provided
        reasoning = None
        if model:
            ai_backends = self._user_manager.get_ai_backends()
            available_models = list(AIConversationSettings.iter_models_by_backends(ai_backends))

            if model not in available_models:
                raise AIToolExecutionError(
                    f"Model '{model}' is not available. Available models: {', '.join(available_models)}",
                    "system",
                    arguments
                )

            # Get reasoning capability from model
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        try:
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Create conversation
            self._column_manager.protect_current_tab(True)
            conversation_tab: ConversationTab | None = None
            try:
                conversation_tab = self._column_manager.new_conversation(model, temperature, reasoning)
                conversation_tab.set_sub_conversation_mode(True)
                conversation_tab.set_input_text(message)
                conversation_tab.submit()

                # Log the sub-conversation creation
                tab_id = conversation_tab.tab_id()
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI spawned child conversation, tab ID: {tab_id}, and submitted message: '{message[:50]}...'"
                )

                # Create a continuation task that waits for completion
                continuation_task = asyncio.create_task(self._wait_for_completion(conversation_tab, message))

                return AIToolResult(
                    id=tool_call_id,
                    name="system",
                    content=f"Started child conversation, tab ID: {tab_id}, and submitted message: '{message[:50]}...'",
                    continuation=continuation_task
                )

            finally:
                self._column_manager.protect_current_tab(False)

        except MindspaceError as e:
            raise AIToolExecutionError(
                f"Failed to create conversation directory: {str(e)}",
                "system",
                arguments
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to spawn child AI conversation: {str(e)}",
                "system",
                arguments
            ) from e

    async def _spawn_ai_child_conversation_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """
        Submit a message to a new AI conversation and wait for the response.

        Args:
            arguments: Dictionary containing operation parameters
            _request_authorization: Authorization callback (unused for this operation)

        Returns:
            String result containing the AI's response

        Raises:
            AIToolExecutionError: If operation fails
        """
        # Extract required message parameter
        message = self._get_str_value_from_key("message", arguments)

        # Extract optional parameters
        model = arguments.get("model")
        temperature = arguments.get("temperature")

        # Validate model if provided
        if model and not isinstance(model, str):
            raise AIToolExecutionError(
                "'model' must be a string",
                "system",
                arguments
            )

        # Validate temperature if provided
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError(
                    "'temperature' must be a number",
                    "system",
                    arguments
                )

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError(
                    "'temperature' must be between 0.0 and 1.0",
                    "system",
                    arguments
                )

        # Validate model exists if provided
        reasoning = None
        if model:
            ai_backends = self._user_manager.get_ai_backends()
            available_models = list(AIConversationSettings.iter_models_by_backends(ai_backends))

            if model not in available_models:
                raise AIToolExecutionError(
                    f"Model '{model}' is not available. Available models: {', '.join(available_models)}",
                    "system",
                    arguments
                )

            # Get reasoning capability from model
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        try:
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Create conversation
            self._column_manager.protect_current_tab(True)
            conversation_tab: ConversationTab | None = None
            try:
                conversation_tab = self._column_manager.new_conversation(model, temperature, reasoning)
                conversation_tab.set_sub_conversation_mode(True)
                conversation_tab.set_input_text(message)
                conversation_tab.submit()

                # Wait for completion and return result
                return await self._wait_for_completion(conversation_tab, message)

            finally:
                self._column_manager.protect_current_tab(False)

        except MindspaceError as e:
            raise AIToolExecutionError(
                f"Failed to create conversation directory: {str(e)}",
                "system",
                arguments
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to spawn child AI conversation: {str(e)}",
                "system",
                arguments
            ) from e

    async def _open_editor_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Edit or create a file in an editor tab."""
        file_path_arg = self._get_str_value_from_key("file_path", arguments)
        file_path = self._validate_and_resolve_path(file_path_arg, "open_editor")

        try:
            # Create parent directories if needed
            directory = os.path.dirname(file_path)
            if directory and not os.path.exists(directory):
                os.makedirs(directory, exist_ok=True)

            # Open the file in editor
            self._column_manager.protect_current_tab(True)
            try:
                editor_tab = self._column_manager.open_file(file_path)

            finally:
                self._column_manager.protect_current_tab(False)

            relative_path = self._mindspace_manager.get_relative_path(file_path)
            tab_id = editor_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened editor for file: '{relative_path}', tab ID: {tab_id}"
            )
            return f"Opened editor tab for file: '{relative_path}', tab ID: {tab_id}"

        except OSError as e:
            raise AIToolExecutionError(
                f"Failed to access file '{file_path_arg}': {str(e)}",
                "system",
                arguments
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to open file '{file_path_arg}' for editing: {str(e)}",
                "system",
                arguments
            ) from e

    async def _new_terminal_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Create a new terminal tab."""
        try:
            self._column_manager.protect_current_tab(True)
            try:
                terminal_tab = self._column_manager.new_terminal()

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = terminal_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI created new terminal, tab ID: {tab_id}"
            )
            return f"Created new terminal, tab ID: {tab_id}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to create terminal: {str(e)}",
                "system",
                arguments
            ) from e

    async def _open_conversation_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Create a new conversation tab."""
        # Get file path
        file_path_arg = self._get_str_value_from_key("file_path", arguments)
        conversation_path = self._validate_and_resolve_path(file_path_arg, "open_conversation")

        try:
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Create conversation
            self._column_manager.protect_current_tab(True)
            try:
                conversation_tab = self._column_manager.open_conversation(conversation_path, False)

            finally:
                self._column_manager.protect_current_tab(False)

            if conversation_tab is None:
                raise AIToolExecutionError(
                    f"Conversation file '{file_path_arg}' does not exist or is not a valid conversation.",
                    "system",
                    arguments
                )

            tab_id = conversation_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened conversation for: '{conversation_path}', tab ID: {tab_id}"
            )
            return f"Opened conversation for: '{conversation_path}', tab ID: {tab_id}"

        except MindspaceError as e:
            raise AIToolExecutionError(
                f"Failed to create conversation directory: {str(e)}",
                "system",
                arguments
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to create conversation: {str(e)}",
                "system",
                arguments
            ) from e

    async def _new_conversation_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Create a new conversation tab."""
        # Extract optional parameters
        model = arguments.get("model")
        temperature = arguments.get("temperature")

        # Validate model if provided
        if model and not isinstance(model, str):
            raise AIToolExecutionError(
                "'model' must be a string",
                "system",
                arguments
            )

        # Validate temperature if provided
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError(
                    "'temperature' must be a number",
                    "system",
                arguments
            )

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError(
                    "'temperature' must be between 0.0 and 1.0",
                    "system",
                    arguments
                )

        # Validate model exists if provided
        reasoning = None
        if model:
            ai_backends = self._user_manager.get_ai_backends()
            available_models = list(AIConversationSettings.iter_models_by_backends(ai_backends))

            if model not in available_models:
                raise AIToolExecutionError(
                    f"Model '{model}' is not available. Available models: {', '.join(available_models)}",
                    "system",
                    arguments
                )

            # Get reasoning capability from model
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        try:
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Create conversation
            self._column_manager.protect_current_tab(True)
            try:
                conversation_tab = self._column_manager.new_conversation(model, temperature, reasoning)
                conversation_tab.set_sub_conversation_mode(False)

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = conversation_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI created new conversation, tab ID: {tab_id}"
            )
            result_parts = [f"Created new conversation, tab ID: {tab_id}"]
            if model:
                result_parts.append(f"model: {model}")

            if temperature is not None:
                result_parts.append(f"temperature: {temperature}")

            return ", ".join(result_parts)

        except MindspaceError as e:
            raise AIToolExecutionError(
                f"Failed to create conversation directory: {str(e)}",
                "system",
                arguments
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to create conversation: {str(e)}",
                "system",
                arguments
            ) from e

    async def _show_system_shell_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Show the system shell tab."""
        try:
            self._column_manager.protect_current_tab(True)

            try:
                shell_tab = self._column_manager.show_system_shell()

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = shell_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened system shell, tab ID: {tab_id}"
            )
            return f"Opened system shell, tab ID: {tab_id}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to show system shell: {str(e)}",
                "system",
                arguments
            ) from e

    async def _show_log_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Show the system shell tab."""
        try:
            self._column_manager.protect_current_tab(True)

            try:
                shell_tab = self._column_manager.show_system_log()

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = shell_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened mindspace log, tab ID: {tab_id}"
            )
            return f"Opened mindspace log, tab ID: {tab_id}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to show system shell: {str(e)}",
                "system",
                arguments
            ) from e

    async def _open_wiki_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Open wiki view for a specific location or mindspace root."""
        # Get file path (optional)
        file_path_arg = arguments.get("file_path", "")

        if file_path_arg:
            wiki_path = self._validate_and_resolve_path(file_path_arg, "open_wiki")

        else:
            # Use mindspace root if no path provided
            wiki_path = self._mindspace_manager.get_absolute_path(".")

        try:
            # Open wiki page
            self._column_manager.protect_current_tab(True)
            try:
                wiki_tab = self._column_manager.open_wiki_page(wiki_path, False)

            finally:
                self._column_manager.protect_current_tab(False)

            relative_path = self._mindspace_manager.get_relative_path(wiki_path)
            location = relative_path if relative_path else "."

            tab_id = wiki_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened wiki tab for: '{location}', tab ID: {tab_id}"
            )
            return f"Opened wiki tab for: '{location}', tab ID: {tab_id}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to open wiki: {str(e)}",
                "system",
                arguments
            ) from e

    async def _tab_info(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Get information about a specific tab by ID or the current tab."""
        tab_id = arguments.get("tab_id")

        try:
            # If no tab ID provided, use the current tab
            if not tab_id:
                current_tab = self._column_manager.get_current_tab()
                if not current_tab:
                    raise AIToolExecutionError(
                        "No current tab is open",
                        "system",
                        arguments
                    )

                tab_id = current_tab.tab_id()

            # Validate tab_id is a string if provided
            if not isinstance(tab_id, str):
                raise AIToolExecutionError(
                    "'tab_id' must be a string",
                    "system",
                    arguments
                )

            # Get tab info
            tab_info = self._column_manager.get_tab_info_by_id(tab_id)
            if not tab_info:
                raise AIToolExecutionError(
                    f"No tab found with ID: {tab_id}",
                    "system",
                    arguments
                )

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested info for tab ID: {tab_id}"
            )
            return f"Tab info for ID {tab_id}:\n{json.dumps(tab_info, indent=2)}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to get tab info for ID {tab_id}: {str(e)}",
                "system",
                arguments
            ) from e

    async def _close_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Close an existing tab by ID."""
        tab_id = self._get_str_value_from_key("tab_id", arguments)

        try:
            self._column_manager.close_tab_by_id(tab_id)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI closed tab, tab ID: {tab_id}"
            )
            return f"Closed tab, tab ID: {tab_id}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to close tab {tab_id}: {str(e)}",
                "system",
                arguments
            ) from e

    async def _list_tabs(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """List all currently open tabs across all columns."""
        try:
            tab_info = self._column_manager.list_all_tabs()

            if not tab_info:
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    "AI requested tab list: no tabs currently open"
                )
                return "No tabs are currently open."

            # Format the response as a structured JSON string for easy parsing
            result = {
                "total_tabs": len(tab_info),
                "total_columns": self._column_manager.num_colunns(),
                "tabs": tab_info
            }

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested tab list: {len(tab_info)} tabs across {result['total_columns']} columns"
            )

            return f"Current tabs:\n{json.dumps(result, indent=2)}"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to list tabs: {str(e)}",
                "system",
                arguments
            ) from e

    async def _move_tab(
        self,
        arguments: Dict[str, Any],
        _request_authorization: AIToolAuthorizationCallback
    ) -> str:
        """Move a tab to a specific column."""
        tab_id = self._get_str_value_from_key("tab_id", arguments)
        target_column = self._get_int_value_from_key("target_column", arguments)

        try:
            # Validate target column is non-negative
            if target_column < 0:
                raise AIToolExecutionError(
                    f"Target column must be non-negative, got {target_column}",
                    "system",
                    arguments
                )

            # Attempt to move the tab
            success = self._column_manager.move_tab_to_column(tab_id, target_column)

            if success:
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI moved tab {tab_id} to column {target_column}"
                )
                return f"Successfully moved tab {tab_id} to column {target_column}"

            # If not successful, it means the tab was already in the target column
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI attempted to move tab {tab_id} to column {target_column} (already in target column)"
            )
            return f"Tab {tab_id} was already in column {target_column}"

        except ValueError as e:
            raise AIToolExecutionError(
                str(e),
                "system",
                arguments
            ) from e

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to move tab {tab_id} to column {target_column}: {str(e)}",
                "system",
                arguments
            ) from e
