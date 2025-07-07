import logging
import os
from typing import Dict, Any

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_tool_manager import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback
)
from humbug.gui.column_manager import ColumnManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceNotFoundError, MindspaceError
from humbug.user.user_manager import UserManager


class AIToolSystem(AITool):
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
        self._logger = logging.getLogger("AIToolSystem")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="system",
            description=(
                "Control the application user interface on behalf of the user. Available operations: "
                "open_editor_tab (open a file in an editor tab), "
                "new_terminal_tab (create a terminal tab), "
                "open_conversation_tab (open an existing AI conversation in a conversation tab), "
                "new_conversation_tab (start a new AI conversation in a conversation tab, with optional model/temperature), "
                "show_system_shell_tab (open a system shell tab), "
                "open_wiki_tab (open a file/direcotry in a wiki tab). "
                "close_tab (close an existing tab by ID). "
                "All operations work within the current mindspace. "
                "Returns detailed information about created tabs, opened files, and operation results."
            ),
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="System operation to perform",
                    required=True,
                    enum=[
                        "open_editor_tab",
                        "new_terminal_tab",
                        "open_conversation_tab",
                        "new_conversation_tab",
                        "show_system_shell_tab",
                        "open_wiki_tab",
                        "close_tab"
                    ]
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
                    description="ID of a tab (for close_tab operation)",
                    required=False
                ),
                AIToolParameter(
                    name="model",
                    type="string",
                    description="AI model to use (for new_conversation)",
                    required=False
                ),
                AIToolParameter(
                    name="temperature",
                    type="number",
                    description="Temperature setting 0.0-1.0 (for new_conversation)",
                    required=False
                )
            ]
        )

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

        # Extract and validate operation
        operation = self._get_str_value_from_key("operation", arguments)

        # Route to specific operation handler
        handlers = {
            "open_editor_tab": self._open_editor_tab,
            "new_terminal_tab": self._new_terminal_tab,
            "open_conversation_tab": self._open_conversation_tab,
            "new_conversation_tab": self._new_conversation_tab,
            "show_system_shell_tab": self._show_system_shell_tab,
            "open_wiki_tab": self._open_wiki_tab,
            "close_tab": self._close_tab
        }

        if operation not in handlers:
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}",
                "system",
                arguments
            )

        self._logger.debug("System operation requested: %s", operation)

        try:
            result = await handlers[operation](arguments, request_authorization)
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
            editor_tab = self._column_manager.open_file(file_path)
            self._column_manager.protect_current_tab(False)

            relative_path = self._mindspace_manager.get_relative_path(file_path)
            return f"Opened editor tab for file: {relative_path} (tab ID: {editor_tab.tab_id()})"

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
            terminal_tab = self._column_manager.new_terminal()
            self._column_manager.protect_current_tab(False)
            return f"Created new terminal tab (tab ID: {terminal_tab.tab_id()})"

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
            conversation_tab = self._column_manager.open_conversation(conversation_path, False)
            self._column_manager.protect_current_tab(False)

            if conversation_tab is None:
                raise AIToolExecutionError(
                    f"Conversation file '{file_path_arg}' does not exist or is not a valid conversation.",
                    "system",
                    arguments
                )

            return f"Created new conversation tab (tab ID: {conversation_tab.tab_id()})"

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
            conversation_tab = self._column_manager.new_conversation(model, temperature, reasoning)
            self._column_manager.protect_current_tab(False)

            result_parts = [f"Created new conversation tab (tab ID: {conversation_tab.tab_id()})"]
            if model:
                result_parts.append(f"Model: {model}")

            if temperature is not None:
                result_parts.append(f"Temperature: {temperature}")

            return ". ".join(result_parts)

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
            shell_tab = self._column_manager.show_system_shell()
            self._column_manager.protect_current_tab(False)
            return f"Opened system shell tab (tab ID: {shell_tab.tab_id()})"

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
            wiki_tab = self._column_manager.open_wiki_page(wiki_path, False)
            self._column_manager.protect_current_tab(False)

            relative_path = self._mindspace_manager.get_relative_path(wiki_path)
            location = relative_path if relative_path else "."
            return f"Opened wiki tab for: {location} (tab ID: {wiki_tab.tab_id()})"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to open wiki: {str(e)}",
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
            return f"Closed tab (tab ID: {tab_id})"

        except Exception as e:
            raise AIToolExecutionError(
                f"Failed to close tab {tab_id}: {str(e)}",
                "system",
                arguments
            ) from e
