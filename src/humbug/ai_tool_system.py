import json
import logging
import os
from typing import Dict, Any

from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_tool_manager import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback
)
from humbug.column_manager import ColumnManager
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
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
                "The 'system' tool let's you (the AI) control the application user interface for the user. "
                "It does not give you the ability to control or use these tools directly. "
                "Available operations are:\n"
                "- open_editor_tab: open a file in an editor tab\n"
                "- new_terminal_tab: create a terminal tab for the user (you cannot run commands in this directly)\n"
                "- open_conversation_tab: open an existing AI conversation in a conversation tab\n"
                "- new_conversation_tab: start a new AI conversation in a conversation tab, with optional model/temperature\n"
                "- show_system_shell_tab: open a system shell tab)\n"
                "- show_log_tab: open the mindspace log tab)\n"
                "- open_wiki_tab: open a file/direcotry in a wiki tab)\n"
                "- tab_info: get information about a tab, given its ID (if no ID then gets the current tab info)\n"
                "- close_tab: close an existing tab\n"
                "- list_tabs: enumerate all currently open tabs across all columns)\n"
                "- move_tab: move a tab to a specific column by index - there are a maximum of 6 columns\n"
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
                        "show_log_tab",
                        "open_wiki_tab",
                        "tab_info",
                        "close_tab",
                        "list_tabs",
                        "move_tab"
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

        # Extract and validate operation
        operation = self._get_str_value_from_key("operation", arguments)

        # Route to specific operation handler
        handlers = {
            "open_editor_tab": self._open_editor_tab,
            "new_terminal_tab": self._new_terminal_tab,
            "open_conversation_tab": self._open_conversation_tab,
            "new_conversation_tab": self._new_conversation_tab,
            "show_system_shell_tab": self._show_system_shell_tab,
            "show_log_tab": self._show_log_tab,
            "open_wiki_tab": self._open_wiki_tab,
            "tab_info": self._tab_info,
            "close_tab": self._close_tab,
            "list_tabs": self._list_tabs,
            "move_tab": self._move_tab
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

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = conversation_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI created new conversation, tab ID: {tab_id}"
            )
            result_parts = [f"Created new conversation, tab ID: {tab_id}"]
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
        tab_id = self._get_str_value_from_key("tab_id", arguments) if "tab_id" in arguments else None

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
