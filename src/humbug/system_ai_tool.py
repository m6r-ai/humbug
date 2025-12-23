import logging
import json
import os
from typing import Any, Dict

from ai import AIConversationSettings
from ai_tool import (
    AITool,
    AIToolAuthorizationCallback,
    AIToolCall,
    AIToolDefinition,
    AIToolExecutionError,
    AIToolOperationDefinition,
    AIToolParameter,
    AIToolResult,
)
from humbug.mindspace.mindspace_error import MindspaceError, MindspaceNotFoundError
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.column_manager_error import ColumnManagerError
from humbug.user.user_manager import UserManager


class SystemAITool(AITool):
    """
    System operations tool for tab lifecycle management.

    Provides operations for creating, opening, closing, and organizing tabs.
    This is the primary tool for managing the workspace layout. Use specific
    tab tools (editor, terminal, conversation, log, preview) for working with
    tab content.
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

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="system",
            description_prefix=(
            "Tab lifecycle and workspace management operations. Use this tool to create, open, "
            "close, and organize tabs. Returns tab IDs (GUIDs) that can be used with specific tab tools "
                "(editor, terminal, conversation, log, preview) to work with tab content."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="file_path",
                    type="string",
                    description="Path to file or directory (for open_editor_tab, open_conversation_tab, open_preview_tab)",
                    required=False
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of tab (for get_tab_info, close_tab, move_tab operations)",
                    required=False
                ),
                AIToolParameter(
                    name="target_column",
                    type="integer",
                    description="Target column index (0-based) for move_tab operation. Maximum 6 columns",
                    required=False
                ),
                AIToolParameter(
                    name="model",
                    type="string",
                    description="AI model to use (for new_conversation_tab operation)",
                    required=False
                ),
                AIToolParameter(
                    name="temperature",
                    type="number",
                    description="Temperature setting 0.0-1.0 (for new_conversation_tab operation)",
                    required=False
                ),
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
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open a file in an editor tab for the user to browse/edit. "
                    "Returns the tab GUID for use with the editor tool"
            ),
            "new_terminal_tab": AIToolOperationDefinition(
                name="new_terminal_tab",
                handler=self._new_terminal_tab,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Create a fully interactive terminal tab. "
                "This provides a terminal emulator connected to a new shell. "
                "You may interact with this terminal using the terminal tool. "
                    "Returns the tab GUID for use with the terminal tool"
            ),
            "open_conversation_tab": AIToolOperationDefinition(
                name="open_conversation_tab",
                handler=self._open_conversation_tab,
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open an existing conversation in a conversation tab for the user to browse/edit. "
                "You (the AI) cannot use this to send messages. "
                    "Returns the tab GUID for use with the conversation tool"
            ),
            "new_conversation_tab": AIToolOperationDefinition(
                name="new_conversation_tab",
                handler=self._new_conversation_tab,
                extract_context=None,
                allowed_parameters={"model", "temperature"},
                required_parameters=set(),
                description="Create a new AI conversation tab, with optional model/temperature. "
                    "Returns the tab GUID for use with the conversation tool"
            ),
            "show_system_shell_tab": AIToolOperationDefinition(
                name="show_system_shell_tab",
                handler=self._show_system_shell_tab,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Open a system shell tab. "
                    "Returns the tab GUID for use with the terminal tool"
            ),
            "show_log_tab": AIToolOperationDefinition(
                name="show_log_tab",
                handler=self._show_log_tab,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Open the mindspace log tab. "
                    "Returns the tab GUID for use with the log tool"
            ),
            "open_preview_tab": AIToolOperationDefinition(
                name="open_preview_tab",
                handler=self._open_preview_tab,
                extract_context=None,
                allowed_parameters={"file_path"},
                required_parameters=set(),
                description="Open a file/directory in a preview view tab. "
                    "If no file_path provided, opens mindspace root. "
                    "Returns the tab GUID for use with the preview tool"
            ),
            "get_tab_info": AIToolOperationDefinition(
                name="get_tab_info",
                handler=self._get_tab_info,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters=set(),
                description="Get information about a tab, given its tab GUID. "
                    "If no tab_id provided, gets the current tab info"
            ),
            "close_tab": AIToolOperationDefinition(
                name="close_tab",
                handler=self._close_tab,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Close an existing tab. You must provide the tab_id parameter"
            ),
            "list_tabs": AIToolOperationDefinition(
                name="list_tabs",
                handler=self._list_tabs,
                extract_context=None,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Enumerate all currently open tabs across all columns"
            ),
            "move_tab": AIToolOperationDefinition(
                name="move_tab",
                handler=self._move_tab,
                extract_context=None,
                allowed_parameters={"tab_id", "target_column"},
                required_parameters={"tab_id", "target_column"},
                description="Move a tab to a specific column by index. You must provide the tab_id and target_column "
                    "parameters. There are a maximum of 6 columns"
            ),
        }

    def _validate_mindspace_access(self) -> None:
        """
        Validate that a mindspace is currently open.

        Raises:
            AIToolExecutionError: If no mindspace is open
        """
        if not self._mindspace_manager.has_mindspace():
            raise AIToolExecutionError(
                "No mindspace is currently open. System operations require an active mindspace."
            )

    def _validate_and_resolve_path(self, path_str: str) -> str:
        """
        Validate path is within mindspace and resolve to absolute path.

        Args:
            path_str: String path to validate and resolve

        Returns:
            Resolved absolute path within mindspace

        Raises:
            AIToolExecutionError: If path is invalid or outside mindspace
        """
        if not path_str:
            raise AIToolExecutionError("Path parameter is required")

        try:
            # Check if our path starts with a separator - assume it's for the root of the mindspace
            if path_str.startswith(os.sep):
                path_str = path_str[1:]

            # Convert to absolute path via mindspace manager
            abs_path = self._mindspace_manager.get_absolute_path(path_str)

            # Verify the resolved path is still within mindspace
            relative_path = self._mindspace_manager.get_mindspace_relative_path(abs_path)
            if relative_path is None:
                raise AIToolExecutionError(f"Path is outside mindspace boundaries: {path_str}")

            return abs_path

        except MindspaceNotFoundError as e:
            raise AIToolExecutionError(f"Mindspace error: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Invalid path '{path_str}': {str(e)}") from e

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute a system operation.

        Validates mindspace access before delegating to base class.

        Args:
            tool_call: Tool call containing operation name and arguments
            requester_ref: Reference to the requester
            request_authorization: Function to call for user authorization

        Returns:
            AIToolResult containing the execution result

        Raises:
            AIToolExecutionError: If operation fails
        """
        # Validate mindspace is open before any operation
        self._validate_mindspace_access()

        # Delegate to base class for operation routing
        return await super().execute(tool_call, requester_ref, request_authorization)

    async def _open_editor_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open or create a file in an editor tab."""
        arguments = tool_call.arguments

        file_path_arg = self._get_required_str_value("file_path", arguments)

        file_path = self._validate_and_resolve_path(file_path_arg)

        try:
            # Create parent directories if needed
            directory = os.path.dirname(file_path)
            if directory and not os.path.exists(directory):
                os.makedirs(directory, exist_ok=True)

            # Open the file in editor
            self._column_manager.protect_current_tab(True)
            try:
                editor_tab = self._column_manager.open_file(file_path, False)

            finally:
                self._column_manager.protect_current_tab(False)

            relative_path = self._mindspace_manager.get_relative_path(file_path)
            tab_id = editor_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened editor for file: '{relative_path}'\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened editor tab for file: '{relative_path}', tab ID: {tab_id}"
            )

        except OSError as e:
            raise AIToolExecutionError(f"Failed to access file '{file_path_arg}': {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open file '{file_path_arg}' for editing: {str(e)}") from e

    async def _new_terminal_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
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
                f"AI created new terminal\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Created new terminal, tab ID: {tab_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to create terminal: {str(e)}") from e

    async def _open_conversation_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open an existing conversation tab."""
        arguments = tool_call.arguments

        file_path_arg = self._get_required_str_value("file_path", arguments)

        conversation_path = self._validate_and_resolve_path(file_path_arg)

        try:
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Open conversation
            self._column_manager.protect_current_tab(True)
            try:
                conversation_tab = self._column_manager.open_conversation(conversation_path, False)

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = conversation_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened conversation for: '{conversation_path}'\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened conversation for: '{conversation_path}', tab ID: {tab_id}"
            )

        except MindspaceError as e:
            raise AIToolExecutionError(f"Failed to create conversation directory: {str(e)}") from e

        except ColumnManagerError as e:
            raise AIToolExecutionError(f"Failed to open conversation: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open conversation: {str(e)}") from e

    async def _new_conversation_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Create a new conversation tab."""
        arguments = tool_call.arguments
        model = self._get_optional_str_value("model", arguments)
        temperature = arguments.get("temperature")

        # Validate temperature if provided
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError("'temperature' must be a number")

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError("'temperature' must be between 0.0 and 1.0")

        # Validate model exists if provided
        reasoning = None
        if model:
            ai_backends = self._user_manager.get_ai_backends()
            available_models = list(AIConversationSettings.iter_models_by_backends(ai_backends))

            if model not in available_models:
                raise AIToolExecutionError(
                    f"Model '{model}' is not available. Available models: {', '.join(available_models)}"
                )

            # Get reasoning capability from model
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        try:
            self._column_manager.protect_current_tab(True)

            # Create conversation
            try:
                self._mindspace_manager.ensure_mindspace_dir("conversations")
                conversation_tab = self._column_manager.new_conversation(False, None, model, temperature, reasoning)

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = conversation_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI created new conversation\ntab ID: {tab_id}"
            )
            result_parts = [f"Created new conversation, tab ID: {tab_id}"]
            if model:
                result_parts.append(f"model: {model}")

            if temperature is not None:
                result_parts.append(f"temperature: {temperature}")

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=", ".join(result_parts)
            )

        except (MindspaceError, ColumnManagerError) as e:
            raise AIToolExecutionError(f"Failed to create conversation: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to create conversation: {str(e)}") from e

    async def _show_system_shell_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
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
                f"AI opened system shell\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened system shell, tab ID: {tab_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to show system shell: {str(e)}") from e

    async def _show_log_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Show the mindspace log tab."""
        try:
            self._column_manager.protect_current_tab(True)
            try:
                log_tab = self._column_manager.show_system_log()

            finally:
                self._column_manager.protect_current_tab(False)

            tab_id = log_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened mindspace log\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened mindspace log, tab ID: {tab_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to show mindspace log: {str(e)}") from e

    async def _open_preview_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open preview view for a specific location or mindspace root."""
        arguments = tool_call.arguments
        file_path_arg = arguments.get("file_path", "")

        if file_path_arg:
            preview_path = self._validate_and_resolve_path(file_path_arg)

        else:
            # Use mindspace root if no path provided
            preview_path = self._mindspace_manager.get_absolute_path(".")

        try:
            # Open preview page
            self._column_manager.protect_current_tab(True)
            try:
                preview_tab = self._column_manager.open_preview_page(preview_path, False)

            finally:
                self._column_manager.protect_current_tab(False)

            relative_path = self._mindspace_manager.get_relative_path(preview_path)
            location = relative_path if relative_path else "."

            tab_id = preview_tab.tab_id()
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI opened preview tab for: '{location}'\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened preview tab for: '{location}', tab ID: {tab_id}"
            )

        except ColumnManagerError as e:
            raise AIToolExecutionError(f"Failed to open preview: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open preview: {str(e)}") from e

    async def _get_tab_info(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get information about a specific tab by ID or the current tab."""
        arguments = tool_call.arguments
        tab_id = arguments.get("tab_id")

        try:
            # If no tab ID provided, use the current tab
            if not tab_id:
                current_tab = self._column_manager.get_current_tab()
                if not current_tab:
                    raise AIToolExecutionError("No current tab is open")

                tab_id = current_tab.tab_id()

            # Validate tab_id is a string if provided
            if not isinstance(tab_id, str):
                raise AIToolExecutionError("'tab_id' must be a string")

            # Get tab info
            tab_info = self._column_manager.get_tab_info_by_id(tab_id)
            if not tab_info:
                raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested info for tab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=json.dumps(tab_info, indent=2),
                context="json"
            )

        except AIToolExecutionError:
            raise

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get tab info for ID {tab_id}: {str(e)}") from e

    async def _close_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Close an existing tab by ID."""
        arguments = tool_call.arguments

        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        try:
            self._column_manager.close_tab_by_id(tab_id)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI closed tab\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content="Closed tab"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to close tab {tab_id}: {str(e)}") from e

    async def _list_tabs(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """List all currently open tabs across all columns."""
        try:
            tab_info = self._column_manager.list_all_tabs()

            if not tab_info:
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    "AI requested tab list: no tabs currently open"
                )
                return AIToolResult(
                    id=tool_call.id,
                    name="system",
                    content="No tabs are currently open."
                )

            # Format the response as a structured result
            result = {
                "total_tabs": len(tab_info),
                "total_columns": self._column_manager.num_colunns(),
                "tabs": tab_info
            }

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested tab list: {len(tab_info)} tabs across {result['total_columns']} columns"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=json.dumps(result, indent=2),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to list tabs: {str(e)}") from e

    async def _move_tab(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Move a tab to a specific column."""
        arguments = tool_call.arguments

        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        if "target_column" not in arguments:
            raise AIToolExecutionError("No 'target_column' argument provided")

        target_column = arguments["target_column"]
        if not isinstance(target_column, int):
            raise AIToolExecutionError("'target_column' must be an integer")

        try:
            # Validate target column is non-negative
            if target_column < 0:
                raise AIToolExecutionError(f"Target column must be non-negative, got {target_column}")

            # Attempt to move the tab
            success = self._column_manager.move_tab_to_column(tab_id, target_column)
            if not success:
                # If not successful, it means the tab was already in the target column
                self._mindspace_manager.add_interaction(
                    MindspaceLogLevel.INFO,
                    f"AI attempted to move tab {tab_id} to column {target_column} (already in target column)"
                )
                return AIToolResult(
                    id=tool_call.id,
                    name="system",
                    content=f"Tab {tab_id} was already in column {target_column}"
                )

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI moved tab {tab_id} to column {target_column}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Successfully moved tab {tab_id} to column {target_column}"
            )

        except ValueError as e:
            raise AIToolExecutionError(str(e)) from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to move tab {tab_id} to column {target_column}: {str(e)}") from e
