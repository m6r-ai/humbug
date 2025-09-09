import json
import logging
import os
from typing import Dict, Any, List, Optional

from ai import AIConversationSettings
from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback, AIToolOperationDefinition,
    AIToolResult, AIToolCall
)
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceNotFoundError, MindspaceError
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.terminal.terminal_tab import TerminalTab
from humbug.user.user_manager import UserManager


class SystemAITool(AITool):
    """
    System operations tool for LLM interaction.

    Provides structured access to system operations like opening files, creating terminals,
    starting conversations, accessing wiki views, and controlling terminal operations.
    All operations are restricted to the current mindspace and require user authorization
    where appropriate.
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
        operations = self.get_operation_definitions()
        operation_names: List[str] = list(operations.keys())

        # Build description from operations
        base_description = (
            "The system tool let's you (the AI) control the application user interface for the user. "
            "The user interface is organized into columns, each containing tabs."
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
                    description="ID of a tab (for tab_info, close_tab, move_tab, and terminal operations)",
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
                ),
                AIToolParameter(
                    name="command",
                    type="string",
                    description="Command to send to terminal (for send_terminal_command operation)",
                    required=False
                ),
                AIToolParameter(
                    name="lines",
                    type="integer",
                    description="Number of lines to read from terminal buffer (for read_terminal operation)",
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
                description="Open a file in an editor tab for the user to browse/edit. "
                    "You (the AI) cannot use this to edit files"
            ),
            "new_terminal_tab": AIToolOperationDefinition(
                name="new_terminal_tab",
                handler=self._new_terminal_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Create a terminal tab. "
                    "You (the AI) can use this to run commands"
            ),
            "open_conversation_tab": AIToolOperationDefinition(
                name="open_conversation_tab",
                handler=self._open_conversation_tab,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open an existing conversation in a conversation tab for the user to browse/edit. "
                    "You (the AI) cannot use this to send messages"
            ),
            "new_conversation_tab": AIToolOperationDefinition(
                name="new_conversation_tab",
                handler=self._new_conversation_tab,
                allowed_parameters={"model", "temperature"},
                required_parameters=set(),
                description="Create a new AI conversation tab, with optional model/temperature. "
            ),
            "show_system_shell_tab": AIToolOperationDefinition(
                name="show_system_shell_tab",
                handler=self._show_system_shell_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Open a system shell tab"
            ),
            "show_log_tab": AIToolOperationDefinition(
                name="show_log_tab",
                handler=self._show_log_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Open the mindspace log tab"
            ),
            "open_wiki_tab": AIToolOperationDefinition(
                name="open_wiki_tab",
                handler=self._open_wiki_tab,
                allowed_parameters={"file_path"},
                required_parameters=set(),
                description="open a file/directory in a wiki view tab"
            ),
            "tab_info": AIToolOperationDefinition(
                name="tab_info",
                handler=self._tab_info,
                allowed_parameters={"tab_id"},
                required_parameters=set(),
                description="Get information about a tab, given its ID (if no ID then gets the current tab info)"
            ),
            "close_tab": AIToolOperationDefinition(
                name="close_tab",
                handler=self._close_tab,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Close an existing tab"
            ),
            "list_tabs": AIToolOperationDefinition(
                name="list_tabs",
                handler=self._list_tabs,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Enumerate all currently open tabs across all columns"
            ),
            "move_tab": AIToolOperationDefinition(
                name="move_tab",
                handler=self._move_tab,
                allowed_parameters={"tab_id", "target_column"},
                required_parameters={"tab_id", "target_column"},
                description="Move a tab to a specific column by index - there are a maximum of 6 columns"
            ),
            "send_terminal_command": AIToolOperationDefinition(
                name="send_terminal_command",
                handler=self._send_terminal_command,
                allowed_parameters={"tab_id", "command"},
                required_parameters={"command"},
                description="Send a command to a terminal tab (given its tab ID). Requires user authorization."
            ),
            "read_terminal": AIToolOperationDefinition(
                name="read_terminal",
                handler=self._read_terminal,
                allowed_parameters={"tab_id", "lines"},
                required_parameters=set(),
                description="Read current terminal buffer content (given a terminal tab ID)"
            ),
            "get_terminal_status": AIToolOperationDefinition(
                name="get_terminal_status",
                handler=self._get_terminal_status,
                allowed_parameters={"tab_id"},
                required_parameters=set(),
                description="Get terminal status and process information (given a terminal tab ID)"
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
                "No mindspace is currently open. System operations require an active mindspace."
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
            raise AIToolExecutionError(f"No '{key}' argument provided")

        value = arguments[key]
        if not isinstance(value, str):
            raise AIToolExecutionError(f"'{key}' must be a string")

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
            raise AIToolExecutionError(f"No '{key}' argument provided")

        value = arguments[key]
        if not isinstance(value, int):
            raise AIToolExecutionError(f"'{key}' must be an integer")

        return value

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
            # Check if our path starts with a separator.  If it does we'll assume it's for the root of the mindspace.
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

    def _get_terminal_tab(self, arguments: Dict[str, Any]) -> TerminalTab:
        """
        Get a terminal tab by ID or current tab.

        Args:
            arguments: Tool arguments containing optional tab_id

        Returns:
            TerminalTab instance

        Raises:
            AIToolExecutionError: If no terminal tab found
        """
        tab_id = arguments.get("tab_id")
        if tab_id:
            # Get specific terminal by ID
            if not isinstance(tab_id, str):
                raise AIToolExecutionError("'tab_id' must be a string")

            tab = self._column_manager.get_tab_by_id(tab_id)
            if not tab:
                raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

            if not isinstance(tab, TerminalTab):
                raise AIToolExecutionError(f"Tab {tab_id} is not a terminal tab")

            return tab

        else:
            # Use current tab
            current_tab = self._column_manager.get_current_tab()
            if not current_tab:
                raise AIToolExecutionError("No current tab is open")

            if not isinstance(current_tab, TerminalTab):
                raise AIToolExecutionError("Current tab is not a terminal tab")

            return current_tab

    def _get_terminal_buffer_content(self, terminal_tab: TerminalTab, max_lines: Optional[int] = None) -> str:
        """
        Get the terminal buffer content as text.

        Args:
            terminal_tab: Terminal tab to read from
            max_lines: Maximum number of lines to return (None for all)

        Returns:
            Terminal buffer content as string
        """
        # Access the terminal widget and its buffer
        if not hasattr(terminal_tab, '_terminal_widget'):
            raise AIToolExecutionError("Terminal widget is not available")

        terminal_widget = terminal_tab._terminal_widget

        # Get the terminal state and buffer
        if not hasattr(terminal_widget, '_state'):
            raise AIToolExecutionError("Terminal state is not available")

        state = terminal_widget._state
        buffer = state.current_buffer()

        # Get buffer dimensions
        history_lines = buffer.history_lines()
        cols = buffer.cols

        # Determine which lines to read
        if max_lines is None:
            start_line = 0
            end_line = history_lines

        else:
            start_line = max(0, history_lines - max_lines)
            end_line = history_lines

        # Extract text content
        lines = []
        for line_idx in range(start_line, end_line):
            if line_idx < len(buffer.lines):
                line = buffer.lines[line_idx]
                line_text = ""

                # Extract characters from the line
                for col in range(cols):
                    char, _, _, _ = line.get_character(col)
                    line_text += char

                # Remove trailing spaces
                line_text = line_text.rstrip()
                lines.append(line_text)

        return '\n'.join(lines)

    def _get_terminal_status_info(self, terminal_tab: TerminalTab) -> Dict[str, Any]:
        """
        Get terminal status information.

        Args:
            terminal_tab: Terminal tab to get status for

        Returns:
            Dictionary containing terminal status information
        """
        status: Dict[str, Any] = {}

        try:
            # Get basic tab info
            status['tab_id'] = terminal_tab.tab_id()
            status['running'] = getattr(terminal_tab, '_running', False)

            # Get process information
            if hasattr(terminal_tab, '_terminal_process') and terminal_tab._terminal_process:
                process = terminal_tab._terminal_process
                status['process_id'] = getattr(process, 'get_process_id', lambda: None)()
                status['process_running'] = getattr(process, 'is_running', lambda: False)()
                status['working_directory'] = getattr(process, 'get_working_directory', lambda: None)()
                status['process_name'] = getattr(process, 'get_process_name', lambda: 'Unknown')()

            else:
                status['process_id'] = None
                status['process_running'] = False
                status['working_directory'] = None
                status['process_name'] = 'No process'

            # Get terminal dimensions
            if hasattr(terminal_tab, '_terminal_widget'):
                terminal_widget = terminal_tab._terminal_widget
                if hasattr(terminal_widget, 'get_terminal_size'):
                    rows, cols = terminal_widget.get_terminal_size()
                    status['terminal_size'] = {'rows': rows, 'cols': cols}

                else:
                    status['terminal_size'] = {'rows': 'unknown', 'cols': 'unknown'}

                # Get cursor position if available
                if hasattr(terminal_widget, '_state'):
                    state = terminal_widget._state
                    buffer = state.current_buffer()
                    status['cursor_position'] = {
                        'row': buffer.cursor.row,
                        'col': buffer.cursor.col,
                        'visible': buffer.cursor.visible
                    }
                    status['buffer_lines'] = buffer.history_lines()

            else:
                status['terminal_size'] = {'rows': 'unknown', 'cols': 'unknown'}
                status['cursor_position'] = None
                status['buffer_lines'] = 0

        except Exception as e:
            self._logger.warning(f"Error getting terminal status: {e}")
            status['error'] = str(e)

        return status

    def _format_terminal_status(self, status_info: Dict[str, Any]) -> str:
        """
        Format terminal status information as readable text.

        Args:
            status_info: Status information dictionary

        Returns:
            Formatted status text
        """
        lines = []

        lines.append(f"Tab ID: {status_info.get('tab_id', 'Unknown')}")
        lines.append(f"Running: {status_info.get('running', False)}")

        # Process information
        pid = status_info.get('process_id')
        if pid:
            lines.append(f"Process ID: {pid}")

        else:
            lines.append("Process ID: None")

        lines.append(f"Process Running: {status_info.get('process_running', False)}")
        lines.append(f"Process Name: {status_info.get('process_name', 'Unknown')}")

        working_dir = status_info.get('working_directory')
        if working_dir:
            lines.append(f"Working Directory: {working_dir}")

        else:
            lines.append("Working Directory: Unknown")

        # Terminal dimensions
        term_size = status_info.get('terminal_size', {})
        rows = term_size.get('rows', 'unknown')
        cols = term_size.get('cols', 'unknown')
        lines.append(f"Terminal Size: {rows} rows x {cols} cols")

        # Cursor position
        cursor_pos = status_info.get('cursor_position')
        if cursor_pos:
            lines.append(f"Cursor Position: row {cursor_pos['row']}, col {cursor_pos['col']} (visible: {cursor_pos['visible']})")

        else:
            lines.append("Cursor Position: Unknown")

        lines.append(f"Buffer Lines: {status_info.get('buffer_lines', 0)}")

        # Error information
        error = status_info.get('error')
        if error:
            lines.append(f"Error: {error}")

        return '\n'.join(lines)

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the system operation with continuation support.

        Args:
            tool_call: Tool call containing operation name and arguments
            requester_ref: Reference to the requester
            request_authorization: Function to call for user authorization

        Returns:
            AIToolResult containing the execution result and optional continuation

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
        # Validate mindspace is open
        self._validate_mindspace_access()

        # Extract operation name
        arguments = tool_call.arguments
        operation = arguments.get("operation")
        if not operation:
            raise AIToolExecutionError("No 'operation' argument provided")

        if not isinstance(operation, str):
            raise AIToolExecutionError("'operation' must be a string")

        # Get operation definition
        operation_definitions = self.get_operation_definitions()
        if operation not in operation_definitions:
            available_operations = ", ".join(sorted(operation_definitions.keys()))
            raise AIToolExecutionError(
                f"Unsupported operation: {operation}. Available operations: {available_operations}"
            )

        operation_def = operation_definitions[operation]

        self._logger.debug("System operation requested: %s", operation)

        try:
            return await operation_def.handler(tool_call, request_authorization)

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            # Re-raise our own errors
            raise

        except Exception as e:
            self._logger.error("Unexpected error in system operation '%s': %s", operation, str(e), exc_info=True)
            raise AIToolExecutionError(f"System operation failed: {str(e)}") from e

    async def _open_editor_tab(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Edit or create a file in an editor tab."""
        arguments = tool_call.arguments
        file_path_arg = self._get_str_value_from_key("file_path", arguments)
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
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Create a new conversation tab."""
        # Get file path
        arguments = tool_call.arguments
        file_path_arg = self._get_str_value_from_key("file_path", arguments)
        conversation_path = self._validate_and_resolve_path(file_path_arg)

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
                    f"Conversation file '{file_path_arg}' does not exist or is not a valid conversation."
                )

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

        except Exception as e:
            raise AIToolExecutionError(f"Failed to create conversation: {str(e)}") from e

    async def _new_conversation_tab(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Create a new conversation tab."""
        # Extract optional parameters
        arguments = tool_call.arguments
        model = arguments.get("model")
        temperature = arguments.get("temperature")

        # Validate model if provided
        if model and not isinstance(model, str):
            raise AIToolExecutionError("'model' must be a string")

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
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Create conversation
            self._column_manager.protect_current_tab(True)
            try:
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

        except MindspaceError as e:
            raise AIToolExecutionError(f"Failed to create conversation directory: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to create conversation: {str(e)}") from e

    async def _show_system_shell_tab(
        self,
        tool_call: AIToolCall,
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
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
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
                f"AI opened mindspace log\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened mindspace log, tab ID: {tab_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to show system shell: {str(e)}") from e

    async def _open_wiki_tab(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open wiki view for a specific location or mindspace root."""
        # Get file path (optional)
        arguments = tool_call.arguments
        file_path_arg = arguments.get("file_path", "")

        if file_path_arg:
            wiki_path = self._validate_and_resolve_path(file_path_arg)

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
                f"AI opened wiki tab for: '{location}'\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Opened wiki tab for: '{location}', tab ID: {tab_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open wiki: {str(e)}") from e

    async def _tab_info(
        self,
        tool_call: AIToolCall,
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
                content=f"Tab info for ID {tab_id}:\n{json.dumps(tab_info, indent=2)}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get tab info for ID {tab_id}: {str(e)}") from e

    async def _close_tab(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Close an existing tab by ID."""
        arguments = tool_call.arguments
        tab_id = self._get_str_value_from_key("tab_id", arguments)

        try:
            self._column_manager.close_tab_by_id(tab_id)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI closed tab\ntab ID: {tab_id}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Closed tab, tab ID: {tab_id}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to close tab {tab_id}: {str(e)}") from e

    async def _list_tabs(
        self,
        tool_call: AIToolCall,
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
            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Current tabs:\n{json.dumps(result, indent=2)}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to list tabs: {str(e)}") from e

    async def _move_tab(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Move a tab to a specific column."""
        arguments = tool_call.arguments
        tab_id = self._get_str_value_from_key("tab_id", arguments)
        target_column = self._get_int_value_from_key("target_column", arguments)

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

    async def _send_terminal_command(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Send a command to a terminal."""
        arguments = tool_call.arguments

        # Get and validate command
        command = arguments.get("command")
        if not command or not isinstance(command, str):
            raise AIToolExecutionError("'command' must be a non-empty string")

        # Get terminal tab
        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        # Build authorization context
        context = f"Send command to terminal (tab {tab_id}): '{command}'"

        # Request authorization - commands can be destructive
        authorized = await request_authorization("system", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to send command: {command}")

        try:
            # Send command to terminal - add newline to execute
            command_bytes = (command + '\n').encode('utf-8')

            # Access the terminal process through the tab's internal structure
            if hasattr(terminal_tab, '_terminal_process') and terminal_tab._terminal_process:
                await terminal_tab._terminal_process.write_data(command_bytes)
            else:
                raise AIToolExecutionError("Terminal process is not available")

            # Log the operation
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI sent command to terminal: '{command}'\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Command sent to terminal {tab_id}: '{command}'"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to send command to terminal: {str(e)}") from e

    async def _read_terminal(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read terminal buffer content."""
        arguments = tool_call.arguments

        # Get terminal tab
        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        # Get optional lines parameter
        lines = arguments.get("lines")
        if lines is not None and not isinstance(lines, int):
            raise AIToolExecutionError("'lines' must be an integer")

        try:
            # Get terminal buffer content
            buffer_content = self._get_terminal_buffer_content(terminal_tab, lines)

            # Log the operation
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI read terminal buffer\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Terminal buffer content (tab {tab_id}):\n\n{buffer_content}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read terminal: {str(e)}") from e

    async def _get_terminal_status(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get terminal status information."""
        arguments = tool_call.arguments

        # Get terminal tab
        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        try:
            # Get terminal status
            status_info = self._get_terminal_status_info(terminal_tab)

            # Log the operation
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested terminal status\ntab ID: {tab_id}"
            )

            # Format status as readable text
            status_text = self._format_terminal_status(status_info)

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Terminal status (tab {tab_id}):\n{status_text}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get terminal status: {str(e)}") from e
