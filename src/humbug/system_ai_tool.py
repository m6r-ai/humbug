import json
import logging
import os
import re
from typing import Dict, Any, List

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
from humbug.tabs.editor.editor_tab import EditorTab
from humbug.tabs.terminal.terminal_status import TerminalStatusInfo
from humbug.tabs.terminal.terminal_tab import TerminalTab
from humbug.user.user_manager import UserManager


class SystemAITool(AITool):
    """
    System operations tool for LLM interaction.

    Provides structured access to system operations like opening files, creating terminals,
    starting conversations, accessing previews, and controlling terminal operations.
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
                    description="Path to file or directory (for open_editor, open_conversation, and open_preview operations)",
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
                    name="keystrokes",
                    type="string",
                    description="Keystrokes to send to terminal (for write_terminal operation)",
                    required=False
                ),
                AIToolParameter(
                    name="lines",
                    type="integer",
                    description="Number of lines to read from terminal buffer (for read_terminal operation)",
                    required=False
                ),
                AIToolParameter(
                    name="line",
                    type="integer",
                    description="Line number (1-indexed) for editor operations",
                    required=False
                ),
                AIToolParameter(
                    name="column",
                    type="integer",
                    description="Column number (1-indexed) for editor operations",
                    required=False
                ),
                AIToolParameter(
                    name="start_line",
                    type="integer",
                    description="Start line number (1-indexed) for editor operations",
                    required=False
                ),
                AIToolParameter(
                    name="end_line",
                    type="integer",
                    description="End line number (1-indexed) for editor operations",
                    required=False
                ),
                AIToolParameter(
                    name="start_column",
                    type="integer",
                    description="Start column number (1-indexed) for editor operations",
                    required=False
                ),
                AIToolParameter(
                    name="end_column",
                    type="integer",
                    description="End column number (1-indexed) for editor operations",
                    required=False
                ),
                AIToolParameter(
                    name="search_text",
                    type="string",
                    description="Text to search for in editor",
                    required=False
                ),
                AIToolParameter(
                    name="case_sensitive",
                    type="boolean",
                    description="Whether search should be case-sensitive (for editor_search operation)",
                    required=False
                ),
                AIToolParameter(
                    name="new_lines",
                    type="string",
                    description="New line content for editor_replace_lines operation. All lines MUST have a newline terminator " \
                        "unless no new content is being provided (e.g. to delete lines)",
                    required=False
                ),
                AIToolParameter(
                    name="move_cursor_after",
                    type="boolean",
                    description="Whether to move cursor after edit operation (default: True)",
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
            "editor_open_tab": AIToolOperationDefinition(
                name="editor_open_tab",
                handler=self._editor_open_tab,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open a file in an editor tab for the user to browse/edit. "
                    "You (the AI) cannot use this to edit files"
            ),
            "terminal_new_tab": AIToolOperationDefinition(
                name="terminal_new_tab",
                handler=self._terminal_new_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Create a fully interactive terminal tab. "
                    "This provides a terminal emulator connected to a new shell. "
                    "You may interact with this terminal using the `terminal_read` and `terminal_write` operations, but"
                    "you must use `terminal_read` to observe any changes"
            ),
            "conversation_open_tab": AIToolOperationDefinition(
                name="conversation_open_tab",
                handler=self._conversation_open_tab,
                allowed_parameters={"file_path"},
                required_parameters={"file_path"},
                description="Open an existing conversation in a conversation tab for the user to browse/edit. "
                    "You (the AI) cannot use this to send messages"
            ),
            "conversation_new_tab": AIToolOperationDefinition(
                name="conversation_new_tab",
                handler=self._conversation_new_tab,
                allowed_parameters={"model", "temperature"},
                required_parameters=set(),
                description="Create a new AI conversation tab, with optional model/temperature"
            ),
            "system_shell_show_tab": AIToolOperationDefinition(
                name="system_shell_show_tab",
                handler=self._system_shell_show_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Open a system shell tab"
            ),
            "log_show_tab": AIToolOperationDefinition(
                name="log_show_tab",
                handler=self._log_show_tab,
                allowed_parameters=set(),
                required_parameters=set(),
                description="Open the mindspace log tab"
            ),
            "preview_open_tab": AIToolOperationDefinition(
                name="preview_open_tab",
                handler=self._preview_open_tab,
                allowed_parameters={"file_path"},
                required_parameters=set(),
                description="open a file/directory in a preview view tab"
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
            "terminal_write": AIToolOperationDefinition(
                name="terminal_write",
                handler=self._terminal_write,
                allowed_parameters={"tab_id", "keystrokes"},
                required_parameters={"tab_id", "keystrokes"},
                description="Send keystrokes to a terminal tab, given its ID. "
                    "You may send more than one keystroke at a time by submitting them as a string. "
                    "The string is not terminated with a newline automatically, so if you want to execute a command "
                    "you must include appropriate end-of-line control characters. "
                    "You MUST use `\\u####` format to send any control characters (ASCII values less than 0x20), "
                    "inluding newline (\\u000a), carriage return (\\u000d), tab (\\u0009), and escape (\\u001b)"
            ),
            "terminal_read": AIToolOperationDefinition(
                name="terminal_read",
                handler=self._terminal_read,
                allowed_parameters={"tab_id", "lines"},
                required_parameters={"tab_id"},
                description="Read the current terminal buffer (ouput display) content, given its tab ID. "
                    "This returns the raw content of the terminal display. "
                    "The terminal can have over 10k lines of text and that's far too much content so you must "
                    "think carefully about the number of `lines` you need to request"
            ),
            "terminal_get_status": AIToolOperationDefinition(
                name="terminal_get_status",
                handler=self._terminal_get_status,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get terminal status and process information, given its tab ID"
            ),
            "editor_read_content": AIToolOperationDefinition(
                name="editor_read_content",
                handler=self._editor_read_content,
                allowed_parameters={"tab_id", "start_line", "end_line"},
                required_parameters={"tab_id"},
                description="Read content from an editor tab. Optionally specify line range with start_line "
                    "and end_line (1-indexed, inclusive)"
            ),
            "editor_get_cursor_info": AIToolOperationDefinition(
                name="editor_get_cursor_info",
                handler=self._editor_get_cursor_info,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get cursor position and selection information from an editor tab"
            ),
            "editor_get_info": AIToolOperationDefinition(
                name="editor_get_info",
                handler=self._editor_get_info,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get editor metadata including line count, language, encoding, and modification status"
            ),
            "editor_goto_line": AIToolOperationDefinition(
                name="editor_goto_line",
                handler=self._editor_goto_line,
                allowed_parameters={"tab_id", "line", "column"},
                required_parameters={"tab_id", "line"},
                description="Move cursor to specific line and optional column in an editor tab (1-indexed)"
            ),
            "editor_select_range": AIToolOperationDefinition(
                name="editor_select_range",
                handler=self._editor_select_range,
                allowed_parameters={"tab_id", "start_line", "start_column", "end_line", "end_column"},
                required_parameters={"tab_id", "start_line", "start_column", "end_line", "end_column"},
                description="Select a specific range of text in an editor tab (1-indexed). Highlights the region for the user"
            ),
            "editor_search": AIToolOperationDefinition(
                name="editor_search",
                handler=self._editor_search,
                allowed_parameters={"tab_id", "search_text", "case_sensitive"},
                required_parameters={"tab_id", "search_text"},
                description="Find all occurrences of text in an editor tab. Returns list of matches with line, column, and context"
            ),
            "editor_replace_lines": AIToolOperationDefinition(
                name="editor_replace_lines",
                handler=self._editor_replace_lines,
                allowed_parameters={"tab_id", "start_line", "end_line", "new_lines", "move_cursor_after"},
                required_parameters={"tab_id", "start_line", "end_line", "new_lines"},
                description="Replace entire lines with new content. Changes will not save until " \
                    "you use the `editor_save_file` operation"
            ),
            "editor_get_selected_text": AIToolOperationDefinition(
                name="editor_get_selected_text",
                handler=self._editor_get_selected_text,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get the currently selected text from an editor tab"
            ),
            "editor_save_file": AIToolOperationDefinition(
                name="editor_save_file",
                handler=self._editor_save_file,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Save the current editor content to file. Requires user authorization"
            )        }

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
        tab_id = self._get_str_value_from_key("tab_id", arguments)

        # Get specific terminal by ID
        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, TerminalTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not a terminal tab")

        return tab


    def _get_editor_tab(self, arguments: Dict[str, Any]) -> 'EditorTab':
        """
        Get an editor tab by ID.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            EditorTab instance

        Raises:
            AIToolExecutionError: If no editor tab found
        """
        tab_id = self._get_str_value_from_key("tab_id", arguments)

        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, EditorTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not an editor tab")

        return tab

    def _format_terminal_status(self, status_info: TerminalStatusInfo) -> str:
        """
        Format terminal status information as readable text.

        Args:
            status_info: TerminalStatusInfo instance

        Returns:
            Formatted status text
        """
        lines = []

        lines.append(f"Tab ID: {status_info.tab_id}")
        lines.append(f"Running: {status_info.tab_running}")

        # Process information
        if status_info.process_id:
            lines.append(f"Process ID: {status_info.process_id}")
        else:
            lines.append("Process ID: None")

        lines.append(f"Process Running: {status_info.process_running}")
        lines.append(f"Process Name: {status_info.process_name}")

        # Terminal dimensions
        rows, cols = status_info.terminal_size
        lines.append(f"Terminal Size: {rows} rows x {cols} cols")

        # Cursor position
        cursor_row, cursor_col = status_info.cursor_position
        lines.append(f"Cursor Position: row {cursor_row}, col {cursor_col} (visible: {status_info.cursor_visible})")

        lines.append(f"Buffer Lines: {status_info.buffer_lines}")

        return '\n'.join(lines)

    def _process_ai_escape_sequences(self, raw_input: str) -> str:
        """
        Convert AI's literal Unicode escape sequences to actual control characters.

        This handles the JSON double-escaping issue where the AI sends input with
        Unicode escapes that got converted to literal text. For example:
        - AI intends: ESC character for ANSI colors
        - AI should send: \\u001b in JSON (becomes actual ESC after json.loads)
        - But if double-escaped: \\\\u001b in JSON (becomes literal \\u001b text)
        - This function: converts literal \\u001b back to actual ESC character

        We only process Unicode escapes (\\u####) because:
        1. They're unambiguous in intent (clearly meant to be characters)
        2. They're valid JSON escape sequences (unlike \\x##)
        3. They avoid the ambiguity issues with \\n, \\t, etc.

        Args:
            raw_input: Input string potentially containing literal Unicode escape sequences

        Returns:
            Processed input string with Unicode escape sequences converted to actual characters
        """
        if not raw_input:
            return raw_input

        # Convert \u#### Unicode sequences to actual characters
        def unicode_replace(match: re.Match[str]) -> str:
            hex_value = match.group(1)
            try:
                char_code = int(hex_value, 16)
                return chr(char_code)

            except (ValueError, OverflowError):
                # If invalid Unicode code point, return original
                return match.group(0)

        result = re.sub(r'\\u([0-9a-fA-F]{4})', unicode_replace, raw_input)

        # Log conversion for debugging if any changes were made
        if result != raw_input:
            self._logger.debug("Processed AI Unicode escape sequences: %r -> %r", raw_input, result)

        return result

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

    async def _editor_open_tab(
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

    async def _terminal_new_tab(
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

    async def _conversation_open_tab(
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

    async def _conversation_new_tab(
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

    async def _system_shell_show_tab(
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

    async def _log_show_tab(
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

    async def _preview_open_tab(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Open preview view for a specific location or mindspace root."""
        # Get file path (optional)
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

        except Exception as e:
            raise AIToolExecutionError(f"Failed to open preview: {str(e)}") from e

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

    async def _terminal_write(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Write to a terminal."""
        arguments = tool_call.arguments

        # Get and validate keystrokes
        raw_keystrokes = arguments.get("keystrokes")
        if not raw_keystrokes or not isinstance(raw_keystrokes, str):
            raise AIToolExecutionError("'keystrokes' must be a non-empty string")

        # Process escape sequences from AI - convert literal Unicode escapes to actual characters
        processed_keystrokes = self._process_ai_escape_sequences(raw_keystrokes)

        # Get terminal tab
        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        # Build authorization context - show original keystrokes for transparency
        context = f"Send keystrokes to terminal (tab {tab_id}): '{raw_keystrokes}'"
        if processed_keystrokes != raw_keystrokes:
            context += f"\n(will be processed as: '{processed_keystrokes!r}')"

        # Request authorization - commands can be destructive
        authorized = await request_authorization("system", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to send keystrokes: {raw_keystrokes}")

        try:
            await terminal_tab.send_keystrokes(processed_keystrokes)

            # Log the operation
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI sent keystrokes to terminal: '{raw_keystrokes}'\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Keystrokes sent to terminal {tab_id}: '{raw_keystrokes}'"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to send keystrokes to terminal: {str(e)}") from e

    async def _terminal_read(
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
            # Get terminal buffer content using public method
            buffer_content = terminal_tab.get_terminal_buffer_content(lines)

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

    async def _terminal_get_status(
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
            # Get terminal status using public method
            status_info = terminal_tab.get_terminal_status_info()

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
    async def _editor_read_content(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read content from an editor tab."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        start_line = arguments.get("start_line")
        end_line = arguments.get("end_line")

        if start_line is not None and not isinstance(start_line, int):
            raise AIToolExecutionError("'start_line' must be an integer")

        if end_line is not None and not isinstance(end_line, int):
            raise AIToolExecutionError("'end_line' must be an integer")

        try:
            content = editor_tab.get_text_range(start_line, end_line)

            if start_line is not None or end_line is not None:
                range_desc = f"lines {start_line or 1}-{end_line or 'end'}"
                log_msg = f"AI read editor content ({range_desc})\ntab ID: {tab_id}"
                result_msg = f"Editor content (tab {tab_id}, {range_desc}):\n\n{content}"

            else:
                log_msg = f"AI read editor content\ntab ID: {tab_id}"
                result_msg = f"Editor content (tab {tab_id}):\n\n{content}"

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                log_msg
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=result_msg
            )

        except ValueError as e:
            raise AIToolExecutionError(f"Invalid line range: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read editor content: {str(e)}") from e

    async def _editor_get_cursor_info(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get cursor position and selection information from an editor tab."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        try:
            cursor_info = editor_tab.get_cursor_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested cursor info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Cursor info (tab {tab_id}):\n{json.dumps(cursor_info, indent=2)}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get cursor info: {str(e)}") from e

    async def _editor_get_info(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get editor metadata and document information."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        try:
            editor_info = editor_tab.get_editor_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested editor info\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Editor info (tab {tab_id}):\n{json.dumps(editor_info, indent=2)}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get editor info: {str(e)}") from e

    async def _editor_goto_line(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Move cursor to specific line and column in an editor tab."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        line = self._get_int_value_from_key("line", arguments)
        column = arguments.get("column", 1)

        if not isinstance(column, int):
            raise AIToolExecutionError("'column' must be an integer")

        try:
            editor_tab.goto_line(line, column)

            col_desc = f", column {column}" if column != 1 else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI moved cursor to line {line}{col_desc}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Moved cursor to line {line}{col_desc} in tab {tab_id}"
            )

        except ValueError as e:
            raise AIToolExecutionError(f"Invalid position: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to goto line: {str(e)}") from e

    async def _editor_select_range(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Select a specific range of text in an editor tab."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        start_line = self._get_int_value_from_key("start_line", arguments)
        start_column = self._get_int_value_from_key("start_column", arguments)
        end_line = self._get_int_value_from_key("end_line", arguments)
        end_column = self._get_int_value_from_key("end_column", arguments)

        try:
            editor_tab.set_selection(start_line, start_column, end_line, end_column)

            range_desc = f"lines {start_line}:{start_column} to {end_line}:{end_column}"
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI selected text range: {range_desc}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Selected text range {range_desc} in tab {tab_id}"
            )

        except ValueError as e:
            raise AIToolExecutionError(f"Invalid selection range: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to select range: {str(e)}") from e

    async def _editor_search(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Find all occurrences of text in an editor tab."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        search_text = self._get_str_value_from_key("search_text", arguments)
        case_sensitive = arguments.get("case_sensitive", False)

        if not isinstance(case_sensitive, bool):
            raise AIToolExecutionError("'case_sensitive' must be a boolean")

        try:
            matches = editor_tab.find_all_occurrences(search_text, case_sensitive)

            case_desc = " (case-sensitive)" if case_sensitive else ""
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI searched for '{search_text}'{case_desc}: {len(matches)} matches\ntab ID: {tab_id}"
            )

            if not matches:
                return AIToolResult(
                    id=tool_call.id,
                    name="system",
                    content=f"No matches found for '{search_text}'{case_desc} in tab {tab_id}"
                )

            result = {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "match_count": len(matches),
                "matches": matches
            }

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Search results for '{search_text}'{case_desc} in tab {tab_id}:\n{json.dumps(result, indent=2)}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to search in editor: {str(e)}") from e

    async def _editor_replace_lines(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Replace entire lines with new content."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        start_line = self._get_int_value_from_key("start_line", arguments)
        end_line = self._get_int_value_from_key("end_line", arguments)
        new_lines = self._get_str_value_from_key("new_lines", arguments)
        move_cursor_after = arguments.get("move_cursor_after", True)

        if not isinstance(move_cursor_after, bool):
            raise AIToolExecutionError("'move_cursor_after' must be a boolean")

        try:
            editor_tab.replace_lines(start_line, end_line, new_lines, move_cursor_after)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI replaced lines {start_line}-{end_line}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Replaced lines {start_line}-{end_line} in tab {tab_id}"
            )

        except ValueError as e:
            raise AIToolExecutionError(f"Invalid line range: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to replace lines: {str(e)}") from e

    async def _editor_get_selected_text(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get the currently selected text from an editor tab."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        try:
            selected_text = editor_tab.get_selected_text()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested selected text\ntab ID: {tab_id}"
            )

            if not selected_text:
                return AIToolResult(
                    id=tool_call.id,
                    name="system",
                    content=f"No text selected in tab {tab_id}"
                )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Selected text (tab {tab_id}):\n\n{selected_text}"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get selected text: {str(e)}") from e

    async def _editor_save_file(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Save the current editor content to file."""
        arguments = tool_call.arguments

        editor_tab = self._get_editor_tab(arguments)
        tab_id = editor_tab.tab_id()

        # Get editor info to show file path
        editor_info = editor_tab.get_editor_info()
        file_path = editor_info.get('file_path', '')

        if not file_path:
            raise AIToolExecutionError("Cannot save: editor has no file path (untitled file)")

        # Build authorization context
        context = f"Save editor content to file: {file_path} (tab {tab_id})"

        # Request authorization - writing to filesystem
        authorized = await request_authorization("system", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied("User denied permission to save file")

        try:
            success = editor_tab.save()

            if not success:
                raise AIToolExecutionError("Save operation failed")

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI saved editor file: {file_path}\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Saved file: {file_path} (tab {tab_id})"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to save file: {str(e)}") from e
