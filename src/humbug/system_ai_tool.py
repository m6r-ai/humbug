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
from humbug.tabs.terminal.terminal_status import TerminalStatusInfo
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
                    name="input",
                    type="string",
                    description="Input to write to terminal (for write_terminal operation)",
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
                description="Create a fully interactive terminal tab. "
                    "This provides a terminal emulator connected to a new shell. "
                    "You may interact with this terminal using the `read_terminal` and `write_terminal` operations, but"
                    "you must use `read_terminal` to observe any changes."
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
            "write_terminal": AIToolOperationDefinition(
                name="write_terminal_input",
                handler=self._write_terminal,
                allowed_parameters={"tab_id", "input"},
                required_parameters={"tab_id", "input"},
                description="Write input to a terminal tab, given its ID. "
                    "This will be processed as a series of interactive key strokes. "
                    "You must use `\\u####` format to send any control characters (ASCII values less than 0x20)"
            ),
            "read_terminal": AIToolOperationDefinition(
                name="read_terminal",
                handler=self._read_terminal,
                allowed_parameters={"tab_id", "lines"},
                required_parameters={"tab_id"},
                description="Read the current terminal buffer (ouput display) content, given its tab ID. "
                    "This returns the raw content of the terminal display"
            ),
            "get_terminal_status": AIToolOperationDefinition(
                name="get_terminal_status",
                handler=self._get_terminal_status,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get terminal status and process information, given its tab ID"
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
        tab_id = self._get_str_value_from_key("tab_id", arguments)

        # Get specific terminal by ID
        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, TerminalTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not a terminal tab")

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
        def unicode_replace(match):
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

    async def _write_terminal(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Write to a terminal."""
        arguments = tool_call.arguments

        # Get and validate input
        raw_input = arguments.get("input")
        if not raw_input or not isinstance(raw_input, str):
            raise AIToolExecutionError("'input' must be a non-empty string")

        # Process escape sequences from AI - convert literal Unicode escapes to actual characters
        processed_input = self._process_ai_escape_sequences(raw_input)

        # Get terminal tab
        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        # Build authorization context - show original input for transparency
        context = f"Send input to terminal (tab {tab_id}): '{raw_input}'"
        if processed_input != raw_input:
            context += f"\n(will be processed as: '{processed_input!r}')"

        # Request authorization - commands can be destructive
        authorized = await request_authorization("system", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to send input: {raw_input}")

        try:
            await terminal_tab.send_command(processed_input)

            # Log the operation
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI sent command to terminal: '{raw_input}'\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="system",
                content=f"Input sent to terminal {tab_id}: '{input}'"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to send input to terminal: {str(e)}") from e

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
