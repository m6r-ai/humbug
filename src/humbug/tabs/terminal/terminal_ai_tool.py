import logging
import re
from typing import Any, Dict

from ai_tool import (
    AITool,
    AIToolAuthorizationCallback,
    AIToolAuthorizationDenied,
    AIToolCall,
    AIToolDefinition,
    AIToolExecutionError,
    AIToolOperationDefinition,
    AIToolParameter,
    AIToolResult,
)
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.terminal.terminal_status import TerminalStatusInfo
from humbug.tabs.terminal.terminal_tab import TerminalTab


class TerminalAITool(AITool):
    """
    AI tool for terminal operations.

    Provides operations for reading terminal output, writing input,
    and querying terminal status. Requires a terminal tab to be open
    (use system tool to create terminals).
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the terminal tool.

        Args:
            column_manager: Column manager for accessing terminal tabs
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("TerminalAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        operations = self.get_operation_definitions()
        operation_names = list(operations.keys())

        # Build description from operations
        operation_list = []
        for name, op_def in operations.items():
            operation_list.append(f"- {name}: {op_def.description}")

        description = (
            "Operations for interacting with terminal tabs. Use this tool to send commands, "
            "read output, and check terminal status. Write operations require user authorization. "
            "You must have a terminal tab open first "
            "(use the system tool to create terminals).\n\n"
            "Available operations:\n\n" + "\n".join(operation_list)
        )

        return AIToolDefinition(
            name="terminal",
            description=description,
            parameters=[
                AIToolParameter(
                    name="operation",
                    type="string",
                    description="Terminal operation to perform",
                    required=True,
                    enum=operation_names
                ),
                AIToolParameter(
                    name="tab_id",
                    type="string",
                    description="GUID of the terminal tab to operate on",
                    required=True
                ),
                AIToolParameter(
                    name="keystrokes",
                    type="string",
                    description="Keystrokes to send to terminal (for write operation). "
                    "You MUST use \\u#### format for control characters (e.g., \\u000a for newline)",
                    required=False
                ),
                AIToolParameter(
                    name="lines",
                    type="integer",
                    description="Number of lines to read from terminal buffer (for read operation)",
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
            "write": AIToolOperationDefinition(
                name="write",
                handler=self._write,
                allowed_parameters={"tab_id", "keystrokes"},
                required_parameters={"tab_id", "keystrokes"},
                description="Send keystrokes to a terminal tab. Requires user authorization before execution. "
                    "The terminal may have access beyond the project sandbox, so user will review commands before "
                    "they are sent. You may send more than one keystroke at a time by submitting them as a string. "
                    "The string is not terminated with a newline automatically, so "
                    "if you want to execute a command you must include appropriate end-of-line control characters. "
                    "You MUST use \\u#### format to send any control characters (ASCII values less than 0x20), "
                    "including newline (\\u000a), carriage return (\\u000d), tab (\\u0009), and escape (\\u001b)"
            ),
            "read": AIToolOperationDefinition(
                name="read",
                handler=self._read,
                allowed_parameters={"tab_id", "lines"},
                required_parameters={"tab_id"},
                description="Read the current terminal buffer (output display) content. This returns the raw "
                    "content of the terminal display. The terminal can have over 10k lines of text and that's far "
                    "too much content so you must think carefully about the number of lines you need to request"
            ),
            "get_status": AIToolOperationDefinition(
                name="get_status",
                handler=self._get_status,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get terminal status and process information"
            ),
        }

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute a terminal operation.

        Args:
            tool_call: Tool call containing operation and arguments
            requester_ref: Reference to the requester
            request_authorization: Function to request user authorization

        Returns:
            Result of the operation

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
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

        self._logger.debug("Terminal operation requested: %s", operation)

        try:
            return await operation_def.handler(tool_call, request_authorization)

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            raise

        except Exception as e:
            self._logger.error("Unexpected error in terminal operation '%s': %s", operation, str(e), exc_info=True)
            raise AIToolExecutionError(f"Terminal operation failed: {str(e)}") from e

    def _get_terminal_tab(self, arguments: Dict[str, Any]) -> TerminalTab:
        """
        Get a terminal tab by ID.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            TerminalTab instance

        Raises:
            AIToolExecutionError: If no terminal tab found
        """
        if "tab_id" not in arguments:
            raise AIToolExecutionError("No 'tab_id' argument provided")

        tab_id = arguments["tab_id"]
        if not isinstance(tab_id, str):
            raise AIToolExecutionError("'tab_id' must be a string")

        tab = self._column_manager.get_tab_by_id(tab_id)
        if not tab:
            raise AIToolExecutionError(f"No tab found with ID: {tab_id}")

        if not isinstance(tab, TerminalTab):
            raise AIToolExecutionError(f"Tab {tab_id} is not a terminal tab")

        return tab

    def _process_ai_escape_sequences(self, raw_input: str) -> str:
        """
        Convert AI's literal Unicode escape sequences to actual control characters.

        This handles the JSON double-escaping issue where the AI sends input with
        Unicode escapes that got converted to literal text.

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

    async def _write(
        self,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Write keystrokes to a terminal."""
        arguments = tool_call.arguments

        # Get and validate keystrokes
        raw_keystrokes = arguments.get("keystrokes")
        if not raw_keystrokes or not isinstance(raw_keystrokes, str):
            raise AIToolExecutionError("'keystrokes' must be a non-empty string")

        # Process escape sequences from AI
        processed_keystrokes = self._process_ai_escape_sequences(raw_keystrokes)

        # Get terminal tab
        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        # Build authorization context
        context = f"Send keystrokes to terminal (tab {tab_id}): '{raw_keystrokes}'"
        if processed_keystrokes != raw_keystrokes:
            context += f"\n(will be processed as: '{processed_keystrokes!r}')"

        # Request authorization - commands can be destructive
        authorized = await request_authorization("terminal", arguments, context, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to send keystrokes: {raw_keystrokes}")

        try:
            await terminal_tab.send_keystrokes(processed_keystrokes)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI sent keystrokes to terminal: '{raw_keystrokes}'\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="terminal",
                content="Keystrokes sent"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to send keystrokes to terminal: {str(e)}") from e

    async def _read(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read terminal buffer content."""
        arguments = tool_call.arguments

        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        lines = arguments.get("lines")
        if lines is not None and not isinstance(lines, int):
            raise AIToolExecutionError("'lines' must be an integer")

        try:
            buffer_content = terminal_tab.get_terminal_buffer_content(lines)

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI read terminal buffer\ntab ID: {tab_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="terminal",
                content=buffer_content
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read terminal: {str(e)}") from e

    async def _get_status(
        self,
        tool_call: AIToolCall,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get terminal status information."""
        arguments = tool_call.arguments

        terminal_tab = self._get_terminal_tab(arguments)
        tab_id = terminal_tab.tab_id()

        try:
            status_info = terminal_tab.get_terminal_status_info()

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested terminal status\ntab ID: {tab_id}"
            )

            status_text = self._format_terminal_status(status_info)

            return AIToolResult(
                id=tool_call.id,
                name="terminal",
                content=status_text
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get terminal status: {str(e)}") from e
