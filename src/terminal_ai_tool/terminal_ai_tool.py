import json
import logging
import re
from typing import Any

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
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel
from terminal_context.terminal_context import TerminalContext


class TerminalAITool(AITool):
    """
    AI tool for terminal operations.

    Provides operations for reading terminal output, writing input,
    and querying terminal status. Requires a terminal tab to be open
    (use system tool to create terminals).
    """

    def __init__(self, mindspace: Mindspace):
        """
        Initialize the terminal tool.

        Args:
            mindspace: The active mindspace model
        """
        self._mindspace = mindspace
        self._logger = logging.getLogger("TerminalAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="terminal",
            description_prefix=(
            "Operations for interacting with terminal tabs. Use this tool to send commands, "
            "read output, and check terminal status. Write operations require user authorization. "
            "You must have a terminal tab open first (use the system tool to create terminals). "
            "You must not assume a terminal shell or operating system - if you are unsure, check "
            "with the get_status operation."
            ),
            additional_parameters=[
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
                        "You MUST use `\\u####` format for control characters "
                        "(e.g., `\\u000a` for newline, `\\u000d` for carriage return)",
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

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Send commands and read output from terminal tabs."

    def get_operation_definitions(self) -> dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "write": AIToolOperationDefinition(
                name="write",
                handler=self._write,
                extract_context=self.write_context,
                allowed_parameters={"tab_id", "keystrokes"},
                required_parameters={"tab_id", "keystrokes"},
                description="Send keystrokes to a terminal tab. Requires user authorization before execution. "
                    "The terminal may have access beyond the project mindspace, so user will review commands before "
                    "they are sent. You may send more than one keystroke at a time by submitting them as a string. "
                    "The string is not terminated with an enter/return keycode automatically, so "
                    "if you want to execute a command you must include appropriate end-of-line control characters. "
                    "You MUST use `\\u####` format to send any control characters (ASCII values less than 0x20), "
                    "including newline (`\\u000a`), carriage return (`\\u000d`), tab (`\\u0009`), and escape (`\\u001b`)"
            ),
            "read": AIToolOperationDefinition(
                name="read",
                handler=self._read,
                extract_context=None,
                allowed_parameters={"tab_id", "lines"},
                required_parameters={"tab_id"},
                description="Read the current terminal buffer (output display) content. This returns the raw "
                    "content of the terminal display. The terminal can have over 10k lines of text and that's far "
                    "too much content so you must think carefully about the number of lines you need to request"
            ),
            "get_status": AIToolOperationDefinition(
                name="get_status",
                handler=self._get_status,
                extract_context=None,
                allowed_parameters={"tab_id"},
                required_parameters={"tab_id"},
                description="Get terminal status and process information. This includes the shell program "
                    "(e.g., /bin/sh, /bin/bash, cmd.exe, powershell.exe) and platform (e.g., linux, darwin, "
                    "win32), which determine the command syntax and behavior for this terminal"
            ),
        }

    def _get_terminal_context(self, arguments: dict[str, Any]) -> TerminalContext:
        """
        Retrieve the TerminalContext for the given context_id.

        Args:
            arguments: Tool arguments containing tab_id

        Returns:
            TerminalContext instance

        Raises:
            AIToolExecutionError: If no terminal context found for the given ID
        """
        context_id = self._get_required_str_value("tab_id", arguments)
        context = self._mindspace.contexts().get_model(context_id, TerminalContext)
        if context is None:
            raise AIToolExecutionError(
                f"No terminal context found with ID: {context_id}"
            )

        return context

    def _process_ai_escape_sequences(self, raw_input: str) -> str:
        """
        Convert AI's literal Unicode escape sequences to actual control characters.

        Args:
            raw_input: Input string potentially containing literal Unicode escape sequences

        Returns:
            Processed input string with Unicode escape sequences converted to actual characters
        """
        if not raw_input:
            return raw_input

        def unicode_replace(match: re.Match[str]) -> str:
            hex_value = match.group(1)
            try:
                char_code = int(hex_value, 16)
                return chr(char_code)

            except (ValueError, OverflowError):
                return match.group(0)

        result = re.sub(r'\\u([0-9a-fA-F]{4})', unicode_replace, raw_input)

        if result != raw_input:
            self._logger.debug("Processed AI Unicode escape sequences: %r -> %r", raw_input, result)

        return result

    def _preview_ai_escape_sequences(self, input_text: str) -> str:
        """
        Preview AI's literal Unicode escape sequences for display purposes.

        Args:
            input_text: Input string potentially containing literal Unicode escape sequences

        Returns:
            Preview string with newlines and carriage returns converted for display
        """
        if not input_text:
            return input_text

        return input_text.replace('\\u000a', '\n').replace('\\u000d', '\r')

    def write_context(self, arguments: dict[str, Any]) -> str | None:
        """
        Extract context for write operation.

        Args:
            arguments: Tool call arguments

        Returns:
            Context string for write operation
        """
        raw_keystrokes = self._get_required_str_value("keystrokes", arguments)
        preview_keystrokes = self._preview_ai_escape_sequences(raw_keystrokes)
        return f"`keystrokes` is:\n```text\n{preview_keystrokes}\n```"

    async def _write(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Write keystrokes to a terminal."""
        arguments = tool_call.arguments
        raw_keystrokes = self._get_required_str_value("keystrokes", arguments)
        processed_keystrokes = self._process_ai_escape_sequences(raw_keystrokes)

        terminal_context = self._get_terminal_context(arguments)
        context_id = terminal_context.context_id()

        context = f"Send keystrokes to terminal (tab {context_id}):"
        authorized = await request_authorization("terminal", arguments, context, None, True)
        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to send keystrokes: {raw_keystrokes}")

        try:
            await terminal_context.send_keystrokes(processed_keystrokes)

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI sent keystrokes to terminal: '{raw_keystrokes}'\ntab ID: {context_id}"
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
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Read terminal buffer content."""
        arguments = tool_call.arguments
        terminal_context = self._get_terminal_context(arguments)
        context_id = terminal_context.context_id()
        lines = self._get_optional_int_value("lines", arguments)

        try:
            buffer_content = terminal_context.get_buffer_content(lines)

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI read terminal buffer\ntab ID: {context_id}"
            )

            return AIToolResult(
                id=tool_call.id,
                name="terminal",
                content=buffer_content,
                context="text"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to read terminal: {str(e)}") from e

    async def _get_status(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        _request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Get terminal status information."""
        arguments = tool_call.arguments
        terminal_context = self._get_terminal_context(arguments)
        context_id = terminal_context.context_id()

        try:
            status = terminal_context.get_status()

            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI requested terminal status\ntab ID: {context_id}"
            )

            status_dict = {
                "tab_id": status["context_id"],
                "tab_running": status["process_running"],
                "shell": status["shell"],
                "platform": status["platform"],
                "process_id": status["process_id"],
                "process_running": status["process_running"],
                "process_name": status["process_name"],
                "terminal_size": {
                    "rows": status["terminal_size"][0],
                    "cols": status["terminal_size"][1]
                },
                "cursor_position": {
                    "row": status["cursor_position"][0],
                    "col": status["cursor_position"][1],
                    "visible": status["cursor_visible"]
                },
                "buffer_lines": status["buffer_lines"]
            }

            return AIToolResult(
                id=tool_call.id,
                name="terminal",
                content=json.dumps(status_dict, indent=2),
                context="json"
            )

        except Exception as e:
            raise AIToolExecutionError(f"Failed to get terminal status: {str(e)}") from e
