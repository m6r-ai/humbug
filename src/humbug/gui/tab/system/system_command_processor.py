"""Processes system commands and handles tab completion."""

import logging
from typing import List

from humbug.gui.tab.system.system_command_completion_result import SystemCommandCompletionResult
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command_registry import SystemCommandRegistry
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import CommandLexer, TokenType, Token


class SystemCommandProcessor:
    """Processes system commands using the command registry."""

    def __init__(self) -> None:
        """Initialize command processor."""
        self._logger = logging.getLogger("CommandProcessor")
        self._mindspace_manager = MindspaceManager()
        self._command_registry = SystemCommandRegistry()

        # Tab completion state tracking
        self._tab_completions: List[str] = []
        self._current_completion_index: int = -1
        self._completion_start_pos: int = 0
        self._current_completion_text: str = ""
        self._tab_completion_active: bool = False

        # Token tracking for current command
        self._current_tokens: List[Token] = []

    def _escape_text(self, text: str) -> str:
        """
        Escape text, converging spaces to an escaped form.

        Args:
            text: The text to escape

        Returns:
            Escaped completion string
        """
        # Escape spaces with backslashes
        escaped = ""
        for char in text:
            if char == ' ':
                escaped += '\\ '
                continue

            escaped += char

        return escaped

    def _unescape_text(self, text: str) -> str:
        """
        Unescape text, converting escaped spaces to their original form.

        Args:
            text: The text to unescape

        Returns:
            Unescaped command string
        """
        unescaped = ""
        i = 0
        while i < len(text):
            if text[i] == '\\' and i + 1 < len(text):
                unescaped += text[i + 1]
                i += 2
                continue

            unescaped += text[i]
            i += 1

        return unescaped

    def process_command(self, command_text: str) -> None:
        """
        Process a command string and execute appropriate action.

        Args:
            command_text: The command text to process
        """
        command_text = command_text.strip()
        if not command_text:
            return

        self._completion_start_pos = 0

        self._parse_command_line(command_text)
        cmd = self._get_command_name(self._current_tokens)
        if not cmd:
            # No command name found
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                "Invalid command format. Type 'help' for a list of available commands."
            )
            return

        command = self._command_registry.get_command(cmd)
        if not command:
            # Command not found
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Unknown command: {cmd}. Type 'help' for a list of available commands."
            )
            return

        try:
            command.execute(self._current_tokens, command_text)

        except Exception as e:
            self._logger.error("Error executing command '%s': %s", command_text, str(e), exc_info=True)
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Error executing command: {str(e)}"
            )

    def _parse_command_line(self, current_text: str) -> None:
        """
        Parse the command line text and update token information.

        Args:
            current_text: The current command text
            cursor_position: The position of the cursor in the text

        Returns:
            The index of the token at the cursor position, or -1 if not found
        """
        self._tab_completions = []
        self._current_completion_index = -1
        self._tab_completion_active = False
        self._current_completion_text = ""

        # Tokenize the input
        lexer = CommandLexer()
        lexer.lex(None, current_text)

        # Store tokens
        self._current_tokens = []
        token = lexer.get_next_token()
        while token is not None:
            self._current_tokens.append(token)
            token = lexer.get_next_token()

    def _get_command_name(self, tokens: List[Token]) -> str | None:
        """
        Get the command name from the tokens.

        Args:
            tokens: List of tokens to search

        Returns:
            The command name if found, None otherwise
        """
        for token in tokens:
            if token.type == TokenType.COMMAND:
                return token.value

        return None

    def handle_tab_completion(
        self,
        current_text: str,
        is_continuation: bool,
        move_forward: bool,
        cursor_position: int
    ) -> SystemCommandCompletionResult:
        """
        Handle tab completion for the current input text.

        Args:
            current_text: Current input text
            is_continuation: Whether this is a continuation of previous tab presses
            move_forward: Whether to move forward or backward in completions
            cursor_position: Position of cursor in text

        Returns:
            SystemCommandCompletionResult with information about what to replace
        """
        # If empty text, nothing to complete
        if not current_text:
            return SystemCommandCompletionResult(success=False)

        # Handle continuation of existing tab completion
        if is_continuation and self._tab_completion_active and self._tab_completions:
            # Move to next completion in the list
            offset = 1 if move_forward else -1
            self._current_completion_index = (self._current_completion_index + offset) % len(self._tab_completions)
            new_completion = self._tab_completions[self._current_completion_index]

            # Calculate end position based on current completion text
            end_pos = self._completion_start_pos + len(self._current_completion_text)

            # Create result with dynamic end position
            result = SystemCommandCompletionResult(
                success=True,
                replacement=new_completion,
                start_pos=self._completion_start_pos,
                end_pos=end_pos,
                add_space=not new_completion.endswith('/')
            )

            # Update the current completion text for next cycle
            self._current_completion_text = new_completion

            return result

        # This is a new tab completion request
        self._parse_command_line(current_text)
        cmd = self._get_command_name(self._current_tokens)

        # Find the token at cursor position, and the token before it
        cursor_token_index = -1
        prev_token_index = -1
        for i, token in enumerate(self._current_tokens):
            token_end = token.start + len(token.value)
            if token.start <= cursor_position <= token_end:
                cursor_token_index = i
                break

            if token_end < cursor_position:
                prev_token_index = i

        if cursor_token_index >= 0:
            token = self._current_tokens[cursor_token_index]
            self._completion_start_pos = token.start

        else:
            # If there's no token at the cursor, we're completing at whitespace.  We create an
            # empty token at the cursor position and insert it in our token list.
            self._completion_start_pos = cursor_position
            if not cmd:
                token = Token(TokenType.COMMAND, "", cursor_position)

            else:
                token = Token(TokenType.ARGUMENT, "", cursor_position)

            token.start = cursor_position
            cursor_token_index = prev_token_index + 1
            self._current_tokens.insert(cursor_token_index, token)

        if token.type == TokenType.COMMAND:
            command_names = self._command_registry.get_command_names()
            completions = [name for name in command_names if name.startswith(token.value)]

        else:
            # Get the command
            if not cmd:
                return SystemCommandCompletionResult(success=False)

            command = self._command_registry.get_command(cmd)
            if not command:
                return SystemCommandCompletionResult(success=False)

            # Get completions from the command
            arguments = command.get_token_completions(
                token,
                self._current_tokens,
                cursor_token_index
            )

            # Filter completions based on the partial argument
            unescaped_partial = self._unescape_text(token.value)
            matches = [arg for arg in arguments if arg.startswith(unescaped_partial)]
            completions = [self._escape_text(match) for match in matches]

        if not completions:
            return SystemCommandCompletionResult(success=False)

        self._tab_completions = completions
        self._tab_completion_active = True

        start_pos = token.start
        end_pos = token.start + len(token.value)

        if len(completions) == 1:
            # Single completion - replace just the argument
            completion = completions[0]
            self._current_completion_text = completion
            add_space = not completion.endswith('/')
            self._current_completion_index = 0

            return SystemCommandCompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=add_space
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = completions[0]
        self._current_completion_text = completion

        return SystemCommandCompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )
