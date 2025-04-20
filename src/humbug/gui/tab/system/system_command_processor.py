"""Processes system commands and handles tab completion."""

import logging
from typing import List

from humbug.gui.tab.system.completion_result import CompletionResult
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
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
        self._cursor_token_index: int = -1
        self._current_command_name: str | None = None

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

        # Reset tab completion state when processing a command
        self.reset_tab_completion()

        try:
            # Parse the command line
            self._parse_command_line(command_text, len(command_text))

            # Get the command name
            cmd = self._current_command_name
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

            command.execute(self._current_tokens, command_text)

        except Exception as e:
            self._logger.error("Error executing command '%s': %s", command_text, str(e), exc_info=True)
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Error executing command: {str(e)}"
            )

    def _parse_command_line(self, current_text: str, cursor_position: int) -> None:
        """
        Parse the command line text and update token information.

        Args:
            current_text: The current command text
            cursor_position: The position of the cursor in the text
        """
        # Tokenize the input
        lexer = CommandLexer()
        lexer.lex(None, current_text)

        # Store tokens
        self._current_tokens = []
        token = lexer.get_next_token()
        while token is not None:
            self._current_tokens.append(token)
            token = lexer.get_next_token()

        # Find the token at cursor position
        self._cursor_token_index = -1
        for i, token in enumerate(self._current_tokens):
            token_end = token.start + len(token.value)
            if token.start <= cursor_position <= token_end:
                self._cursor_token_index = i
                break

        # Extract command name
        self._current_command_name = self._get_command_name(self._current_tokens)

    def get_available_commands(self) -> List[str]:
        """
        Return list of available command names.

        Returns:
            List of command names
        """
        return self._command_registry.get_command_names()

    def reset_tab_completion(self) -> None:
        """Reset tab completion state."""
        self._tab_completions = []
        self._current_completion_index = -1
        self._completion_start_pos = 0
        self._current_completion_text = ""
        self._tab_completion_active = False
        self._current_tokens = []
        self._cursor_token_index = -1
        self._current_command_name = None

    def _get_token_at_cursor(self) -> Token | None:
        """
        Get the token at the cursor position.

        Returns:
            The token at the cursor position, or None if not found
        """
        if 0 <= self._cursor_token_index < len(self._current_tokens):
            return self._current_tokens[self._cursor_token_index]

        return None

    def _get_previous_token(self, token_index: int) -> Token | None:
        """
        Get the token before the specified token index.

        Args:
            token_index: The index of the token to get the previous token for

        Returns:
            The previous token, or None if not found
        """
        if 0 <= token_index - 1 < len(self._current_tokens):
            return self._current_tokens[token_index - 1]

        return None

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
        is_continuation: bool = False,
        cursor_position: int | None = None
    ) -> CompletionResult:
        """
        Handle tab completion for the current input text.

        Args:
            current_text: Current input text
            is_continuation: Whether this is a continuation of previous tab presses
            cursor_position: Position of cursor in text (defaults to end of text)

        Returns:
            CompletionResult with information about what to replace
        """
        print(f"Tab completion requested: '{current_text}', continuation: {is_continuation}, cursor_position: {cursor_position}")
        # If cursor_position is not provided, assume it's at the end
        if cursor_position is None:
            cursor_position = len(current_text)

        # If empty text, nothing to complete
        if not current_text:
            return CompletionResult(success=False)

        # Handle continuation of existing tab completion
        if is_continuation and self._tab_completion_active and self._tab_completions:
            # Move to next completion in the list
            self._current_completion_index = (self._current_completion_index + 1) % len(self._tab_completions)
            new_completion = self._tab_completions[self._current_completion_index]

            # Calculate end position based on current completion text
            end_pos = self._completion_start_pos + len(self._current_completion_text)

            # Create result with dynamic end position
            result = CompletionResult(
                success=True,
                replacement=new_completion,
                start_pos=self._completion_start_pos,
                end_pos=end_pos,
                add_space=not new_completion.endswith('/')
            )

            # Update the current completion text for next cycle
            self._current_completion_text = new_completion

            return result

        # Otherwise, this is a new tab completion request
        self._tab_completions = []
        self._current_completion_index = -1
        self._tab_completion_active = False
        self._current_completion_text = ""

        # Parse the command line once
        self._parse_command_line(current_text, cursor_position)

        # Determine what to complete based on token context
        token = self._get_token_at_cursor()

        # Store start position for future continuations
        if token:
            self._completion_start_pos = token.start

        else:
            # If no token at cursor, we're completing at whitespace
            self._completion_start_pos = cursor_position
            if not self._current_command_name:
                token = Token(TokenType.COMMAND, "", cursor_position)

            else:
                token = Token(TokenType.ARGUMENT, "", cursor_position)

        if token.type == TokenType.COMMAND:
            result = self._complete_command_name(token)

        else:
            # Get the command
            if not self._current_command_name:
                return CompletionResult(success=False)

            command = self._command_registry.get_command(self._current_command_name)
            if not command:
                return CompletionResult(success=False)

            result = self._complete_argument(command, token)

        if result.success and result.replacement is not None:
            self._tab_completion_active = True
            # Store the current completion text for next cycle
            self._current_completion_text = result.replacement

        return result

    def _complete_command_name(self, token: Token) -> CompletionResult:
        """
        Complete a command name.

        Args:
            token: The command token to complete

        Returns:
            CompletionResult with command name completion
        """
        command_names = self.get_available_commands()
        matches = [name for name in command_names if name.startswith(token.value)]
        return self._complete(matches, token)

    def _complete_argument(self, command: SystemCommand, token: Token) -> CompletionResult:
        """
        Complete an argument token.

        Args:
            command: The command to complete the argument for
            token: The argument token to complete

        Returns:
            CompletionResult with argument completion
        """
        # Get completions from the command
        completions = command.get_token_completions(
            token,
            self._current_tokens,
            self._cursor_token_index
        )

        # Filter completions based on the partial argument
        unescaped_partial = self._unescape_text(token.value)
        matches = [comp for comp in completions if comp.startswith(unescaped_partial)]
        escaped_matches = [self._escape_text(match) for match in matches]
        return self._complete(escaped_matches, token)

    def _complete(self, completions: List[str], token: Token) -> CompletionResult:
        """
        Complete an argument token.

        Args:
            completions: The list of completions
            token: The argument token to complete

        Returns:
            CompletionResult with argument completion
        """
        if not completions:
            return CompletionResult(success=False)

        # Escape spaces in completions
        self._tab_completions = completions

        start_pos = token.start
        end_pos = token.start + len(token.value)

        if len(completions) == 1:
            # Single completion - replace just the argument
            completion = completions[0]
            add_space = not completion.endswith('/')
            self._current_completion_index = 0

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=add_space
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = completions[0]

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )
