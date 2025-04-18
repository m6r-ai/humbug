"""Processes system commands and handles tab completion."""

import logging
from typing import List

from humbug.gui.tab.system.completion_result import CompletionResult
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.mindspace.system.system_command_registry import SystemCommandRegistry
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
        self._last_completion_text: str = ""

        # Token tracking for current command
        self._current_text: str = ""
        self._current_tokens: List[Token] = []
        self._cursor_token_index: int = -1
        self._cursor_position: int = 0
        self._current_command_name: str | None = None

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

            # Get the remaining arguments (everything after the command name)
            # Find the command token to get its end position
            command_token = None
            for token in self._current_tokens:
                if token.type == TokenType.COMMAND:
                    command_token = token
                    break

            args = ""
            if command_token:
                command_end = command_token.start + len(command_token.value)
                args = command_text[command_end:].lstrip()

            command = self._command_registry.get_command(cmd)
            if not command:
                # Command not found
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"Unknown command: {cmd}. Type 'help' for a list of available commands."
                )
                return

            success = command.execute(args)
            if not success:
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"Error executing command '{cmd}'. Check arguments and try again."
                )

        except Exception as e:
            self._logger.error("Error executing command '%s': %s", command_text, str(e))
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
        self._current_text = current_text

        # Tokenize the input
        lexer = CommandLexer()
        lexer.lex(None, current_text)

        # Store tokens
        self._current_tokens = self._collect_tokens(lexer)
        self._cursor_position = cursor_position

        # Find the token at cursor position
        self._cursor_token_index = -1
        for i, token in enumerate(self._current_tokens):
            token_end = token.start + len(token.value)
            if token.start <= cursor_position <= token_end:
                self._cursor_token_index = i
                break

        # Extract command name
        self._current_command_name = self._get_command_name(self._current_tokens)

    def _collect_tokens(self, lexer: CommandLexer) -> List[Token]:
        """
        Collect all tokens from a lexer into a list.

        Args:
            lexer: The lexer to collect tokens from

        Returns:
            List of all tokens
        """
        tokens = []
        token = lexer.get_next_token()
        while token:
            tokens.append(token)
            token = lexer.get_next_token()

        return tokens

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
        self._last_completion_text = ""
        self._current_tokens = []
        self._cursor_token_index = -1
        self._cursor_position = 0
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

    def _is_cursor_at_whitespace(self) -> bool:
        """
        Check if cursor is at whitespace between tokens.

        Returns:
            True if cursor is at whitespace, False otherwise
        """
        token = self._get_token_at_cursor()
        if not token:
            return True

        # If cursor is at the end of a token, it's effectively at whitespace
        return self._cursor_position > (token.start + len(token.value))

    def handle_tab_completion(self, current_text: str, is_continuation: bool = False, cursor_position: int = None) -> CompletionResult:
        """
        Handle tab completion for the current input text.

        Args:
            current_text: Current input text
            is_continuation: Whether this is a continuation of previous tab presses
            cursor_position: Position of cursor in text (defaults to end of text)

        Returns:
            CompletionResult with information about what to replace
        """
        current_text = current_text.strip()

        # If cursor_position is not provided, assume it's at the end
        if cursor_position is None:
            cursor_position = len(current_text)

        # If empty text, nothing to complete
        if not current_text:
            return CompletionResult(success=False)

        # If we're cycling through completions and this is a continuation
        if is_continuation and self._tab_completions:
            # Move to next completion in the list
            self._current_completion_index = (self._current_completion_index + 1) % len(self._tab_completions)
            completion = self._tab_completions[self._current_completion_index]
            self._last_completion_text = completion

            # For cycling, we usually replace the entire text
            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=0,
                end_pos=len(current_text),
                add_space=False
            )

        # Otherwise, this is a new tab completion request
        self._tab_completions = []
        self._current_completion_index = -1

        # Parse the command line once
        self._parse_command_line(current_text, cursor_position)

        # Determine what to complete based on token context
        token = self._get_token_at_cursor()

        if not token or self._is_cursor_at_whitespace():
            # Complete new token based on context
            return self._complete_new_token(current_text)

        # Complete existing token
        if token.type == TokenType.COMMAND:
            return self._complete_command_name(token.value, token.start, token.start + len(token.value))

        if token.type == TokenType.OPTION:
            # Get the command
            command = self._command_registry.get_command(self._current_command_name)
            if not command:
                return CompletionResult(success=False)

            return self._complete_option(command, token.value, token.start, token.start + len(token.value))

        if token.type == TokenType.ARGUMENT:
            return self._complete_argument(token, current_text)

        return CompletionResult(success=False)

    def _complete_new_token(self, current_text: str) -> CompletionResult:
        """
        Complete a new token based on the current context.

        Args:
            current_text: The current command text

        Returns:
            CompletionResult with completion information
        """
        # If no command yet, suggest commands
        if not self._current_command_name:
            return self._complete_command_name("", self._cursor_position, self._cursor_position)

        # We have a command, get it
        command = self._command_registry.get_command(self._current_command_name)
        if not command:
            return CompletionResult(success=False)

        # Check if the previous token was an option that might take a value
        if self._cursor_token_index > 0:
            prev_token = self._get_previous_token(self._cursor_token_index)
            if prev_token and prev_token.type == TokenType.OPTION:
                # Convert option name to proper format (-s -> s, --long -> long)
                option_name = prev_token.value[2:] if prev_token.value.startswith('--') else prev_token.value[1:]

                # Check if this option takes a value
                options = command.setup_options()
                option = options.find_option(option_name)

                if option and option.takes_value:
                    # We're completing a value for this option
                    return self._complete_option_value(command, option_name, "", self._cursor_position, self._cursor_position)

        # Otherwise suggest new options
        return self._complete_option(command, "", self._cursor_position, self._cursor_position)

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

    def _complete_command_name(self, partial_command: str, start_pos: int, end_pos: int) -> CompletionResult:
        """
        Complete a command name.

        Args:
            partial_command: The partial command name to complete
            start_pos: Start position of the token to replace
            end_pos: End position of the token to replace

        Returns:
            CompletionResult with command name completion
        """
        command_names = self.get_available_commands()
        matches = [name for name in command_names if name.startswith(partial_command)]

        if not matches:
            return CompletionResult(success=False)

        self._tab_completions = matches

        if len(matches) == 1:
            # Single completion - return with trailing space
            completion = matches[0]
            self._last_completion_text = completion + " "

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=True
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = matches[0]
        self._last_completion_text = completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )

    def _complete_option(self, command, partial_option: str, start_pos: int, end_pos: int) -> CompletionResult:
        """
        Complete a command option.

        Args:
            command: The command object
            partial_option: The partial option to complete
            start_pos: Start position of the token to replace
            end_pos: End position of the token to replace

        Returns:
            CompletionResult with option completion
        """
        print("Completing option:", partial_option)
        options = command.setup_options()
        option_completions = options.get_option_completions(partial_option)

        if not option_completions:
            # If no option matches, try completing an argument
            remaining_text = ""
            for token in self._current_tokens:
                if token.type == TokenType.COMMAND:
                    command_token = token
                    command_end = command_token.start + len(command_token.value)
                    remaining_text = self._current_text[command_end:].lstrip()
                    break

            completions = command.get_completions(remaining_text)
            if not completions:
                return CompletionResult(success=False)

            matches = [comp for comp in completions if comp.startswith(partial_option)]
            if not matches:
                return CompletionResult(success=False)

            self._tab_completions = matches
            print("Matches:", matches)

            if len(matches) == 1:
                # Single completion
                completion = matches[0]
                is_path = completion.endswith('/')
                self._last_completion_text = completion + ("" if is_path else " ")

                return CompletionResult(
                    success=True,
                    replacement=completion,
                    start_pos=start_pos,
                    end_pos=end_pos,
                    add_space=not is_path
                )

            # Multiple completions - start cycling
            self._current_completion_index = 0
            completion = matches[0]
            self._last_completion_text = completion

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=False
            )

        self._tab_completions = option_completions

        if len(option_completions) == 1:
            print("Single option completion:", option_completions[0])
            # Single completion
            completion = option_completions[0]
            self._last_completion_text = completion + " "

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=True
            )

        # Multiple completions - start cycling
        print("Multiple option completions:", option_completions)
        self._current_completion_index = 0
        completion = option_completions[0]
        self._last_completion_text = completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )

    def _complete_argument(self, token: Token, current_text: str) -> CompletionResult:
        """
        Complete an argument token.

        Args:
            token: The argument token to complete
            current_text: The current command text

        Returns:
            CompletionResult with argument completion
        """
        # Get the command
        if not self._current_command_name:
            return CompletionResult(success=False)

        command = self._command_registry.get_command(self._current_command_name)
        if not command:
            return CompletionResult(success=False)

        # Check if this argument is an option value by looking at the previous token
        prev_token = self._get_previous_token(self._cursor_token_index)

        if prev_token and prev_token.type == TokenType.OPTION:
            # Convert option name to proper format (-s -> s, --long -> long)
            option_name = prev_token.value[2:] if prev_token.value.startswith('--') else prev_token.value[1:]

            # Check if this option takes a value
            options = command.setup_options()
            option = options.find_option(option_name)

            if option and option.takes_value:
                # We're completing a value for this option
                return self._complete_option_value(
                    command,
                    option_name,
                    token.value,
                    token.start,
                    token.start + len(token.value)
                )

        # If we get here, it's a regular argument
        start_pos = token.start
        end_pos = token.start + len(token.value)

        # Get completions from the command
        # Calculate the command line without the command name
        command_token = None
        for t in self._current_tokens:
            if t.type == TokenType.COMMAND:
                command_token = t
                break

        if command_token:
            command_end = command_token.start + len(command_token.value)
            remaining_text = current_text[command_end:].lstrip()
        else:
            remaining_text = ""

        completions = command.get_completions(remaining_text)

        if not completions:
            return CompletionResult(success=False)

        # Filter completions based on the partial argument
        partial_argument = token.value
        matches = [comp for comp in completions if comp.startswith(partial_argument)]

        if not matches:
            return CompletionResult(success=False)

        self._tab_completions = matches

        if len(matches) == 1:
            # Single completion - replace just the argument
            completion = matches[0]
            add_space = not completion.endswith('/')
            self._last_completion_text = current_text[:start_pos] + completion + (" " if add_space else "")

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=add_space
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = matches[0]
        self._last_completion_text = current_text[:start_pos] + completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )

    def _complete_option_value(
        self,
        command,
        option_name: str,
        partial_value: str,
        start_pos: int,
        end_pos: int
    ) -> CompletionResult:
        """
        Complete a value for an option.

        Args:
            command: The command object
            option_name: The option name (without dashes)
            partial_value: The partial value to complete
            start_pos: Start position of the token to replace
            end_pos: End position of the token to replace

        Returns:
            CompletionResult with option value completion
        """
        options = command.setup_options()

        # Check if this option exists and takes a value
        option = options.find_option(option_name)
        if not option or not option.takes_value:
            return CompletionResult(success=False)

        # Get value completions for this option
        value_completions = options.get_value_completions(option_name, partial_value)

        if not value_completions:
            return CompletionResult(success=False)

        self._tab_completions = value_completions

        if len(value_completions) == 1:
            # Single completion
            completion = value_completions[0]
            is_path = completion.endswith('/')
            self._last_completion_text = completion + ("" if is_path else " ")

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=not is_path
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = value_completions[0]
        self._last_completion_text = completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )
