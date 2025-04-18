"""Processes system commands and handles tab completion."""

import logging
from typing import List, Optional, Tuple

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
            # Lex the command text to get tokens
            lexer = CommandLexer()
            lexer.lex(None, command_text)

            # Get the command name (first token)
            cmd = self._get_command_name(lexer)
            if not cmd:
                # No command name found
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    "Invalid command format. Type 'help' for a list of available commands."
                )
                return

            # Get the remaining arguments (everything after the command name)
            args = command_text[len(cmd):].lstrip() if len(cmd) < len(command_text) else ""

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

    def handle_tab_completion(self, current_text: str) -> CompletionResult:
        """
        Handle tab completion for the current input text.

        Args:
            current_text: Current input text

        Returns:
            CompletionResult with information about what to replace
        """
        current_text = current_text.strip()

        # If empty text, nothing to complete
        if not current_text:
            return CompletionResult(success=False)

        # If we're cycling through completions and the current text matches our last
        # completion (minus any trailing space), continue cycling
        if (self._tab_completions and
                current_text == self._last_completion_text.rstrip()):
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

        # Tokenize the input
        lexer = CommandLexer()
        lexer.lex(None, current_text)

        # Get the position of the cursor (end of text for tab completion)
        cursor_position = len(current_text)

        # Get the token at the cursor position
        current_token = self._get_token_at_position(lexer, cursor_position - 1) if cursor_position > 0 else None

        # If no token at cursor, we're at whitespace at the end - prepare to add a new token
        if not current_token or cursor_position > (current_token.start + len(current_token.value)):
            # Check if we have a command yet
            command_name = self._get_command_name(lexer)

            # If no command, offer command completions
            if not command_name:
                return self._complete_command_name(current_text, "")

            # If we have a command but cursor is after a space, we could be starting an option or argument
            command = self._command_registry.get_command(command_name)
            if not command:
                return CompletionResult(success=False)

            # Get the last token to see what we might be completing
            last_token = lexer.peek_next_token(-1)

            # If the last token was an option, we might be starting an option value
            if last_token and last_token.type == TokenType.OPTION:
                # Convert option name to proper format (-s -> s, --long -> long)
                option_name = last_token.value[2:] if last_token.value.startswith('--') else last_token.value[1:]

                # Check if this option takes a value
                options = command.setup_options()
                option = options.find_option(option_name)

                if option and option.takes_value:
                    # We're completing a value for this option
                    return self._complete_option_value(command, option_name, "", current_text)

            # Otherwise suggest new options or arguments based on command context
            # We'll prioritize options first
            return self._complete_option(command, "", current_text)

        # We have a token at cursor position - what kind is it?
        if current_token.type == TokenType.COMMAND:
            # Completing the command name
            partial_command = current_token.value
            return self._complete_command_name(current_text, partial_command)

        if current_token.type == TokenType.OPTION:
            # Completing an option
            command_name = self._get_command_name(lexer)
            if not command_name:
                return CompletionResult(success=False)

            command = self._command_registry.get_command(command_name)
            if not command:
                return CompletionResult(success=False)

            partial_option = current_token.value
            return self._complete_option(command, partial_option, current_text)

        if current_token.type == TokenType.ARGUMENT:
            # This could be either an option value or a regular argument
            # We need to check the context
            command_name = self._get_command_name(lexer)
            if not command_name:
                return CompletionResult(success=False)

            command = self._command_registry.get_command(command_name)
            if not command:
                return CompletionResult(success=False)

            # Try to find if this argument is an option value
            # by looking at the previous token
            previous_token = self._get_token_before(lexer, current_token)

            if previous_token and previous_token.type == TokenType.OPTION:
                # This argument might be a value for the previous option
                option_name = previous_token.value[2:] if previous_token.value.startswith('--') else previous_token.value[1:]

                # Check if this option takes a value
                options = command.setup_options()
                option = options.find_option(option_name)

                if option and option.takes_value:
                    # We're completing a value for this option
                    partial_value = current_token.value
                    return self._complete_option_value(command, option_name, partial_value, current_text)

            # If we get here, it's a regular argument (or we couldn't determine it's an option value)
            # Calculate the parts of the text to keep/replace
            start_pos = current_token.start
            end_pos = current_token.start + len(current_token.value)

            # Get completions from the command
            remaining_text = current_text[len(command_name):].lstrip()
            completions = command.get_completions(remaining_text)

            if not completions:
                return CompletionResult(success=False)

            # Filter completions based on the partial argument
            partial_argument = current_token.value
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

    def _get_token_at_position(self, lexer: CommandLexer, position: int) -> Token | None:
        """
        Get the token at the specified character position in the input.

        Args:
            lexer: The lexer containing the tokens
            position: The character position to find a token for

        Returns:
            The token at the position, or None if no token is at that position
        """
        # Save current token position
        current_position = lexer._next_token

        # Reset to beginning
        lexer._next_token = 0

        # Scan through tokens to find one at the right position
        token = lexer.get_next_token()
        while token:
            token_end = token.start + len(token.value)
            if token.start <= position < token_end:
                # Restore position
                lexer._next_token = current_position
                return token

            token = lexer.get_next_token()

        # Restore position
        lexer._next_token = current_position
        return None

    def _get_token_before(self, lexer: CommandLexer, target_token: Token) -> Token | None:
        """
        Get the token that comes before the specified token.

        Args:
            lexer: The lexer containing the tokens
            target_token: The token to find the predecessor for

        Returns:
            The token before the target token, or None if no such token exists
        """
        # Save current token position
        current_position = lexer._next_token

        # Reset to beginning
        lexer._next_token = 0

        previous_token = None
        token = lexer.get_next_token()

        while token:
            if token.start == target_token.start and token.value == target_token.value:
                # Found target token, return the previous one
                lexer._next_token = current_position
                return previous_token

            previous_token = token
            token = lexer.get_next_token()

        # Restore position
        lexer._next_token = current_position
        return None

    def _get_command_name(self, lexer: CommandLexer) -> str | None:
        """
        Get the command name from the tokens.

        Args:
            lexer: The lexer containing the tokens

        Returns:
            The command name if found, None otherwise
        """
        # Save current token position
        current_position = lexer._next_token

        # Reset to beginning
        lexer._next_token = 0

        # Find first token of type COMMAND
        token = lexer.get_next_token()
        while token:
            if token.type == TokenType.COMMAND:
                # Restore position
                lexer._next_token = current_position
                return token.value

            token = lexer.get_next_token()

        # Restore position
        lexer._next_token = current_position
        return None

    def _get_tokens_by_type(self, lexer: CommandLexer, token_type: TokenType) -> List[Token]:
        """
        Get all tokens of a specific type.

        Args:
            lexer: The lexer containing the tokens
            token_type: The type of tokens to retrieve

        Returns:
            A list of tokens matching the requested type
        """
        # Save current token position
        current_position = lexer._next_token

        # Reset to beginning
        lexer._next_token = 0

        # Collect all tokens of the specified type
        matching_tokens = []
        token = lexer.get_next_token()
        while token:
            if token.type == token_type:
                matching_tokens.append(token)

            token = lexer.get_next_token()

        # Restore position
        lexer._next_token = current_position
        return matching_tokens

    def _complete_command_name(self, current_text: str, partial_command: str) -> CompletionResult:
        """
        Complete a command name.

        Args:
            current_text: The full current text
            partial_command: The partial command name to complete

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
                start_pos=0,
                end_pos=len(partial_command),
                add_space=True
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = matches[0]
        self._last_completion_text = completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=0,
            end_pos=len(partial_command),
            add_space=False
        )

    def _complete_option(self, command, partial_option: str, current_text: str) -> CompletionResult:
        """
        Complete a command option.

        Args:
            command: The command object
            partial_option: The partial option to complete
            current_text: The full current text

        Returns:
            CompletionResult with option completion
        """
        options = command.setup_options()
        option_completions = options.get_option_completions(partial_option)

        if not option_completions:
            return CompletionResult(success=False)

        self._tab_completions = option_completions

        # Find token position
        start_pos = current_text.find(partial_option)
        if start_pos == -1:
            start_pos = len(current_text)

        end_pos = start_pos + len(partial_option)

        if len(option_completions) == 1:
            # Single completion
            completion = option_completions[0]
            self._last_completion_text = current_text[:start_pos] + completion + " "

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=True
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = option_completions[0]
        self._last_completion_text = current_text[:start_pos] + completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )

    def _complete_option_value(self, command, option_name: str, partial_value: str, current_text: str) -> CompletionResult:
        """
        Complete a value for an option.

        Args:
            command: The command object
            option_name: The option name (without dashes)
            partial_value: The partial value to complete
            current_text: The full current text

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

        # Find token position
        start_pos = current_text.rfind(partial_value)
        if start_pos == -1:
            # If not found, assume it's at the end
            start_pos = len(current_text)

        end_pos = start_pos + len(partial_value)

        self._tab_completions = value_completions

        if len(value_completions) == 1:
            # Single completion
            completion = value_completions[0]
            is_path = completion.endswith('/')
            self._last_completion_text = current_text[:start_pos] + completion + ("" if is_path else " ")

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
        self._last_completion_text = current_text[:start_pos] + completion

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )
