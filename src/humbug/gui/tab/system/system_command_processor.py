"""Processes system commands and handles tab completion."""

import logging
from typing import List, Dict, Optional

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
        self._completion_start_pos: int = 0
        self._current_completion_text: str = ""
        self._tab_completion_active: bool = False

        # Token tracking for current command
        self._current_tokens: List[Token] = []
        self._cursor_token_index: int = -1
        self._cursor_position: int = 0
        self._current_command_name: Optional[str] = None
        self._token_map: Dict[TokenType, List[Token]] = {}

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
            # Unescape the command text
            unescaped_command = self._unescape_text(command_text)

            # Parse the command line
            self._parse_command_line(unescaped_command, len(unescaped_command))

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

            # Pass the tokens and full text to the command
            success = command.execute(self._current_tokens, unescaped_command)
            if not success:
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"Error executing command '{cmd}'. Check arguments and try again."
                )

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
        self._token_map.clear()

        # Tokenize the input
        lexer = CommandLexer()
        lexer.lex(None, current_text)

        # Store tokens
        self._current_tokens = []
        token = lexer.get_next_token()
        while token is not None:
            self._current_tokens.append(token)

            # Add to token map by type for quick lookup
            if token.type not in self._token_map:
                self._token_map[token.type] = []
            self._token_map[token.type].append(token)

            token = lexer.get_next_token()

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
        self._cursor_position = 0
        self._current_command_name = None
        self._token_map.clear()

    def _get_token_at_cursor(self) -> Optional[Token]:
        """
        Get the token at the cursor position.

        Returns:
            The token at the cursor position, or None if not found
        """
        if 0 <= self._cursor_token_index < len(self._current_tokens):
            return self._current_tokens[self._cursor_token_index]

        return None

    def _get_previous_token(self, token_index: int) -> Optional[Token]:
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

    def _get_command_name(self, tokens: List[Token]) -> Optional[str]:
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
        cursor_position: Optional[int] = None
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
        current_text = current_text.strip()

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

        # Generate completions based on token type and context
        if not token or self._is_cursor_at_whitespace():
            # Complete new token based on context
            result = self._complete_new_token()
        elif token.type == TokenType.COMMAND:
            result = self._complete_command_name(token)
        elif token.type == TokenType.OPTION:
            # Get the command
            command = self._command_registry.get_command(self._current_command_name)
            if not command:
                return CompletionResult(success=False)

            result = self._complete_option(command, token)
        elif token.type == TokenType.ARGUMENT:
            result = self._complete_argument(token)
        else:
            return CompletionResult(success=False)

        if result.success and result.replacement is not None:
            self._tab_completion_active = True
            # Store the current completion text for next cycle
            self._current_completion_text = result.replacement

        return result

    def _complete_new_token(self) -> CompletionResult:
        """
        Complete a new token based on the current context.

        Returns:
            CompletionResult with completion information
        """
        # If no command yet, suggest commands
        if not self._current_command_name:
            empty_token = Token(TokenType.COMMAND, "", self._cursor_position)
            return self._complete_command_name(empty_token)

        # We have a command, get it
        command = self._command_registry.get_command(self._current_command_name)
        if not command:
            return CompletionResult(success=False)

        # Check if the previous token was an option that might take a value
        if self._cursor_token_index > 0:
            prev_token = self._get_previous_token(self._cursor_token_index)
            if prev_token and prev_token.type == TokenType.OPTION:
                # Complete a value for this option
                empty_value_token = Token(TokenType.ARGUMENT, "", self._cursor_position)
                return self._complete_option_value(command, prev_token.value, empty_value_token)

        # Otherwise suggest new options
        empty_option_token = Token(TokenType.OPTION, "", self._cursor_position)
        return self._complete_option(command, empty_option_token)

    def _complete_command_name(self, token: Token) -> CompletionResult:
        """
        Complete a command name.

        Args:
            token: The command token to complete

        Returns:
            CompletionResult with command name completion
        """
        partial_command = token.value
        start_pos = token.start
        end_pos = token.start + len(partial_command)

        command_names = self.get_available_commands()
        matches = [name for name in command_names if name.startswith(partial_command)]

        if not matches:
            return CompletionResult(success=False)

        self._tab_completions = matches

        if len(matches) == 1:
            # Single completion - return with trailing space
            completion = matches[0]
            self._current_completion_index = 0

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

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )

    def _complete_option(self, command, token: Token) -> CompletionResult:
        """
        Complete a command option.

        Args:
            command: The command object
            token: The option token to complete

        Returns:
            CompletionResult with option completion
        """
        partial_option = token.value
        start_pos = token.start
        end_pos = token.start + len(partial_option)

        # Get option completions from the command
        option_completions = command._get_option_completions(partial_option)

        if not option_completions:
            # If no option matches, try completing an argument
            completions = command.get_token_completions(
                token,
                self._current_tokens,
                self._cursor_token_index
            )

            if not completions:
                return CompletionResult(success=False)

            matches = [comp for comp in completions if comp.startswith(partial_option)]
            if not matches:
                return CompletionResult(success=False)

            # Escape spaces in completions
            escaped_matches = [self._escape_text(match) for match in matches]
            self._tab_completions = escaped_matches

            if len(matches) == 1:
                # Single completion
                completion = escaped_matches[0]
                is_path = completion.endswith('/')
                self._current_completion_index = 0

                return CompletionResult(
                    success=True,
                    replacement=completion,
                    start_pos=start_pos,
                    end_pos=end_pos,
                    add_space=not is_path
                )

            # Multiple completions - start cycling
            self._current_completion_index = 0
            completion = escaped_matches[0]

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=False
            )

        self._tab_completions = option_completions

        if len(option_completions) == 1:
            # Single completion
            completion = option_completions[0]
            self._current_completion_index = 0

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
        token: Token
    ) -> CompletionResult:
        """
        Complete a value for an option.

        Args:
            command: The command object
            option_name: The option name (with dashes)
            token: The token containing the partial value

        Returns:
            CompletionResult with option value completion
        """
        partial_value = token.value
        start_pos = token.start
        end_pos = token.start + len(partial_value)

        # Get completions from the command's token completions method
        completions = command.get_token_completions(
            token,
            self._current_tokens,
            self._cursor_token_index
        )

        if not completions:
            return CompletionResult(success=False)

        # Filter completions based on the partial value
        unescaped_partial = self._unescape_text(partial_value)
        matches = [comp for comp in completions if comp.startswith(unescaped_partial)]

        if not matches:
            return CompletionResult(success=False)

        # Escape spaces in completions
        escaped_completions = [self._escape_text(comp) for comp in matches]
        self._tab_completions = escaped_completions

        if len(matches) == 1:
            # Single completion
            completion = escaped_completions[0]
            is_path = completion.endswith('/')
            self._current_completion_index = 0

            return CompletionResult(
                success=True,
                replacement=completion,
                start_pos=start_pos,
                end_pos=end_pos,
                add_space=not is_path
            )

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = escaped_completions[0]

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )

    def _complete_argument(self, token: Token) -> CompletionResult:
        """
        Complete an argument token.

        Args:
            token: The argument token to complete

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
            # We're completing a value for this option
            return self._complete_option_value(command, prev_token.value, token)

        # If we get here, it's a regular argument
        start_pos = token.start
        end_pos = token.start + len(token.value)

        # Get completions from the command
        completions = command.get_token_completions(
            token,
            self._current_tokens,
            self._cursor_token_index
        )

        if not completions:
            return CompletionResult(success=False)

        # Filter completions based on the partial argument
        partial_argument = token.value
        # Unescape the partial argument for matching against completions
        unescaped_partial = self._unescape_text(partial_argument)
        matches = [comp for comp in completions if comp.startswith(unescaped_partial)]

        if not matches:
            return CompletionResult(success=False)

        # Escape spaces in completions
        escaped_matches = [self._escape_text(match) for match in matches]
        self._tab_completions = escaped_matches

        if len(matches) == 1:
            # Single completion - replace just the argument
            completion = escaped_matches[0]
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
        completion = escaped_matches[0]

        return CompletionResult(
            success=True,
            replacement=completion,
            start_pos=start_pos,
            end_pos=end_pos,
            add_space=False
        )
