import logging
import re
from typing import Tuple, Optional, List

from humbug.gui.tab.system.completion_result import CompletionResult
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.mindspace.system.system_command_registry import SystemCommandRegistry

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
            # Split only the first token to get the command name
            parts = command_text.split(maxsplit=1)
            cmd = parts[0].lower()
            args = parts[1] if len(parts) > 1 else ""

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

        # Tokenize the input text to find token boundaries
        tokens = self._tokenize_args(current_text)
        token_positions = self._find_token_positions(current_text, tokens)

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

        # Parse the input to separate command and args
        parts = current_text.split(maxsplit=1)
        cmd_name = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""

        # Complete command name if needed
        if len(parts) == 1 and not current_text.endswith(' '):
            command_names = self._command_registry.get_command_names()
            matches = [name for name in command_names if name.startswith(cmd_name)]

            if not matches:
                return CompletionResult(success=False)

            self._tab_completions = matches

            if len(matches) == 1:
                # Single completion - return with trailing space
                completion = matches[0]
                self._last_completion_text = completion
                return CompletionResult(
                    success=True,
                    replacement=completion,
                    start_pos=0,  # Replace the entire command
                    end_pos=len(cmd_name),
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
                end_pos=len(cmd_name),
                add_space=False
            )

        # Get the command for completion
        command = self._command_registry.get_command(cmd_name)
        if not command:
            return CompletionResult(success=False)

        # Handle option completion
        if args.endswith(' '):
            # We're just after a space, so this is likely a new token
            # Not much to complete here yet
            return CompletionResult(success=False)

        # Handle command option completion
        if args.startswith('-'):
            # Try to get option completions
            options = command.setup_options()
            option_completions = options.get_option_completions(args)

            if option_completions:
                # Format full completions - replace just the option part
                self._tab_completions = option_completions

                if len(option_completions) == 1:
                    # Single completion
                    completion = option_completions[0]
                    self._last_completion_text = f"{cmd_name} {completion}"
                    start_pos = len(cmd_name) + 1  # After command name and space
                    end_pos = len(current_text)  # To the end of the text
                    return CompletionResult(
                        success=True,
                        replacement=completion,
                        start_pos=start_pos,
                        end_pos=end_pos,
                        add_space=True
                    )

                # Multiple completions - start cycling (replace whole text for simplicity)
                self._current_completion_index = 0
                completion = f"{cmd_name} {option_completions[0]}"
                self._last_completion_text = completion
                return CompletionResult(
                    success=True,
                    replacement=completion,
                    start_pos=0,
                    end_pos=len(current_text),
                    add_space=False
                )

        # Check if we're completing an option value
        args_tokens = self._tokenize_args(args)
        if len(args_tokens) >= 2 and args_tokens[-2].startswith('-'):
            # We might be completing a value for the previous option
            option_token = args_tokens[-2]
            option_name = option_token[2:] if option_token.startswith('--') else option_token[1:]

            # Check if this option takes a value
            options = command.setup_options()
            option = options.find_option(option_name)
            if option and option.takes_value:
                # We're completing a value for this option
                current_value = args_tokens[-1]
                option_completions = options.get_value_completions(option_name, current_value)

                if option_completions:
                    # Get the position of the value token we're completing
                    args_token_positions = self._find_token_positions(args, args_tokens)
                    value_start_pos = args_token_positions[-1][0]
                    value_end_pos = args_token_positions[-1][1]

                    # Adjust to account for the command name and space
                    value_start_pos += len(cmd_name) + 1
                    value_end_pos += len(cmd_name) + 1

                    self._tab_completions = option_completions

                    if len(option_completions) == 1:
                        # Single completion - replace just the value
                        completion = option_completions[0]
                        self._last_completion_text = f"{cmd_name} {args[:value_start_pos-len(cmd_name)-1]}{completion}"
                        return CompletionResult(
                            success=True,
                            replacement=completion,
                            start_pos=value_start_pos,
                            end_pos=value_end_pos,
                            add_space=True
                        )

                    # Multiple completions - start cycling (replace the value token)
                    self._current_completion_index = 0
                    completion = option_completions[0]
                    full_text = f"{cmd_name} {args[:value_start_pos-len(cmd_name)-1]}{completion}"
                    self._last_completion_text = full_text
                    return CompletionResult(
                        success=True,
                        replacement=completion,
                        start_pos=value_start_pos,
                        end_pos=value_end_pos,
                        add_space=False
                    )

        # Get completions from the command
        completions = command.get_completions(args)
        if completions:
            self._tab_completions = completions

            # For file completions, we typically replace the last token
            if len(tokens) > 1:
                last_token_idx = len(tokens) - 1
                last_token_start = token_positions[last_token_idx][0]
                last_token_end = token_positions[last_token_idx][1]

                if len(completions) == 1:
                    # Single completion - replace just the last token
                    completion = completions[0]
                    add_space = not completion.endswith('/')
                    self._last_completion_text = f"{current_text[:last_token_start]}{completion}"
                    return CompletionResult(
                        success=True,
                        replacement=completion,
                        start_pos=last_token_start,
                        end_pos=last_token_end,
                        add_space=add_space
                    )

                # Multiple completions - start cycling
                self._current_completion_index = 0
                completion = completions[0]
                self._last_completion_text = f"{current_text[:last_token_start]}{completion}"
                return CompletionResult(
                    success=True,
                    replacement=completion,
                    start_pos=last_token_start,
                    end_pos=last_token_end,
                    add_space=False
                )

            # Fallback to full text replacement if we can't determine token boundaries
            if len(completions) == 1:
                # Single completion
                full_text = f"{cmd_name} {completions[0]}"
                self._last_completion_text = full_text
                return CompletionResult(
                    success=True,
                    replacement=full_text,
                    start_pos=0,
                    end_pos=len(current_text),
                    add_space=not completions[0].endswith('/')
                )

            # Multiple completions
            self._current_completion_index = 0
            full_text = f"{cmd_name} {completions[0]}"
            self._last_completion_text = full_text
            return CompletionResult(
                success=True,
                replacement=full_text,
                start_pos=0,
                end_pos=len(current_text),
                add_space=False
            )

        return CompletionResult(success=False)

    def _tokenize_args(self, args_string: str) -> List[str]:
        """
        Tokenize arguments string, preserving quoted strings.

        Args:
            args_string: Arguments string to tokenize

        Returns:
            List of tokens
        """
        tokens = []
        current = ''
        in_quotes = False
        quote_char = None

        for char in args_string:
            if char in ('"', "'"):
                if not in_quotes:
                    in_quotes = True
                    quote_char = char
                elif char == quote_char:
                    in_quotes = False
                    quote_char = None
                current += char
            elif char.isspace() and not in_quotes:
                if current:
                    tokens.append(current)
                    current = ''
            else:
                current += char

        if current:
            tokens.append(current)

        return tokens

    def _find_token_positions(self, text: str, tokens: List[str]) -> List[Tuple[int, int]]:
        """
        Find the start and end positions of each token in the text.

        Args:
            text: The full text
            tokens: The tokenized text

        Returns:
            List of (start_pos, end_pos) tuples for each token
        """
        positions = []
        search_start = 0

        for token in tokens:
            # Handle quoted tokens specially
            if (token.startswith('"') and token.endswith('"')) or \
            (token.startswith("'") and token.endswith("'")):
                # Look for the exact token including quotes
                pos = text.find(token, search_start)
            else:
                # For unquoted tokens, find the next occurrence after search_start
                pos = text.find(token, search_start)

                # Verify it's a whole word by checking boundaries
                if pos != -1:
                    is_word_start = pos == 0 or text[pos-1].isspace()
                    is_word_end = pos + len(token) >= len(text) or text[pos + len(token)].isspace()

                    # If not a whole word, try to find the next occurrence
                    if not (is_word_start and is_word_end):
                        # Look for token with space before/after or at start/end of string
                        pos = -1
                        i = search_start
                        while i < len(text):
                            i = text.find(token, i)
                            if i == -1:
                                break

                            is_word_start = i == 0 or text[i-1].isspace()
                            is_word_end = i + len(token) >= len(text) or text[i + len(token)].isspace()

                            if is_word_start and is_word_end:
                                pos = i
                                break

                            i += 1

            if pos != -1:
                start_pos = pos
                end_pos = pos + len(token)
                positions.append((start_pos, end_pos))
                search_start = end_pos
            else:
                # This should not happen with proper tokenization, but fall back to
                # appending token at the current search position
                positions.append((search_start, search_start + len(token)))
                search_start += len(token)

        return positions
