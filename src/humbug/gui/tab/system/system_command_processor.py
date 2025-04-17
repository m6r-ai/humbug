import logging
from typing import Tuple, Optional, List

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

    def handle_tab_completion(self, current_text: str) -> Tuple[bool, Optional[str], bool]:
        """
        Handle tab completion for the current input text.

        Args:
            current_text: Current input text

        Returns:
            Tuple of (success, completion, add_space) where:
                - success: Whether completion was successful
                - completion: The suggested completion if success is True, or None if no completion is available
                - add_space: Whether a space should be added after completion
        """
        current_text = current_text.strip()

        # If empty text, nothing to complete
        if not current_text:
            return False, None, False

        # If we're already cycling through completions and the current text matches our last
        # completion (minus any trailing space), continue cycling
        if (self._tab_completions and
                current_text == self._last_completion_text.rstrip()):
            # Move to next completion in the list
            self._current_completion_index = (self._current_completion_index + 1) % len(self._tab_completions)
            completion = self._tab_completions[self._current_completion_index]
            self._last_completion_text = completion
            # Don't add space when cycling between multiple options
            return True, completion, False

        # Otherwise, this is a new tab completion request
        self._tab_completions = []
        self._current_completion_index = -1

        # Parse the input to separate command and args
        parts = current_text.split(maxsplit=1)
        cmd_name = parts[0].lower()

        # Complete command name if needed
        if len(parts) == 1 and not current_text.endswith(' '):
            command_names = self._command_registry.get_command_names()
            self._tab_completions = [name for name in command_names if name.startswith(cmd_name)]

            if not self._tab_completions:
                return False, None, False

            if len(self._tab_completions) == 1:
                # Single completion - return with trailing space
                completion = f"{self._tab_completions[0]}"
                self._last_completion_text = completion
                return True, completion, True

            # Multiple completions - start cycling
            self._current_completion_index = 0
            completion = self._tab_completions[0]
            self._last_completion_text = completion
            return True, completion, False

        # Handle command option completion
        if current_text.endswith(' '):
            # We're just after a space, so this is likely a new token
            # Not much to complete here yet
            return False, None, False

        # Get the command for completion
        command = self._command_registry.get_command(cmd_name)
        if not command:
            return False, None, False

        # Get args for completion
        args = parts[1] if len(parts) > 1 else ""

        # Check if we're potentially completing an option
        if args.startswith('-'):
            # Try to get option completions
            options = command.setup_options()
            option_completions = options.get_option_completions(args)

            if option_completions:
                # Format full completions
                self._tab_completions = [f"{cmd_name} {opt}" for opt in option_completions]

                if len(self._tab_completions) == 1:
                    # Single completion
                    completion = self._tab_completions[0]
                    self._last_completion_text = completion
                    return True, completion, True

                # Multiple completions - start cycling
                self._current_completion_index = 0
                completion = self._tab_completions[0]
                self._last_completion_text = completion
                return True, completion, False

        # Check if we're completing an option value
        args_tokens = self._tokenize_args(args)
        is_option_value = False
        prefix_to_preserve = ""

        if len(args_tokens) >= 2 and args_tokens[-2].startswith('-'):
            # We might be completing a value for the previous option
            option_token = args_tokens[-2]
            option_name = option_token[2:] if option_token.startswith('--') else option_token[1:]

            # Check if this option takes a value
            options = command.setup_options()
            option = options.find_option(option_name)
            if option and option.takes_value:
                is_option_value = True

                # Calculate the prefix to preserve
                last_token_pos = args.rfind(args_tokens[-1])
                if last_token_pos > 0:
                    prefix_to_preserve = f"{cmd_name} {args[:last_token_pos]}"
                else:
                    prefix_to_preserve = f"{cmd_name} {' '.join(args_tokens[:-1])} "

        # Get completions from the command
        completions = command.get_completions(args)
        if not completions:
            return False, None, False

        # Format full completions
        full_completions = []
        for comp in completions:
            if is_option_value and prefix_to_preserve:
                full_completion = f"{prefix_to_preserve}{comp}"
            else:
                full_completion = f"{cmd_name} {comp}"

            full_completions.append(full_completion)

        self._tab_completions = full_completions

        if len(self._tab_completions) == 1:
            # Single completion - return with trailing space if not a directory
            completion = self._tab_completions[0]
            add_space = not completion.endswith('/')
            self._last_completion_text = completion
            return True, completion, add_space

        # Multiple completions - start cycling
        self._current_completion_index = 0
        completion = self._tab_completions[0]
        self._last_completion_text = completion
        return True, completion, False

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
