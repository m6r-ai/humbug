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

    def process_command(self, command_text: str) -> None:
        """
        Process a command string and execute appropriate action.

        Args:
            command_text: The command text to process
        """
        command_text = command_text.strip()
        if not command_text:
            return

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

    def handle_tab_completion(self, current_text: str) -> Tuple[bool, Optional[str]]:
        """
        Handle tab completion for the current input text.

        Args:
            current_text: Current input text

        Returns:
            Tuple of (success, completion) where completion is the suggested
            completion if success is True, or None if no completion is available
        """
        current_text = current_text.strip()

        # If empty text, nothing to complete
        if not current_text:
            return False, None

        parts = current_text.split(maxsplit=1)
        cmd_name = parts[0].lower()

        # If we only have a partial command with no args, try to complete the command name
        if len(parts) == 1 and not current_text.endswith(' '):
            command_names = self._command_registry.get_command_names()
            completions = [name for name in command_names if name.startswith(cmd_name)]

            if not completions:
                # No completions available
                return False, None

            if len(completions) == 1:
                # Single completion - return the full command with trailing space
                return True, f"{completions[0]} "

            # Multiple completions - find common prefix
            common_prefix = completions[0]
            for completion in completions[1:]:
                i = 0
                while i < len(common_prefix) and i < len(completion) and common_prefix[i] == completion[i]:
                    i += 1
                common_prefix = common_prefix[:i]

            if len(common_prefix) > len(cmd_name):
                return True, common_prefix

            # No common prefix longer than current command
            return False, None

        # We have a command and potentially args, get command for completion
        command = self._command_registry.get_command(cmd_name)
        if not command:
            return False, None

        # Get args for completion
        args = parts[1] if len(parts) > 1 else ""

        # Check if we're dealing with an option value
        args_tokens = self._tokenize_args(args)

        # Get completions from the command
        completions = command.get_completions(args)

        if not completions:
            return False, None

        # Check if we're completing an option value rather than a new option
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

                # Calculate the prefix to preserve (everything up to the last token)
                last_token_pos = args.rfind(args_tokens[-1])
                if last_token_pos > 0:
                    prefix_to_preserve = f"{cmd_name} {args[:last_token_pos]}"
                else:
                    # If we can't find the last token position, preserve everything except the last token
                    prefix_to_preserve = f"{cmd_name} {' '.join(args_tokens[:-1])} "

        if len(completions) == 1:
            completion = completions[0]
            # If completion doesn't end with space and isn't a directory (ending with /)
            # append a space for convenience
            if not completion.endswith(' ') and not completion.endswith('/'):
                completion += ' '

            if is_option_value and prefix_to_preserve:
                return True, f"{prefix_to_preserve}{completion}"
            else:
                return True, f"{cmd_name} {completion}"

        # Multiple completions - find common prefix
        common_prefix = completions[0]
        for completion in completions[1:]:
            i = 0
            while i < len(common_prefix) and i < len(completion) and common_prefix[i] == completion[i]:
                i += 1
            common_prefix = common_prefix[:i]

        # Check if common prefix is longer than what's already typed
        current_partial = args_tokens[-1] if args_tokens else ""
        if common_prefix and len(common_prefix) > len(current_partial):
            if is_option_value and prefix_to_preserve:
                return True, f"{prefix_to_preserve}{common_prefix}"
            else:
                return True, f"{cmd_name} {common_prefix}"

        # No common prefix longer than current args
        return False, None

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
