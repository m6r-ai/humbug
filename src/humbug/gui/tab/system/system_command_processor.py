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

        try:
            success = command.execute(args)
            if not success:
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"Error executing command '{cmd}'. Check arguments and try again."
                )

        except Exception as e:
            self._logger.error("Error executing command '%s': %s", cmd, str(e))
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
        parts = current_text.strip().split(maxsplit=1)
        cmd = parts[0] if parts else ""

        # If we have a partial command with no args, try to complete it
        if cmd and (len(parts) == 1):
            command_names = self._command_registry.get_command_names()
            completions = [name for name in command_names if name.startswith(cmd.lower())]

            if not completions:
                # No completions
                return False, None

            if len(completions) == 1:
                # Single completion - return the full command
                return True, completions[0]

            # Multiple completions - find common prefix
            common_prefix = completions[0]
            for completion in completions[1:]:
                i = 0
                while i < len(common_prefix) and i < len(completion) and common_prefix[i] == completion[i]:
                    i += 1

                common_prefix = common_prefix[:i]

            if len(common_prefix) > len(cmd):
                return True, common_prefix

            # No common prefix longer than current command
            return False, None

        # Check for argument completions if command exists
        if len(parts) > 1:
            command = self._command_registry.get_command(cmd.lower())
            if command:
                args = parts[1] if len(parts) > 1 else ""
                arg_completions = command.get_completions(args)

                if arg_completions and len(arg_completions) == 1:
                    return True, f"{cmd} {arg_completions[0]}"

                # Could implement more sophisticated argument completion here

        # No completions available
        return False, None
