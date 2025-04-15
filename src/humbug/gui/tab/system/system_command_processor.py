"""Command processor for system tab terminal functionality."""

import logging
from typing import Callable, Dict

from PySide6.QtWidgets import QWidget

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_message_source import SystemMessageSource


class SystemCommandProcessor:
    """Processes system commands and executes appropriate actions."""

    def __init__(self, parent_widget: QWidget) -> None:
        """
        Initialize command processor with parent widget for context.
        
        Args:
            parent_widget: Parent widget that provides access to application components
        """
        self._parent = parent_widget
        self._logger = logging.getLogger("CommandProcessor")
        self._mindspace_manager = MindspaceManager()

        # Command registry
        self._commands: Dict[str, Callable[[str], bool]] = {
            "terminal": self._cmd_terminal,
            "help": self._cmd_help,
            "?": self._cmd_help
        }

    def process_command(self, command_text: str) -> bool:
        """
        Process a command string and execute appropriate action.
        
        Args:
            command_text: The command text to process
            
        Returns:
            True if command was successfully processed, False otherwise
        """
        command_text = command_text.strip()
        if not command_text:
            return False

        parts = command_text.split(maxsplit=1)
        cmd = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""

        if cmd in self._commands:
            try:
                return self._commands[cmd](args)

            except Exception as e:
                self._logger.error("Error executing command '%s': %s", cmd, str(e))
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"Error executing command: {str(e)}"
                )
                return False
        else:
            return False

    def _cmd_terminal(self, args: str) -> bool:
        """
        Create a new terminal tab.
        
        Args:
            args: Command arguments (unused)
            
        Returns:
            True on success
        """
        window = self._parent.window()
#        window._column_manager.new_terminal()

        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            "New terminal tab created."
        )
        return True

    def _cmd_help(self, args: str) -> bool:
        """
        Display available commands.
        
        Args:
            args: Command arguments (unused)
            
        Returns:
            True on success
        """
        # Format and display the list of available commands
        help_text = "Available commands:\n"
        help_text += "  terminal - Open a new terminal tab\n"
        help_text += "  help, ?  - Show this help message\n\n"
        help_text += "Commands can be submitted with Enter or Ctrl+J (⌘J on macOS).\n"
        help_text += "Use Up/Down arrows to navigate command history.\n"
        help_text += "Use PageUp/PageDown to jump to start/end of current command."

        # Add the help text to the system messages as a success message
        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS, 
            help_text
        )
        return True

    def get_available_commands(self) -> list[str]:
        """
        Return list of available command names.
        
        Returns:
            List of command names
        """
        return list(self._commands.keys())
