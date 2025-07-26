"""Command for opening the mindspace log."""

import logging
from typing import List

from syntax import Token

from humbug.column_manager import ColumnManager
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_message_source import ShellMessageSource


class ShellCommandLog(ShellCommand):
    """Command to open the mindspace log."""

    def __init__(self, column_manager: ColumnManager) -> None:
        """
        Initialize the command.

        Args:
            process_clear_command_callback: Callback to process the clear command
        """
        super().__init__()
        self._column_manager = column_manager
        self._logger = logging.getLogger("ShellCommandLog")

    def name(self) -> str:
        """Get the name of the command."""
        return "log"

    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return []

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens the mindspace log"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        self._column_manager.protect_current_tab(True)

        try:
            self._column_manager.show_system_log()

        finally:
            self._column_manager.protect_current_tab(False)

        self._history_manager.add_message(
            ShellMessageSource.SUCCESS,
            "Opened mindspace log"
        )

        return True
