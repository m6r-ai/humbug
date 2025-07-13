"""Command for clearing the shell history."""

import logging
from typing import List

from syntax.lexer import Token

from humbug.column_manager import ColumnManager
from humbug.tabs.shell.shell_command import ShellCommand


class ShellCommandClear(ShellCommand):
    """Command to clear the shell history."""

    def __init__(self, column_manager: ColumnManager) -> None:
        """
        Initialize the command.

        Args:
            process_clear_command_callback: Callback to process the clear command
        """
        super().__init__()
        self._column_manager = column_manager
        self._logger = logging.getLogger("ShellCommandClear")

    def name(self) -> str:
        """Get the name of the command."""
        return "clear"

    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return []

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Clear the shell history"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        self._column_manager.clear_shell_history()
        return True
