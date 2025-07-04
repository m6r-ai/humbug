"""Command for clearing the system shell history."""

import logging
from typing import List, Callable

from humbug.mindspace.system.system_command import SystemCommand
from humbug.syntax.command.command_lexer import Token


class ClearCommand(SystemCommand):
    """Command to clear the system shell history."""

    def __init__(self, process_clear_command_callback: Callable[[], bool]) -> None:
        """
        Initialize the command.

        Args:
            process_clear_command_callback: Callback to process the clear command
        """
        super().__init__()
        self._process_clear_command = process_clear_command_callback
        self._logger = logging.getLogger("ClearCommand")

    def name(self) -> str:
        """Get the name of the command."""
        return "clear"

    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return []

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Clear the system shell history"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        return self._process_clear_command()
