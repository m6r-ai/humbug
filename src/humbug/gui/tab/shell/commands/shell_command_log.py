"""Command for opening the mindspace log."""

import logging
from typing import List, Callable

from humbug.gui.tab.shell.shell_command import ShellCommand
from humbug.mindspace.mindspace_message_source import MindspaceMessageSource
from humbug.syntax.lexer import Token


class ShellCommandLog(ShellCommand):
    """Command to open the mindspace log."""

    def __init__(self, process_log_command_callback: Callable[[], bool]) -> None:
        """
        Initialize the command.

        Args:
            process_clear_command_callback: Callback to process the clear command
        """
        super().__init__()
        self._process_log_command = process_log_command_callback
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
        self._process_log_command()
        self._history_manager.add_message(
            MindspaceMessageSource.SUCCESS,
            "Opened mindspace log"
        )

        return True
