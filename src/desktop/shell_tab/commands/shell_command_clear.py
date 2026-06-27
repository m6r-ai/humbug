"""Command for clearing the shell history."""

from collections.abc import Callable
import logging

from syntax import Token

from desktop.shell_tab.shell_command import ShellCommand


class ShellCommandClear(ShellCommand):
    """Command to clear the shell history."""

    def __init__(self, clear_callback: Callable[[], None]) -> None:
        """
        Initialize the command.

        Args:
            clear_callback: Callback to invoke when clearing the shell history
        """
        super().__init__()
        self._clear_callback = clear_callback
        self._logger = logging.getLogger("ShellCommandClear")

    def name(self) -> str:
        """Get the name of the command."""
        return "clear"

    def aliases(self) -> list[str]:
        """Get alternate names for the command."""
        return []

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Clear the shell history"

    def _execute_command(self, tokens: list[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        self._clear_callback()
        return True
