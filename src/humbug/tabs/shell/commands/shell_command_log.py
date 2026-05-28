"""Command for opening the mindspace log."""

import logging
from typing import List

from syntax import Token

from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_event_source import ShellEventSource


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
        current_tab = self._column_manager.get_current_tab()
        assert current_tab is not None
        self._column_manager.protect_tab(current_tab.tab_id())

        try:
            contexts = self._mindspace.contexts()
            existing = next((i for i in contexts.list_all() if i.context_type == "log"), None)
            if existing:
                contexts.focus(existing.context_id)

            else:
                contexts.open(context_type="log", title="Mindspace Log")

        finally:
            self._column_manager.unprotect_tab(current_tab.tab_id())

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            "Opened mindspace log"
        )

        return True
