"""Command for opening a side-by-side git diff tab from the system shell."""

import logging
import os
from typing import List, cast

from syntax import Token, TokenType

from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_event_source import ShellEventSource
from humbug.tabs.tab_base import TabBase


class ShellCommandDiff(ShellCommand):
    """Command to open a side-by-side git diff tab."""

    def __init__(self, column_manager: ColumnManager) -> None:
        """
        Initialize diff command.

        Args:
            column_manager: Column manager for opening diff tabs
        """
        super().__init__()
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("ShellCommandDiff")

    def name(self) -> str:
        """Get the name of the command."""
        return "diff"

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens a side-by-side git diff tab for a file"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        args = self._get_positional_arguments(tokens)
        if not args:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                "No filename specified. Usage: diff <filename>"
            )
            return False

        full_path = self._mindspace_manager.get_absolute_path(args[0])
        if not os.path.exists(full_path):
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"File not found: {args[0]}"
            )
            return False

        if os.path.isdir(full_path):
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Cannot diff a directory: {args[0]}"
            )
            return False

        current_tab = cast(TabBase, self._column_manager.get_current_tab())
        self._column_manager.protect_tab(current_tab.tab_id())

        try:
            diff_tab = self._column_manager.open_diff(full_path, False)

        finally:
            self._column_manager.unprotect_tab(current_tab.tab_id())

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"Shell opened diff for: '{full_path}'\ntab ID: {diff_tab.tab_id()}"
        )
        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            f"Opening diff: {args[0]}"
        )
        return True

    def get_token_completions(
        self,
        current_token: Token,
        _tokens: List[Token],
        _cursor_token_index: int
    ) -> List[str]:
        """
        Get completions for the current token based on token information.

        Args:
            current_token: The token at cursor position
            _tokens: All tokens in the command line
            _cursor_token_index: Index of current_token in tokens list

        Returns:
            List of possible completions
        """
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        return self._get_mindspace_path_completions(current_token.value)
