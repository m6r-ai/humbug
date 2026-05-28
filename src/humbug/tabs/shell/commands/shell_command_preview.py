"""Command for opening pages in a preview tab from the system shell."""

import logging
import os
from typing import List

from syntax import Token, TokenType

from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_event_source import ShellEventSource


class ShellCommandPreview(ShellCommand):
    """Command to open a preview tab."""

    def __init__(self, column_manager: ColumnManager) -> None:
        """
        Initialize preview command.

        Args:
            open_file_callback: Callback to open an existing file
        """
        super().__init__()
        self._column_manager = column_manager
        self._logger = logging.getLogger("ShellCommandPreview")

    def name(self) -> str:
        """Get the name of the command."""
        return "preview"

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens a preview tab"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        # Get positional arguments
        args = self._get_positional_arguments(tokens)
        if not args:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                "No filename specified. Usage: preview <filename>"
            )
            return False

        full_path = self._mindspace.get_absolute_path(args[0])
        if not os.path.exists(full_path):
            # Create directory if ned
            directory = os.path.dirname(full_path)
            if directory and not os.path.exists(directory):
                try:
                    os.makedirs(directory, exist_ok=True)

                except OSError as e:
                    self._history_manager.add_message(
                        ShellEventSource.ERROR,
                        f"Failed to create directory: {str(e)}"
                    )
                    return False

        current_tab = self._column_manager.get_current_tab()
        assert current_tab is not None
        self._column_manager.protect_tab(current_tab.tab_id())

        try:
            contexts = self._mindspace.contexts()
            existing = contexts.get_by_path_and_type(full_path, "preview")
            if existing:
                contexts.focus(existing.context_id)

            else:
                contexts.open(
                    context_type="preview",
                    path=full_path,
                    title=os.path.basename(full_path),
                )

        finally:
            self._column_manager.unprotect_tab(current_tab.tab_id())

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            f"Opening page: {args[0]}"
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
            tokens: All tokens in the command line
            cursor_token_index: Index of current_token in tokens list

        Returns:
            List of possible completions
        """
        # For the preview command, we're primarily interested in completing file paths
        # Only handle options if we're explicitly looking at an option token
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # For arguments, complete file paths
        return self._get_mindspace_path_completions(current_token.value)
