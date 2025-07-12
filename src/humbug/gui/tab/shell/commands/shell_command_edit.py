"""Command for opening or creating files in an editor tab from the system shell."""

import logging
import os
from typing import List, Callable

from humbug.gui.tab.shell.shell_command import ShellCommand
from humbug.gui.tab.shell.shell_message_source import ShellMessageSource
from syntax.lexer import Token, TokenType
from humbug.mindspace.mindspace_manager import MindspaceManager


class ShellCommandEdit(ShellCommand):
    """Command to open or create a file in an editor tab."""

    def __init__(self, edit_file_callback: Callable[[str], bool]) -> None:
        """
        Initialize edit command.

        Args:
            open_file_callback: Callback to open an existing file
        """
        super().__init__()
        self._edit_file = edit_file_callback
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("ShellCommandEdit")

    def name(self) -> str:
        """Get the name of the command."""
        return "edit"

    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return ["open"]

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens a file for editing"

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
                ShellMessageSource.ERROR,
                "No filename specified. Usage: edit <filename>"
            )
            return False

        try:
            full_path = self._mindspace_manager.get_absolute_path(args[0])
            if not os.path.exists(full_path):
                # Create directory if needed
                directory = os.path.dirname(full_path)
                if directory and not os.path.exists(directory):
                    try:
                        os.makedirs(directory, exist_ok=True)

                    except OSError as e:
                        self._history_manager.add_message(
                            ShellMessageSource.ERROR,
                            f"Failed to create directory: {str(e)}"
                        )
                        return False

            if not self._edit_file(full_path):
                self._history_manager.add_message(
                    ShellMessageSource.ERROR,
                    f"Failed to edit file: {args[0]}"
                )
                return False

            self._history_manager.add_message(
                ShellMessageSource.SUCCESS,
                f"Editing file: {args[0]}"
            )
            return True

        except Exception as e:
            self._logger.exception("Error processing file: %s", str(e))
            self._history_manager.add_message(
                ShellMessageSource.ERROR,
                f"Error processing file: {str(e)}"
            )
            return False

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
        # For the edit command, we're primarily interested in completing file paths
        # Only handle options if we're explicitly looking at an option token
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # For arguments, complete file paths
        return self._get_mindspace_path_completions(current_token.value)
