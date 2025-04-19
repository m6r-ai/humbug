"""Command for opening or creating files in an editor tab from the system terminal."""

import logging
import os
from typing import List, Callable, Dict, Optional

from humbug.gui.tab.editor.editor_tab import EditorTab
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import Token, TokenType


class EditCommand(SystemCommand):
    """Command to open or create a file in an editor tab."""

    def __init__(self, open_file_callback: Callable[[str], EditorTab], new_file_callback: Callable[[], EditorTab]) -> None:
        """
        Initialize edit command.

        Args:
            open_file_callback: Callback to open an existing file
            new_file_callback: Callback to create a new file
        """
        super().__init__()
        self._open_file = open_file_callback
        self._new_file = new_file_callback
        self._mindspace_manager = MindspaceManager()
        self._logger = logging.getLogger("EditCommand")

    @property
    def name(self) -> str:
        """Get the name of the command."""
        return "edit"

    @property
    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return ["open"]

    @property
    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens a file for editing. Creates the file if it doesn't exist. Usage: edit <filename>"

    def _execute_command(self, tokens: List[Token], args: str) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer
            args: Remaining arguments as a string

        Returns:
            True if command executed successfully, False otherwise
        """
        filename = args.strip()
        if not filename:
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                "No filename specified. Usage: edit <filename>"
            )
            return False

        if not self._mindspace_manager.has_mindspace():
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                "Cannot edit file: no mindspace is open."
            )
            return False

        try:
            # Convert relative path to absolute path within mindspace
            if not os.path.isabs(filename):
                full_path = self._mindspace_manager.get_mindspace_path(filename)
            else:
                full_path = filename

            # Check if file exists
            if os.path.exists(full_path):
                # Open existing file
                editor = self._open_file(full_path)
                if editor:
                    self._mindspace_manager.add_system_interaction(
                        SystemMessageSource.SUCCESS,
                        f"Opened file: {filename}"
                    )
                    return True
            else:
                # Create directory if needed
                directory = os.path.dirname(full_path)
                if directory and not os.path.exists(directory):
                    try:
                        os.makedirs(directory, exist_ok=True)
                    except OSError as e:
                        self._mindspace_manager.add_system_interaction(
                            SystemMessageSource.ERROR,
                            f"Failed to create directory: {str(e)}"
                        )
                        return False

                # Create new file
                editor = self._new_file()
                if editor:
                    # Set filename and save to create the file
                    editor.set_filename(full_path)
                    if editor.save():
                        self._mindspace_manager.add_system_interaction(
                            SystemMessageSource.SUCCESS,
                            f"Created and opened file: {filename}"
                        )
                        return True

            # If we get here, something went wrong
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Failed to open or create file: {filename}"
            )
            return False

        except Exception as e:
            self._logger.exception("Error processing file: %s", str(e))
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Error processing file: {str(e)}"
            )
            return False

    def get_token_completions(
        self,
        current_token: Token,
        tokens: List[Token],
        cursor_token_index: int,
        full_text: str
    ) -> List[str]:
        """
        Get completions for the current token based on token information.

        Args:
            current_token: The token at cursor position
            tokens: All tokens in the command line
            cursor_token_index: Index of current_token in tokens list
            full_text: Full command line text

        Returns:
            List of possible completions
        """
        # For the edit command, we're primarily interested in completing file paths
        # Only handle options if we're explicitly looking at an option token
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # For arguments, complete file paths (with no extension filter)
        return self._get_mindspace_path_completions(current_token.value)
