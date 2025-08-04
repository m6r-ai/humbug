"""Command for creating a new terminal tab from the shell."""

from typing import List

from syntax import Token, TokenType

from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_message_source import ShellMessageSource


class ShellCommandTerminal(ShellCommand):
    """Command to create a new terminal tab."""

    def __init__(self, column_manager: ColumnManager) -> None:
        """
        Initialize the command.

        Args:
            create_terminal_callback: Function to call to create a new terminal
        """
        super().__init__()
        self._column_manager = column_manager

    def name(self) -> str:
        return "terminal"

    def aliases(self) -> List[str]:
        return ["term"]

    def help_text(self) -> str:
        return "Starts a new terminal"

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
            self._column_manager.new_terminal()

        finally:
            self._column_manager.protect_current_tab(False)

        self._history_manager.add_message(
            ShellMessageSource.SUCCESS,
            "Started new terminal"
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
        # If the current token is an option, get option completions
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # Terminal command doesn't take any arguments, so no completions for arguments
        return []
