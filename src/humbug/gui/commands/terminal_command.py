"""Command for creating a new terminal tab from the system interface."""

from typing import Callable, List

from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import Token, TokenType


class TerminalCommand(SystemCommand):
    """Command to create a new terminal tab."""

    def __init__(self, create_terminal_callback: Callable[[], None]) -> None:
        """
        Initialize the command.

        Args:
            create_terminal_callback: Function to call to create a new terminal
        """
        super().__init__()
        self._create_terminal = create_terminal_callback

    @property
    def name(self) -> str:
        return "terminal"

    @property
    def aliases(self) -> List[str]:
        return ["term"]

    @property
    def help_text(self) -> str:
        return "Open a new terminal tab"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        self._create_terminal()
        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            "New terminal tab created"
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
