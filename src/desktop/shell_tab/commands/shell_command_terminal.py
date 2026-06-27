"""Command for creating a new terminal tab from the shell."""


from syntax import Token, TokenType

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_event_source import ShellEventSource


class ShellCommandTerminal(ShellCommand):
    """Command to create a new terminal tab."""

    def name(self) -> str:
        return "terminal"

    def aliases(self) -> list[str]:
        return ["term"]

    def help_text(self) -> str:
        return "Starts a new terminal"

    def _execute_command(self, tokens: list[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        self._mindspace.contexts().open(
            context_type="terminal",
            title="Terminal",
            requester_id=self._requester_id,
        )

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            "Started new terminal"
        )
        return True

    def get_token_completions(
        self,
        current_token: Token,
        _tokens: list[Token],
        _cursor_token_index: int
    ) -> list[str]:
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

        return []
