"""Command for opening pages in a preview tab from the system shell."""

import os

from syntax import Token, TokenType

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_event_source import ShellEventSource


class ShellCommandPreview(ShellCommand):
    """Command to open a preview tab."""

    def name(self) -> str:
        """Get the name of the command."""
        return "preview"

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens a preview tab"

    def _execute_command(self, tokens: list[Token]) -> bool:
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
                "No filename specified. Usage: preview <filename>"
            )
            return False

        full_path = self._mindspace.get_absolute_path(args[0])
        if not os.path.exists(full_path):
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

        contexts = self._mindspace.contexts()
        existing = contexts.get_by_path_and_type(full_path, "preview")
        if existing:
            contexts.focus(existing.context_id)

        else:
            contexts.open(
                context_type="preview",
                path=full_path,
                title=os.path.basename(full_path),
                requester_id=self._requester_id,
            )

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            f"Opening page: {args[0]}"
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

        return self._get_mindspace_path_completions(current_token.value)
