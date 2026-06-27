"""Command for opening a side-by-side git diff tab from the system shell."""

import os

from mindspace.mindspace_log_level import MindspaceLogLevel
from syntax import Token, TokenType

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_event_source import ShellEventSource


class ShellCommandDiff(ShellCommand):
    """Command to open a side-by-side git diff tab."""

    def name(self) -> str:
        """Get the name of the command."""
        return "diff"

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens a side-by-side git diff tab for a file"

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
                "No filename specified. Usage: diff <filename>"
            )
            return False

        full_path = self._mindspace.get_absolute_path(args[0])
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

        contexts = self._mindspace.contexts()
        existing = contexts.get_by_path_and_type(full_path, "diff")
        if existing:
            contexts.focus(existing.context_id)
            context_id = existing.context_id

        else:
            context_id = contexts.open(
                context_type="diff",
                path=full_path,
                title=os.path.basename(full_path),
                requester_id=self._requester_id,
            )

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Shell opened diff for: '{full_path}'\ntab ID: {context_id}"
        )
        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            f"Opening diff: {args[0]}"
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
