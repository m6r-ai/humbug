"""Command for opening the mindspace log."""


from syntax import Token

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_event_source import ShellEventSource


class ShellCommandLog(ShellCommand):
    """Command to open the mindspace log."""

    def name(self) -> str:
        """Get the name of the command."""
        return "log"

    def aliases(self) -> list[str]:
        """Get alternate names for the command."""
        return []

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Opens the mindspace log"

    def _execute_command(self, tokens: list[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        contexts = self._mindspace.contexts()
        existing = next((i for i in contexts.list_all() if i.context_type == "log"), None)
        if existing:
            contexts.focus(existing.context_id)

        else:
            contexts.open(
                context_type="log",
                title="Mindspace Log",
                requester_id=self._requester_id,
            )

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            "Opened mindspace log"
        )

        return True
