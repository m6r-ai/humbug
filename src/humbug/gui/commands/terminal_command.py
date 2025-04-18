from typing import Callable, List

from humbug.gui.command_options import CommandOptionParser
from humbug.mindspace.system.system_command import SystemCommand


class TerminalCommand(SystemCommand):
    """Command to create a new terminal tab."""

    def __init__(self, create_terminal_callback: Callable[[], None]) -> None:
        """
        Initialize the command.

        Args:
            create_terminal_callback: Function to call to create a new terminal
        """
        self._create_terminal = create_terminal_callback

    @property
    def name(self) -> str:
        return "terminal"

    @property
    def help_text(self) -> str:
        return "Open a new terminal tab"

    def _execute_command(self, parser: CommandOptionParser, args: str) -> bool:
        """
        Execute the command with parsed options.

        Args:
            parser: The option parser with parsed options
            args: Remaining arguments after option parsing

        Returns:
            True if command executed successfully, False otherwise
        """
        self._create_terminal()
        return True

    def get_completions(self, partial_args: str) -> List[str]:
        """
        Get completions for partial arguments.

        Args:
            partial_args: Partial command arguments

        Returns:
            List of possible completions
        """
        # Terminal command doesn't take any arguments, so no completions
        return []
