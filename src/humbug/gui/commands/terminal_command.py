from typing import Callable

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

    def execute(self, args: str) -> bool:
        self._create_terminal()
        return True
