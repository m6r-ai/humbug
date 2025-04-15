from typing import Callable, List

from humbug.mindspace.system.system_command import SystemCommand


class M6rcCommand(SystemCommand):
    """Command to create a new conversation with a Metaphor file."""

    def __init__(self, create_metaphor_conversation_callback: Callable[[str], None]) -> None:
        """
        Initialize the command.

        Args:
            main_window: The main window instance
        """
        self._create_metaphor_conversation = create_metaphor_conversation_callback

    @property
    def name(self) -> str:
        return "m6rc"

    @property
    def help_text(self) -> str:
        return "Create a new conversation from a Metaphor file"

    def execute(self, args: str) -> bool:
        if not args.strip():
            # You could handle error reporting through the command itself
            # or let the processor handle it
            return False

        self._create_metaphor_conversation(args.strip())
        return True

    def get_completions(self, _partial_args: str) -> List[str]:
        # Could implement file path completion here
        return []
