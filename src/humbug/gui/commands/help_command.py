from typing import List

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_command_registry import SystemCommandRegistry
from humbug.mindspace.system.system_message_source import SystemMessageSource


class HelpCommand(SystemCommand):
    """Command to display help information."""

    def __init__(self, registry: SystemCommandRegistry) -> None:
        """
        Initialize the command.

        Args:
            registry: The command registry
        """
        self._mindspace_manager = MindspaceManager()
        self._registry = registry

    @property
    def name(self) -> str:
        return "help"

    @property
    def aliases(self) -> List[str]:
        return ["?"]

    @property
    def help_text(self) -> str:
        return "Show this help message"

    def execute(self, args: str) -> bool:
        # Build help text using command registry
        commands = self._registry.get_all_commands()

        help_text = "Available commands:\n"
        for name, cmd in sorted(commands.items()):
            if cmd.help_text:
                help_text += f"  {name} - {cmd.help_text}\n"
            else:
                help_text += f"  {name}\n"

        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            help_text
        )

        # You would need to update this to use the MindspaceManager instead
        # of directly outputting, or pass a callback for displaying output
        return True
