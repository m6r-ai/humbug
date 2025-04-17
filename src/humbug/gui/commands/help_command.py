from typing import List

from humbug.gui.command_options import CommandOptionParser
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
        return "Show help for available commands"

    def _execute_command(self, parser: CommandOptionParser, args: str) -> bool:
        """
        Execute the command with parsed options.

        Args:
            parser: The option parser with parsed options
            args: Remaining arguments after option parsing

        Returns:
            True if command executed successfully, False otherwise
        """
        # If a specific command is given, show help for that command
        command_name = args.strip()
        if command_name:
            command = self._registry.get_command(command_name)
            if command:
                # Use the command's detailed help method
                command._show_detailed_help()

            else:
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"Unknown command: {command_name}"
                )
            return True

        # Otherwise show general help for all commands
        commands = self._registry.get_all_commands()

        help_text = "Available commands:\n"
        for name, cmd in sorted(commands.items()):
            if cmd.help_text:
                help_text += f"  {name} - {cmd.help_text}\n"

            else:
                help_text += f"  {name}\n"

        help_text += "\nType 'help <command>' or '<command> --help' for detailed help on a specific command."

        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            help_text
        )

        return True
