
from PySide6.QtCore import QObject

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_history_manager import ShellHistoryManager
from desktop.mindspace.mindspace_manager import MindspaceManager

class ShellCommandRegistry(QObject):
    """Registry for shell commands."""

    def __init__(self, mindspace_manager: MindspaceManager) -> None:
        """Initialize QObject base class if not already done."""
        super().__init__()
        self._commands: dict[str, ShellCommand] = {}
        self._aliases: dict[str, str] = {}
        self._history_manager = ShellHistoryManager(mindspace_manager)

    def register_command(self, command: ShellCommand) -> None:
        """
        Register a command with the registry.

        Args:
            command: The command to register
        """
        command_name = command.name()
        self._commands[command_name] = command

        # Register aliases
        for alias in command.aliases():
            self._aliases[alias] = command_name

    def get_command(self, name: str) -> ShellCommand | None:
        """
        Get a command by name or alias.

        Args:
            name: The command name or alias

        Returns:
            The command if found, None otherwise
        """
        # Check if it's an alias
        if name in self._aliases:
            name = self._aliases[name]

        return self._commands.get(name)

    def get_all_commands(self) -> dict[str, ShellCommand]:
        """
        Get all registered commands.

        Returns:
            Dictionary of command names to commands
        """
        return self._commands.copy()

    def get_command_names(self) -> list[str]:
        """
        Get all command names and aliases.

        Returns:
            List of command names and aliases
        """
        return list(self._commands.keys()) + list(self._aliases.keys())
