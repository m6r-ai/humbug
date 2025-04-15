from typing import Dict, List, Optional

from PySide6.QtCore import QObject

from humbug.mindspace.system.system_command import SystemCommand


class SystemCommandRegistry(QObject):
    """Registry for system commands."""

    _instance = None

    def __new__(cls) -> 'SystemCommandRegistry':
        """Singleton pattern implementation."""
        if cls._instance is None:
            cls._instance = super(SystemCommandRegistry, cls).__new__(cls)

        return cls._instance

    def __init__(self) -> None:
        """Initialize QObject base class if not already done."""
        if not hasattr(self, '_initialized'):
            super().__init__()
            self._commands: Dict[str, SystemCommand] = {}
            self._aliases: Dict[str, str] = {}
            self._initialized = True


    def register_command(self, command: SystemCommand) -> None:
        """
        Register a command with the registry.

        Args:
            command: The command to register
        """
        self._commands[command.name] = command

        # Register aliases
        for alias in command.aliases:
            self._aliases[alias] = command.name

    def get_command(self, name: str) -> Optional[SystemCommand]:
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

    def get_all_commands(self) -> Dict[str, SystemCommand]:
        """
        Get all registered commands.

        Returns:
            Dictionary of command names to commands
        """
        return self._commands.copy()

    def get_command_names(self) -> List[str]:
        """
        Get all command names and aliases.

        Returns:
            List of command names and aliases
        """
        return list(self._commands.keys()) + list(self._aliases.keys())
