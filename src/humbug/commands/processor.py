"""Command processor for handling application commands."""

import logging
from typing import Callable, Dict, Optional

from humbug.commands.exceptions import CommandNotFoundError, CommandExecutionError


class CommandProcessor:
    """Processes application commands."""

    def __init__(self):
        """Initialize the command processor."""
        self.commands: Dict[str, Callable] = {}
        self._register_commands()
        self._logger = logging.getLogger("CommandProcessor")

    def _register_commands(self):
        """Register all available commands."""
        self.commands = {
            "exit": self._cmd_exit,
        }

    async def process_command(self, command: str) -> Optional[str]:
        """Process a command and return response if any.

        Args:
            command: The command string to process

        Returns:
            Optional response message from the command

        Raises:
            CommandNotFoundError: If the command is not recognized
            CommandExecutionError: If the command fails during execution
        """
        cmd_parts = command.strip().split(maxsplit=1)
        cmd_name = cmd_parts[0].lower()

        if cmd_name not in self.commands:
            self._logger.warning("Unknown command attempted: %s", cmd_name)
            raise CommandNotFoundError(f"Unknown command: {cmd_name}")

        self._logger.debug("Executing command: %s", cmd_name)
        try:
            return await self.commands[cmd_name]()
        except Exception as e:
            self._logger.error("Command execution failed", exc_info=True)
            raise CommandExecutionError(f"Failed to execute command '{cmd_name}'") from e

    async def _cmd_exit(self) -> str:
        """Handle the exit command."""
        self._logger.info("Exit command received")
        return "Exiting application..."
