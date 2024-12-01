"""Command processor for handling application commands."""

from typing import Callable, Dict, Optional


class CommandProcessor:
    """Processes application commands."""

    def __init__(self):
        """Initialize the command processor."""
        self.commands: Dict[str, Callable] = {}
        self._register_commands()

    def _register_commands(self):
        """Register all available commands."""
        self.commands = {
            "exit": self._cmd_exit,
        }

    async def process_command(self, command: str) -> Optional[str]:
        """Process a command and return response if any."""
        cmd_parts = command.strip().split(maxsplit=1)
        cmd_name = cmd_parts[0].lower()

        if cmd_name not in self.commands:
            return f"Unknown command: {cmd_name}"

        try:
            return await self.commands[cmd_name]()
        except Exception as e:
            return f"Error executing command {cmd_name}: {str(e)}"

    async def _cmd_exit(self) -> str:
        """Handle the exit command."""
        return "Exiting application..."
