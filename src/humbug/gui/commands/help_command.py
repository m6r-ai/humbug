"""Command for displaying help information in the system terminal."""

from typing import List

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_command_registry import SystemCommandRegistry
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import Token, TokenType


class HelpCommand(SystemCommand):
    """Command to display help information."""

    def __init__(self, registry: SystemCommandRegistry) -> None:
        """
        Initialize the command.

        Args:
            registry: The command registry
        """
        super().__init__()
        self._mindspace_manager = MindspaceManager()
        self._registry = registry

    def name(self) -> str:
        return "help"

    def aliases(self) -> List[str]:
        return ["?"]

    def help_text(self) -> str:
        return "Show help for available commands"

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        # Get positional arguments
        args = self._get_positional_arguments(tokens)

        # If a specific command is given, show help for that command
        if args:
            command_name = args[0]
            command = self._registry.get_command(command_name)
            if command:
                # Use the command's detailed help method
                command.show_detailed_help()
                return True

            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Unknown command: {command_name}"
            )
            return False

        # Otherwise show general help for all commands
        commands = self._registry.get_all_commands()

        help_text = "Available commands:\n"
        for name, cmd in sorted(commands.items()):
            this_help_text = cmd.help_text()
            if this_help_text:
                help_text += f"  {name} - {this_help_text}\n"
            else:
                help_text += f"  {name}\n"

        help_text += "\nType 'help <command>' or '<command> --help' for detailed help on a specific command."

        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            help_text
        )

        return True

    def get_token_completions(
        self,
        current_token: Token,
        tokens: List[Token],
        _cursor_token_index: int
    ) -> List[str]:
        """
        Get completions for the current token based on token information.

        Args:
            current_token: The token at cursor position
            tokens: All tokens in the command line
            cursor_token_index: Index of current_token in tokens list

        Returns:
            List of possible completions
        """
        # If the current token is an option, get option completions
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # For the help command, we complete with command names if this is an argument token
        if current_token.type == TokenType.ARGUMENT:
            partial_arg = current_token.value.strip()
            return [cmd for cmd in self._registry.get_command_names() if cmd.startswith(partial_arg)]

        # Return command names if completing a new argument
        if current_token.type == TokenType.COMMAND and len(tokens) == 1:
            # No arguments yet, return all command names
            return sorted(self._registry.get_command_names())

        return []
