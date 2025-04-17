from abc import ABC, abstractmethod
from typing import List

from humbug.gui.command_options import CommandOptionsRegistry, CommandOptionParser, CommandOptionDescriptor
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_message_source import SystemMessageSource


class SystemCommand(ABC):
    """Base class for system commands."""

    @property
    @abstractmethod
    def name(self) -> str:
        """Get the name of the command."""

    @property
    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return []

    @property
    def help_text(self) -> str:
        """Get the help text for the command."""
        return ""

    def setup_options(self) -> CommandOptionsRegistry:
        """
        Set up and return options supported by this command.

        Returns:
            Registry with command options
        """
        options = CommandOptionsRegistry()

        # Add common options that all commands support
        options.add_option(CommandOptionDescriptor(
            short_name="?",
            long_name="help",
            help_text="Show detailed help for this command"
        ))

        return options

    @abstractmethod
    def _execute_command(self, parser: CommandOptionParser, args: str) -> bool:
        """
        Execute the command with parsed options.

        Args:
            parser: The option parser with parsed options
            args: Remaining arguments after option parsing

        Returns:
            True if command executed successfully, False otherwise
        """

    def execute(self, args: str) -> bool:
        """
        Parse options and execute the command.
        
        Args:
            args: Command arguments as a string
            
        Returns:
            True if command executed successfully, False otherwise
        """
        parser = CommandOptionParser(args)

        # Check for help flag
        if parser.has_flag("help") or parser.has_flag("?"):
            self._show_detailed_help()
            return True

        # Execute with parser and remaining arguments
        return self._execute_command(parser, parser.remaining_args())

    def _show_detailed_help(self) -> None:
        """Display detailed help text for the command."""
        options = self.setup_options()
        detailed_help = f"Command: {self.name}"

        # Add aliases if any
        if self.aliases:
            detailed_help += f" (aliases: {', '.join(self.aliases)})"

        # Add basic help text
        detailed_help += f"\n\n{self.help_text}\n\n{options.get_help_text()}"

        # Display help using system message
        mindspace_manager = MindspaceManager()
        mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            detailed_help
        )

    def get_completions(self, partial_args: str) -> List[str]:
        """
        Get completions for partial arguments, including options.
        
        Args:
            partial_args: Partial command arguments
            
        Returns:
            List of possible completions
        """
        # If partial_args is empty, just return empty list for default behavior
        if not partial_args:
            return []

        options = self.setup_options()
        partial_args = partial_args.lstrip()

        # Check if we're completing an option
        if partial_args.startswith('-'):
            # Check if we're completing an option value
            # Split into tokens (preserving quoted strings)
            tokens = []
            current = ''
            in_quotes = False
            quote_char = None

            for char in partial_args:
                if char in ('"', "'"):
                    if not in_quotes:
                        in_quotes = True
                        quote_char = char

                    elif char == quote_char:
                        in_quotes = False
                        quote_char = None

                    current += char

                elif char.isspace() and not in_quotes:
                    if current:
                        tokens.append(current)
                        current = ''

                else:
                    current += char

            if current:
                tokens.append(current)

            print(f"Tokens: {tokens}")

            # Check if last token is a partial option value
            if len(tokens) >= 2 and tokens[-2].startswith('-'):
                option_token = tokens[-2]
                option_name = option_token[2:] if option_token.startswith('--') else option_token[1:]

                # Find the option descriptor
                option = options.find_option(option_name)
                if option and option.takes_value:
                    partial_value = tokens[-1]
                    print(f"Partial value: {partial_value}")
                    return options.get_value_completions(option_name, partial_value)

            # We're completing an option name
            return options.get_option_completions(partial_args)

        # Default behavior - delegate to subclass if nothing else matches
        return []
