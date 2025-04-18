from abc import ABC, abstractmethod
import logging
import os
from typing import List, Optional

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

        # Check for help flag - check both short and long forms
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

        # Split into tokens (preserving quoted strings)
        tokens = self._tokenize_args(partial_args)

        # If we have at least two tokens and the second-to-last token is an option
        if len(tokens) >= 2 and tokens[-2].startswith('-') and not tokens[-1].startswith('-'):
            option_token = tokens[-2]

            # Extract option name without dashes
            option_name = ""
            if option_token.startswith('--'):
                option_name = option_token[2:]

            elif option_token.startswith('-'):
                option_name = option_token[1:]

            # Find the option descriptor
            option = options.find_option(option_name)
            if option and option.takes_value:
                partial_value = tokens[-1]
                return options.get_value_completions(option_name, partial_value)

        # Check if we're completing an option name
        if partial_args.startswith('-'):
            return options.get_option_completions(partial_args)

        # Default behavior - delegate to subclass if nothing else matches
        return []

    def _tokenize_args(self, args_string: str) -> List[str]:
        """
        Tokenize arguments string, preserving quoted strings.

        Args:
            args_string: Arguments string to tokenize

        Returns:
            List of tokens
        """
        tokens = []
        current = ''
        in_quotes = False
        quote_char = None

        for char in args_string:
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

        return tokens

    def _get_path_completions(
        self,
        base_dir: str,
        partial_path: str,
        file_extension: Optional[str] = None
    ) -> List[str]:
        """
        Get file path completions for a partial path.

        Args:
            base_dir: Base directory to search from
            partial_path: Partial path to complete
            file_extension: Optional file extension to filter by (e.g., '.m6r')

        Returns:
            List of matching path completions
        """
        try:
            # Handle absolute paths
            if os.path.isabs(partial_path):
                base_dir = os.path.dirname(partial_path) or os.path.dirname(os.path.abspath(partial_path))
                partial_file = os.path.basename(partial_path)

            else:
                # Handle relative paths with directories
                if os.path.dirname(partial_path):
                    base_dir = os.path.join(base_dir, os.path.dirname(partial_path))
                    partial_file = os.path.basename(partial_path)

                else:
                    partial_file = partial_path

            # Ensure the base directory exists
            if not os.path.exists(base_dir):
                return []

            # List files and directories that match the partial path
            matches = []
            for item in os.listdir(base_dir):
                # Skip hidden files
                if item.startswith('.'):
                    continue

                full_item_path = os.path.join(base_dir, item)

                # If partial file is empty or item starts with it
                if not partial_file or item.startswith(partial_file):
                    if os.path.isdir(full_item_path):
                        # Add directory indicator
                        rel_path = os.path.join(os.path.dirname(partial_path), item)
                        matches.append(f"{rel_path}/")

                    elif file_extension is None or item.endswith(file_extension):
                        # Add file if it matches the extension filter (or no filter)
                        matches.append(os.path.join(os.path.dirname(partial_path), item))

            return matches

        except Exception as e:
            logging.getLogger(self.__class__.__name__).error("Error generating path completions: %s", str(e))
            return []

    def _get_mindspace_path_completions(
        self,
        partial_path: str,
        file_extension: Optional[str] = None
    ) -> List[str]:
        """
        Get file path completions relative to the current mindspace.

        Args:
            partial_path: Partial path to complete
            file_extension: Optional file extension to filter by

        Returns:
            List of matching path completions
        """
        mindspace_manager = MindspaceManager()
        if not mindspace_manager.has_mindspace():
            return []

        base_dir = mindspace_manager.mindspace_path()
        return self._get_path_completions(base_dir, partial_path, file_extension)
