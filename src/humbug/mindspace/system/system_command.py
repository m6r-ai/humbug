"""Base class for system commands."""

import logging
import os
import shlex
from typing import Dict, List, Optional, Tuple

from humbug.gui.command_options import CommandOptionParser, CommandOptionsRegistry, CommandOptionDescriptor
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import Token, TokenType


class SystemCommand:
    """Base class for all system commands."""

    def __init__(self) -> None:
        """Initialize base system command."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._mindspace_manager = MindspaceManager()

    @property
    def name(self) -> str:
        """
        Get the name of the command.

        This is the primary name used to invoke the command.
        """
        raise NotImplementedError("Subclasses must implement name property")

    @property
    def aliases(self) -> List[str]:
        """
        Get alternate names for the command.

        Returns:
            List of alternate names for the command
        """
        return []

    @property
    def help_text(self) -> str:
        """
        Get the help text for the command.

        This is a one-line description shown in help listings.
        """
        return ""

    def setup_options(self) -> CommandOptionsRegistry:
        """
        Set up command options.

        Returns:
            CommandOptionsRegistry with registered options
        """
        # Create options registry
        options = CommandOptionsRegistry()

        # Add help option by default
        options.add_option(CommandOptionDescriptor(
            short_name="h",
            long_name="help",
            help_text="Show detailed help for this command",
            takes_value=False
        ))

        return options

    def execute(self, args: str) -> bool:
        """
        Execute the command with the given arguments.

        Args:
            args: Command arguments as a string

        Returns:
            True if command executed successfully, False otherwise
        """
        try:
            # Create the option parser
            parser = CommandOptionParser(args)

            # Configure with our options
            options = self.setup_options()
            parser.setup_registry(options)

            # Parse the command line
            if not parser.parse():
                return False

            # Check for help flag
            if parser.help_requested():
                self._show_detailed_help()
                return True

            # Execute the command with parsed options
            return self._execute_command(parser, parser.remaining_args())

        except Exception as e:
            self._logger.error("Error executing command: %s", str(e), exc_info=True)
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Error executing command: {str(e)}"
            )
            return False

    def _execute_command(self, parser: CommandOptionParser, args: str) -> bool:
        """
        Execute the command with parsed options.

        Args:
            parser: The option parser with parsed options
            args: Remaining arguments after option parsing

        Returns:
            True if command executed successfully, False otherwise
        """
        raise NotImplementedError("Subclasses must implement _execute_command")

    def _show_detailed_help(self) -> None:
        """Show detailed help for this command."""
        # Start with basic info
        help_text = f"{self.name} - {self.help_text}\n\n"

        # Add aliases if any
        if self.aliases:
            help_text += f"Aliases: {', '.join(self.aliases)}\n\n"

        # Add options help
        options = self.setup_options()
        options_help = options.get_help_text()
        if options_help:
            help_text += "Options:\n"
            help_text += options_help

        # Display help
        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            help_text
        )

    def get_completions(self, partial_args: str) -> List[str]:
        """
        Get completions for partial arguments (legacy method).

        Args:
            partial_args: Partial command arguments

        Returns:
            List of possible completions
        """
        # By default, delegate to the token-based implementation when called with a string
        # This provides backward compatibility
        parser = CommandOptionParser(partial_args)
        options = self.setup_options()
        parser.setup_registry(options)

        # Get option completions
        if partial_args and partial_args.strip() and (partial_args.strip()[0] == '-'):
            partial_option = partial_args.strip().split()[-1]
            completions = options.get_option_completions(partial_option)
            if completions:
                return completions

        # Use the implementation in subclasses
        # The default implementation below simply returns empty completions
        return []

    def get_token_completions(
        self,
        current_token: Token,
        tokens: List[Token],
        cursor_token_index: int,
        full_text: str
    ) -> List[str]:
        """
        Get completions for the current token based on token information.

        Args:
            current_token: The token at cursor position
            tokens: All tokens in the command line
            cursor_token_index: Index of current_token in tokens list
            full_text: Full command line text

        Returns:
            List of possible completions
        """
        # Default implementation calls the string-based method for compatibility
        # Subclasses should override this for efficient token-based completion

        # Extract argument portion based on token types
        command_token = None
        for token in tokens:
            if token.type == TokenType.COMMAND:
                command_token = token
                break

        if command_token:
            command_end = command_token.start + len(command_token.value)
            args_text = full_text[command_end:].lstrip()
            return self.get_completions(args_text)

        return []

    def _tokenize_args(self, args: str) -> List[str]:
        """
        Split command line arguments into tokens, preserving quotes.

        Args:
            args: Command line arguments

        Returns:
            List of argument tokens
        """
        try:
            return shlex.split(args)
        except ValueError:
            # Handle unclosed quotes or other parsing errors
            return args.split()

    def _get_mindspace_path_completions(self, partial_path: str, file_extension: Optional[str] = None) -> List[str]:
        """
        Get path completions within the current mindspace.

        Args:
            partial_path: Partial path to complete
            file_extension: Optional file extension to filter by

        Returns:
            List of path completions
        """
        if not self._mindspace_manager.has_mindspace():
            return []

        # Handle empty path
        if not partial_path:
            return self._list_directory(".", file_extension)

        # Split into directory and filename parts
        dir_path, filename = os.path.split(partial_path)
        if not dir_path:
            dir_path = "."

        # Check if dir_path exists in mindspace
        mindspace_dir = self._mindspace_manager.get_mindspace_path(dir_path)
        if not os.path.exists(mindspace_dir) or not os.path.isdir(mindspace_dir):
            return []

        # Get completions
        completions = []
        try:
            for item in os.listdir(mindspace_dir):
                # Filter by extension if specified
                if file_extension and os.path.isfile(os.path.join(mindspace_dir, item)):
                    if not item.endswith(file_extension):
                        continue

                # Filter by partial filename
                if not filename or item.startswith(filename):
                    # Build full path relative to arguments
                    full_path = os.path.join(dir_path, item)

                    # Add trailing slash for directories
                    if os.path.isdir(os.path.join(mindspace_dir, item)):
                        full_path += os.path.sep

                    # Escape spaces in path
                    if " " in full_path:
                        parts = full_path.split(" ")
                        escaped_path = parts[0]
                        for part in parts[1:]:
                            escaped_path += "\\ " + part
                        full_path = escaped_path

                    completions.append(full_path)

            return sorted(completions)

        except (OSError, PermissionError) as e:
            self._logger.warning("Error completing path: %s", str(e))
            return []

    def _list_directory(self, dir_path: str, file_extension: Optional[str] = None) -> List[str]:
        """
        List contents of a directory in mindspace.

        Args:
            dir_path: Directory path relative to mindspace
            file_extension: Optional file extension to filter by

        Returns:
            List of items in directory with proper path formatting
        """
        if not self._mindspace_manager.has_mindspace():
            return []

        mindspace_dir = self._mindspace_manager.get_mindspace_path(dir_path)
        if not os.path.exists(mindspace_dir) or not os.path.isdir(mindspace_dir):
            return []

        try:
            items = []
            for item in os.listdir(mindspace_dir):
                # Filter by extension if specified
                if file_extension and os.path.isfile(os.path.join(mindspace_dir, item)):
                    if not item.endswith(file_extension):
                        continue

                path = os.path.join(dir_path, item)
                # Add trailing slash for directories
                if os.path.isdir(os.path.join(mindspace_dir, item)):
                    path += os.path.sep

                # Escape spaces in path
                if " " in path:
                    parts = path.split(" ")
                    escaped_path = parts[0]
                    for part in parts[1:]:
                        escaped_path += "\\ " + part
                    path = escaped_path

                items.append(path)

            return sorted(items)

        except (OSError, PermissionError) as e:
            self._logger.warning("Error listing directory: %s", str(e))
            return []
