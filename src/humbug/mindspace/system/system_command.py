"""Base class for system commands."""

import logging
import os
from typing import Dict, List

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

    def get_options_help(self) -> Dict[str, str]:
        """
        Get help text for all supported command options.

        Returns:
            Dictionary mapping option names (with dashes) to help text
        """
        # Default implementation includes help flag
        return {
            "-h, --help": "Show detailed help for this command"
        }

    def execute(self, tokens: List[Token], _full_text: str) -> bool:
        """
        Execute the command with the given tokens.

        Args:
            tokens: List of tokens from command lexer
            _full_text: Original full command text (deprecated)

        Returns:
            True if command executed successfully, False otherwise
        """
        try:
            # Check for help flag
            if self._has_flag(tokens, "h") or self._has_flag(tokens, "help"):
                self._show_detailed_help()
                return True

            # Execute the command with tokens only
            return self._execute_command(tokens)

        except Exception as e:
            self._logger.error("Error executing command: %s", str(e), exc_info=True)
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Error executing command: {str(e)}"
            )
            return False

    def _has_flag(self, tokens: List[Token], flag_name: str) -> bool:
        """
        Check if a flag is present in the tokens.

        Args:
            tokens: List of command tokens
            flag_name: Flag name without dashes

        Returns:
            True if flag is present, False otherwise
        """
        for token in tokens:
            if token.type == TokenType.OPTION:
                if token.value == f"-{flag_name}" or token.value == f"--{flag_name}":
                    return True
        return False

    def _get_options(self, tokens: List[Token]) -> Dict[str, str | None]:
        """
        Get dictionary of options and their values.

        Args:
            tokens: List of tokens

        Returns:
            Dictionary mapping option names to their values
        """
        options: Dict[str, str | None] = {}
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if token.type == TokenType.OPTION:
                # Normalize option name (remove leading dashes)
                option_name = token.value.lstrip('-')

                # Check if next token is an argument (option value)
                if i + 1 < len(tokens) and tokens[i + 1].type == TokenType.ARGUMENT:
                    options[option_name] = tokens[i + 1].value
                    i += 2  # Skip both option and value

                else:
                    # Flag option without value
                    options[option_name] = None
                    i += 1

            else:
                i += 1

        return options

    def _get_positional_arguments(self, tokens: List[Token]) -> List[str]:
        """
        Get list of positional arguments (not associated with options).

        Args:
            tokens: List of tokens

        Returns:
            List of positional argument values in order
        """
        args = []
        i = 0
        while i < len(tokens):
            token = tokens[i]
            if token.type == TokenType.COMMAND:
                i += 1

            elif token.type == TokenType.OPTION:
                # Skip option and its potential value
                if i + 1 < len(tokens) and tokens[i + 1].type == TokenType.ARGUMENT:
                    i += 2

                else:
                    i += 1

            elif token.type == TokenType.ARGUMENT:
                # Make sure this argument isn't a value for a preceding option
                if i > 0 and tokens[i-1].type == TokenType.OPTION:
                    i += 1

                else:
                    args.append(token.value)
                    i += 1

            else:
                i += 1

        return args

    def _get_command_token(self, tokens: List[Token]) -> Token | None:
        """
        Find the command token in the token list.

        Args:
            tokens: List of tokens to search

        Returns:
            Command token if found, None otherwise
        """
        for token in tokens:
            if token.type == TokenType.COMMAND:
                return token
        return None

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

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
        options_help = self.get_options_help()
        if options_help:
            help_text += "Options:\n"
            for option, description in options_help.items():
                help_text += f"  {option}\n    {description}\n"

        # Display help
        self._mindspace_manager.add_system_interaction(
            SystemMessageSource.SUCCESS,
            help_text
        )

    def get_token_completions(
        self,
        current_token: Token,
        _tokens: List[Token],
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
        # Base implementation handles option completions
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        return []

    def _get_option_completions(self, partial_option: str) -> List[str]:
        """
        Get completions for a partial option.

        Args:
            partial_option: Partial option text (with dashes)

        Returns:
            List of matching option completions
        """
        options = []

        # Get all options from help text
        for option_text in self.get_options_help():
            # Split combined options like "-h, --help"
            for option in option_text.split(','):
                option = option.strip()
                if option.startswith(partial_option):
                    options.append(option)

        return options

    def _get_mindspace_path_completions(self, partial_path: str, file_extension: str | None = None) -> List[str]:
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
            print("No path provided, listing current directory")
            return self._list_directory(".", file_extension)

        # Split into directory and filename parts
        dir_path, filename = os.path.split(partial_path)

        # Check if dir_path exists in mindspace.  Note if the dir_path is empty we pass an
        # empty string because it will be appended to the mindspace path
        mindspace_dir = self._mindspace_manager.get_mindspace_path(dir_path or "")
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

    def _list_directory(self, dir_path: str, file_extension: str | None = None) -> List[str]:
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
