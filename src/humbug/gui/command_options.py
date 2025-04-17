"""Command-line style option parsing for system commands."""

from dataclasses import dataclass
from typing import Callable, Dict, List, Set


@dataclass
class CommandOptionDescriptor:
    """Descriptor for a command option."""
    short_name: str | None  # Short option name (e.g., 'h' for -h)
    long_name: str | None   # Long option name (e.g., 'help' for --help)
    help_text: str          # Help text for this option
    takes_value: bool = False  # Whether this option takes a value
    value_description: str | None = None  # Description of the value (for help text)

    def get_names(self) -> List[str]:
        """
        Get all names for this option (for completion).
        
        Returns:
            List of option names with appropriate prefixes
        """
        names = []
        if self.short_name:
            names.append(f"-{self.short_name}")

        if self.long_name:
            names.append(f"--{self.long_name}")

        return names

    def matches(self, option_name: str) -> bool:
        """
        Check if this option matches a given name.
        
        Args:
            option_name: Option name to check (without - or -- prefix)
            
        Returns:
            True if the option matches
        """
        return option_name == self.short_name or option_name == self.long_name


class CommandOptionsRegistry:
    """Registry of options supported by a command."""

    def __init__(self) -> None:
        """Initialize with empty options."""
        self._options: List[CommandOptionDescriptor] = []
        self._value_completers: Dict[str, Callable[[str], List[str]]] = {}

    def add_option(self, option: CommandOptionDescriptor) -> None:
        """
        Add an option to the registry.
        
        Args:
            option: The option descriptor to add
        """
        self._options.append(option)

    def add_value_completer(self, option_name: str, completer: Callable[[str], List[str]]) -> None:
        """
        Add a completer function for an option's values.
        
        Args:
            option_name: The option name (long or short)
            completer: Function that takes a partial value and returns completions
        """
        self._value_completers[option_name] = completer

    def get_option_completions(self, partial_arg: str) -> List[str]:
        """
        Get completions for option names.
        
        Args:
            partial_arg: Partial option name to complete
            
        Returns:
            List of matching option names
        """
        completions = []
        for option in self._options:
            for name in option.get_names():
                if name.startswith(partial_arg):
                    completions.append(name)

        return completions

    def get_value_completions(self, option_name: str, partial_value: str) -> List[str]:
        """
        Get completions for option values.
        
        Args:
            option_name: The option name (without - or -- prefix)
            partial_value: Partial value to complete
            
        Returns:
            List of matching value completions
        """
        print(f"Getting value completions for option: {option_name} with partial value: {partial_value}")
        print(f"{self._value_completers}")
        if option_name in self._value_completers:
            return self._value_completers[option_name](partial_value)

        return []

    def find_option(self, option_name: str) -> CommandOptionDescriptor | None:
        """
        Find option descriptor by name.
        
        Args:
            option_name: The option name (without - or -- prefix)
            
        Returns:
            The option descriptor if found, None otherwise
        """
        for option in self._options:
            if option.matches(option_name):
                return option

        return None

    def get_help_text(self) -> str:
        """
        Generate help text for all options.
        
        Returns:
            Formatted help text for all registered options
        """
        if not self._options:
            return "No options available."

        help_text = "Options:\n"
        for option in self._options:
            # Format option names
            option_names = []
            if option.short_name:
                option_names.append(f"-{option.short_name}")

            if option.long_name:
                option_names.append(f"--{option.long_name}")

            option_str = ", ".join(option_names)

            # Add value description if applicable
            if option.takes_value and option.value_description:
                option_str += f" {option.value_description}"

            # Format and add the help text
            help_text += f"  {option_str:<20} {option.help_text}\n"

        return help_text


class CommandOptionParser:
    """Parser for command-line style options in system commands."""

    def __init__(self, args: str):
        """
        Initialize with command arguments.
        
        Args:
            args: Full command arguments string
        """
        self._args = args
        self._options: Dict[str, str] = {}  # Maps option names to values
        self._flags: Set[str] = set()  # Set of flags (options without values)
        self._remaining_args = ""
        self._parse()

    def _parse(self) -> None:
        """Parse arguments into options and remaining arguments."""
        if not self._args:
            return

        # Split args into tokens, preserving quoted strings
        tokens = []
        i = 0
        current_token = ""
        in_quotes = False
        quote_char = None

        while i < len(self._args):
            char = self._args[i]

            # Handle quotes
            if char in ('"', "'"):
                if not in_quotes:
                    in_quotes = True
                    quote_char = char

                elif char == quote_char:
                    in_quotes = False
                    quote_char = None

                else:
                    current_token += char

                i += 1
                continue

            # If in quotes, add character to current token
            if in_quotes:
                current_token += char
                i += 1
                continue

            # Handle whitespace outside quotes
            if char.isspace():
                if current_token:
                    tokens.append(current_token)
                    current_token = ""

                i += 1
                continue

            # Add character to current token
            current_token += char
            i += 1

        # Add final token if any
        if current_token:
            tokens.append(current_token)

        # Process tokens
        i = 0
        non_option_tokens = []

        while i < len(tokens):
            token = tokens[i]

            # Check if token is an option
            if token.startswith('--'):
                # Long option
                option_name = token[2:]

                # Check for --option=value format
                if '=' in option_name:
                    name, value = option_name.split('=', 1)
                    self._options[name] = value

                elif i + 1 < len(tokens) and not tokens[i + 1].startswith('-'):
                    # Option with separate value
                    self._options[option_name] = tokens[i + 1]
                    i += 1

                else:
                    # Flag option
                    self._flags.add(option_name)

            elif token.startswith('-') and len(token) > 1:
                # Short option(s)
                # Handle combined short options like -abc
                option_chars = token[1:]

                # If last option might take a value
                if len(option_chars) == 1 and i + 1 < len(tokens) and not tokens[i + 1].startswith('-'):
                    # Assume it takes a value
                    self._options[option_chars] = tokens[i + 1]
                    i += 1

                else:
                    # Treat each character as a flag
                    for char in option_chars:
                        self._flags.add(char)

            else:
                # Not an option, add to non-option tokens
                non_option_tokens.append(token)

            i += 1

        # Rebuild remaining args from non-option tokens
        self._remaining_args = ' '.join(non_option_tokens)

    def get_option(self, name: str) -> str | None:
        """
        Get value for a specific option.
        
        Args:
            name: Option name (without - or -- prefix)
            
        Returns:
            Option value if present, None otherwise
        """
        return self._options.get(name)

    def has_flag(self, name: str) -> bool:
        """
        Check if a flag option is present.
        
        Args:
            name: Flag name (without - or -- prefix)
            
        Returns:
            True if the flag is present
        """
        return name in self._flags

    def remaining_args(self) -> str:
        """
        Get remaining arguments after options are parsed.
        
        Returns:
            Remaining arguments as a string
        """
        return self._remaining_args
