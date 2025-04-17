"""
Command options implementation for the Humbug application.

This module provides classes for defining, parsing, and completing command options.
"""

from dataclasses import dataclass
from typing import Callable, Dict, List, Optional, Set


@dataclass
class CommandOptionDescriptor:
    """Descriptor for a command option."""

    short_name: str
    long_name: str
    help_text: str
    takes_value: bool = False
    value_description: str = ""

    def __post_init__(self) -> None:
        """Validate option descriptor."""
        # Ensure short_name is a single character
        if len(self.short_name) != 1:
            raise ValueError(f"Short option name must be a single character: {self.short_name}")

        # Ensure long_name is at least two characters
        if len(self.long_name) < 2:
            raise ValueError(f"Long option name must be at least two characters: {self.long_name}")


class CommandOptionsRegistry:
    """Registry of command options."""

    def __init__(self) -> None:
        """Initialize the registry."""
        self._options: Dict[str, CommandOptionDescriptor] = {}
        self._value_completers: Dict[str, Callable[[str], List[str]]] = {}

    def add_option(self, option: CommandOptionDescriptor) -> None:
        """
        Add an option to the registry.

        Args:
            option: Option descriptor to add
        """
        # Store the option by both short and long name
        self._options[option.short_name] = option
        self._options[option.long_name] = option

    def add_value_completer(self, option_name: str, completer: Callable[[str], List[str]]) -> None:
        """
        Add a value completer function for an option.

        Args:
            option_name: Option name (either short or long)
            completer: Function that takes a partial value and returns completions
        """
        # Store completer for both short and long name if possible
        if option_name in self._options:
            option = self._options[option_name]
            self._value_completers[option.short_name] = completer
            self._value_completers[option.long_name] = completer

        else:
            # If option isn't registered yet, just store for the provided name
            self._value_completers[option_name] = completer

    def find_option(self, name: str) -> Optional[CommandOptionDescriptor]:
        """
        Find an option by name.

        Args:
            name: Option name (either short or long)

        Returns:
            Option descriptor if found, None otherwise
        """
        return self._options.get(name)

    def get_value_completions(self, option_name: str, partial_value: str) -> List[str]:
        """
        Get completions for an option value.

        Args:
            option_name: Option name (either short or long)
            partial_value: Partial value to complete

        Returns:
            List of completions
        """
        completer = self._value_completers.get(option_name)
        if completer:
            return completer(partial_value)

        return []


    def get_option_completions(self, partial_option: str) -> List[str]:
        """
        Get completions for a partial option.

        Args:
            partial_option: Partial option to complete

        Returns:
            List of completions
        """
        completions = []

        # Determine if we're working with short or long option format
        is_long_format = partial_option.startswith('--')
        is_short_format = partial_option.startswith('-') and not is_long_format

        if not is_long_format and not is_short_format:
            return []

        if is_long_format:
            # Extract the option name without leading --
            partial_name = partial_option[2:]

            # Find all long options that start with this partial name
            for opt_name, descriptor in self._options.items():
                if len(opt_name) > 1 and opt_name.startswith(partial_name):
                    # Only check long names (more than 1 character)
                    completions.append(f"--{opt_name}")

        elif is_short_format:
            # Extract the option name without leading -
            partial_name = partial_option[1:]

            # For single dash with no additional text, return both short and long options
            if not partial_name:
                # Return all short options
                for opt_name, descriptor in self._options.items():
                    if len(opt_name) == 1:
                        completions.append(f"-{opt_name}")

                # Also return all long options for better tab completion
                for opt_name, descriptor in self._options.items():
                    if len(opt_name) > 1:
                        completions.append(f"--{opt_name}")

            else:
                # Check if any short option matches exactly
                for opt_name, descriptor in self._options.items():
                    if len(opt_name) == 1 and opt_name == partial_name:
                        completions.append(f"-{opt_name}")
                        break

        return sorted(list(set(completions)))

    def get_help_text(self) -> str:
        """
        Get help text for all options.

        Returns:
            Help text string
        """
        help_text = "Options:\n"

        # Collect unique options (short and long forms point to same descriptor)
        unique_options: Set[CommandOptionDescriptor] = set(self._options.values())

        for option in sorted(unique_options, key=lambda o: o.long_name):
            # Format as -s, --long
            option_format = f"  -{option.short_name}, --{option.long_name}"

            # Add value placeholder if option takes a value
            if option.takes_value:
                value_desc = option.value_description or "VALUE"
                option_format += f" <{value_desc}>"

            # Add help text
            help_text += f"{option_format}\n    {option.help_text}\n"

        return help_text


class CommandOptionParser:
    """Parser for command options."""

    def __init__(self, args_string: str) -> None:
        """
        Initialize the parser.

        Args:
            args_string: Command arguments string
        """
        self._args = args_string
        self._parsed_options: Dict[str, Optional[str]] = {}
        self._remaining_args = ""

        # Parse the arguments
        self._parse_args()

    def _parse_args(self) -> None:
        """Parse arguments into options and remaining arguments."""
        if not self._args:
            return

        tokens = []
        current = ''
        in_quotes = False
        quote_char = None

        # Tokenize the arguments string, preserving quoted strings
        for char in self._args:
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

        # Process tokens to extract options and arguments
        i = 0
        while i < len(tokens):
            token = tokens[i]

            # Check for long option format (--option)
            if token.startswith('--'):
                option_name = token[2:]

                # Check for --option=value format
                if '=' in option_name:
                    name, value = option_name.split('=', 1)
                    self._parsed_options[name] = value

                else:
                    # Check if next token is a value (not an option)
                    if i + 1 < len(tokens) and not tokens[i + 1].startswith('-'):
                        self._parsed_options[option_name] = tokens[i + 1]
                        i += 1  # Skip the value token

                    else:
                        # Flag without value
                        self._parsed_options[option_name] = None

            # Check for short option format (-o)
            elif token.startswith('-') and len(token) > 1:
                # Handle combined short options (-abc)
                if len(token) > 2 and not token[2:].startswith('-'):
                    # Each character is a separate option flag
                    for opt in token[1:]:
                        self._parsed_options[opt] = None

                else:
                    # Single short option
                    option_name = token[1:]

                    # Check if next token is a value (not an option)
                    if i + 1 < len(tokens) and not tokens[i + 1].startswith('-'):
                        self._parsed_options[option_name] = tokens[i + 1]
                        i += 1  # Skip the value token

                    else:
                        # Flag without value
                        self._parsed_options[option_name] = None

            # Not an option, add to remaining args
            else:
                self._remaining_args = ' '.join(tokens[i:])
                break

            i += 1

    def has_flag(self, name: str) -> bool:
        """
        Check if a flag option is present.

        Args:
            name: Option name (either short or long)

        Returns:
            True if flag is present
        """
        return name in self._parsed_options

    def get_option(self, name: str) -> Optional[str]:
        """
        Get an option value.

        Args:
            name: Option name (either short or long)

        Returns:
            Option value or None if not present
        """
        return self._parsed_options.get(name)

    def remaining_args(self) -> str:
        """
        Get remaining unparsed arguments.

        Returns:
            Remaining arguments string
        """
        return self._remaining_args
