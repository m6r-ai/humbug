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

        # Track unique options by their long name
        unique_options = {}

        # Collect unique options (long and short forms point to same descriptor)
        for option_name, descriptor in self._options.items():
            # Use long_name as the key for uniqueness
            unique_options[descriptor.long_name] = descriptor

        # Sort by long name for consistent ordering
        for long_name in sorted(unique_options.keys()):
            option = unique_options[long_name]

            # Format as -s, --long
            option_format = f"  -{option.short_name}, --{option.long_name}"

            # Add value placeholder if option takes a value
            if option.takes_value:
                value_desc = option.value_description or "VALUE"
                option_format += f" <{value_desc}>"

            # Add help text
            help_text += f"{option_format}\n    {option.help_text}\n"

        return help_text
