"""Factory for creating platform-specific terminal instances."""

import os
from typing import Type

from humbug.lib.terminal.terminal_base import TerminalBase

# Lazy imports to avoid loading unnecessary platform code
_unix_terminal: Type[TerminalBase] | None = None
_windows_terminal: Type[TerminalBase] | None = None


def create_terminal(working_directory: str) -> TerminalBase:
    """
    Create appropriate terminal implementation for current platform.

    Args:
        working_directory: Directory where the terminal process should start

    Returns:
        Platform-specific terminal instance
    """
    global _unix_terminal, _windows_terminal  # pylint: disable=global-statement

    if os.name == 'nt':
        if _windows_terminal is None:
            from humbug.lib.terminal.windows_terminal import WindowsTerminal  # pylint: disable=import-outside-toplevel
            _windows_terminal = WindowsTerminal

        return _windows_terminal(working_directory)

    if _unix_terminal is None:
        from humbug.lib.terminal.unix_terminal import UnixTerminal  # pylint: disable=import-outside-toplevel
        _unix_terminal = UnixTerminal

    return _unix_terminal(working_directory)
