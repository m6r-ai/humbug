"""Factory for creating platform-specific terminal instances."""

import os
from typing import Optional, Type

from humbug.terminal.terminal_base import TerminalBase

# Lazy imports to avoid loading unnecessary platform code
_unix_terminal: Optional[Type[TerminalBase]] = None
_windows_terminal: Optional[Type[TerminalBase]] = None


def create_terminal() -> TerminalBase:
    """Create appropriate terminal implementation for current platform."""
    global _unix_terminal, _windows_terminal

    if os.name == 'nt':
        if _windows_terminal is None:
            from humbug.terminal.windows_terminal import WindowsTerminal
            _windows_terminal = WindowsTerminal

        return _windows_terminal()

    else:
        if _unix_terminal is None:
            from humbug.terminal.unix_terminal import UnixTerminal
            _unix_terminal = UnixTerminal

        return _unix_terminal()
