"""Terminal emulator framework."""

from terminal.terminal_base import TerminalBase
from terminal.terminal_buffer import TerminalCharacterAttributes, TerminalBuffer
from terminal.terminal_factory import create_terminal
from terminal.terminal_state import TerminalState


__all__ = [
    "TerminalBase",
    "TerminalBuffer",
    "TerminalCharacterAttributes",
    "TerminalState",
    "create_terminal"
]
