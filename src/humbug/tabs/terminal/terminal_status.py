"""Terminal status data structures."""

from dataclasses import dataclass
from typing import Tuple


@dataclass
class TerminalWidgetStatusInfo:
    """Widget-level terminal status information."""
    terminal_size: Tuple[int, int]  # (rows, cols)
    cursor_position: Tuple[int, int]  # (row, col)
    cursor_visible: bool
    buffer_lines: int


@dataclass
class TerminalStatusInfo:
    """Complete terminal status information."""
    # Widget info
    terminal_size: Tuple[int, int]
    cursor_position: Tuple[int, int]
    cursor_visible: bool
    buffer_lines: int

    # Tab/Process info
    tab_id: str
    tab_running: bool
    process_id: int | None
    process_running: bool
    process_name: str
