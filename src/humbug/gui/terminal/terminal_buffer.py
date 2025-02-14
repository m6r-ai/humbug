"""Terminal buffer state management."""

from dataclasses import dataclass
from typing import List, Optional, Tuple

from humbug.gui.terminal.terminal_line import CharacterAttributes, TerminalLine


@dataclass
class CursorState:
    """Cursor state information."""
    row: int = 0
    col: int = 0
    visible: bool = True
    blink: bool = True
    delayed_wrap: bool = False
    saved_position: Optional[Tuple[int, int, bool, bool]] = None  # row, col, delayed_wrap, origin_mode


@dataclass
class AttributeState:
    """Character attribute state."""
    current: CharacterAttributes = CharacterAttributes.NONE
    foreground: Optional[int] = None
    background: Optional[int] = None


@dataclass
class ScrollRegion:
    """Scroll region state."""
    top: int = 0
    bottom: int = 0
    rows: int = 0


@dataclass
class OperatingModes:
    """Terminal operating modes."""
    origin: bool = False
    auto_wrap: bool = True
    application_keypad: bool = False
    application_cursor: bool = False
    bracketed_paste: bool = False


@dataclass
class MouseTrackingState:
    """Mouse tracking configuration."""
    enabled: bool = False
    mode: int = 0  # 0=off, 1000=normal, 1002=button, 1003=any
    utf8_mode: bool = False
    sgr_mode: bool = False


@dataclass
class TerminalBufferSnapshot:
    """Snapshot of terminal buffer state."""
    lines: List[TerminalLine]
    cursor: CursorState
    attributes: AttributeState
    scroll_region: ScrollRegion
    modes: OperatingModes
    mouse_tracking: MouseTrackingState
    focus_tracking: bool
    scroll_value: int


class TerminalBuffer:
    """Manages the state of a terminal screen buffer."""

    def __init__(self, rows: int, cols: int):
        """
        Initialize terminal buffer.

        Args:
            rows: Number of rows in the buffer
            cols: Number of columns in the buffer
        """
        self.rows = rows
        self.cols = cols

        # Initialize line storage
        self.lines: List[TerminalLine] = []
        self._add_new_lines(rows)

        # Initialize state objects
        self.cursor = CursorState()
        self.attributes = AttributeState()
        self.scroll_region = ScrollRegion(bottom=rows, rows=rows)
        self.modes = OperatingModes()
        self.mouse_tracking = MouseTrackingState()
        self.focus_tracking = False
        self.scroll_value = 0

    def get_new_line(self) -> TerminalLine:
        """"Get a new blank line."""
        line = TerminalLine(self.cols)
        # Fill line with spaces using default attributes

        for i in range(self.cols):
            line.set_character(i, ' ')

        return line

    def _add_new_lines(self, count: int) -> None:
        """Add new empty lines to the buffer."""
        for _ in range(count):
            self.lines.append(self.get_new_line())

    def resize(self, new_rows: int, new_cols: int) -> None:
        """
        Resize the buffer.

        Args:
            new_rows: New number of rows
            new_cols: New number of columns
        """
        old_rows = self.rows
        old_cols = self.cols

        # Update buffer dimensions
        self.rows = new_rows
        self.cols = new_cols

        # Create new lines with new width
        new_lines = []

        # Copy content from old lines
        for old_line in self.lines:
            new_line = TerminalLine(new_cols)

            # Copy existing characters
            for col in range(min(old_cols, new_cols)):
                char, attrs, fg, bg = old_line.get_character(col)
                new_line.set_character(col, char, attrs, fg, bg)

            # Pad with spaces if needed
            for col in range(old_cols, new_cols):
                new_line.set_character(col, ' ')

            new_lines.append(new_line)

        # Add additional empty lines if needed
        while len(new_lines) < new_rows:
            new_lines.append(self.get_new_line())

        # Update buffer contents
        self.lines = new_lines

        # Adjust cursor position
        self.cursor.col = min(self.cursor.col, new_cols - 1)
        self.cursor.row = min(self.cursor.row, new_rows - 1)

        # Adjust scroll region
        self.scroll_region.bottom = min(
            self.scroll_region.bottom + new_rows - old_rows, new_rows
        )
        print(f"update bottom {self.scroll_region.bottom} {old_rows} {new_rows}")

        # Ensure minimum scroll region size of 2 lines
        if self.scroll_region.bottom < self.scroll_region.top + 1:
            self.scroll_region.bottom = min(
                self.scroll_region.top + 2,
                new_rows
            )

        self.scroll_region.rows = self.scroll_region.bottom - self.scroll_region.top

    def clear(self) -> None:
        """Clear the buffer contents."""
        self.lines.clear()
        self._add_new_lines(self.rows)
        self.cursor = CursorState()
        self.attributes = AttributeState()
        self.scroll_region = ScrollRegion(bottom=self.rows, rows=self.rows)

    def create_snapshot(self) -> 'TerminalBufferSnapshot':
        """
        Create a snapshot of the current buffer state.

        Returns:
            TerminalBufferSnapshot containing current state
        """
        return TerminalBufferSnapshot(
            lines=self.lines[:],
            cursor=CursorState(
                row=self.cursor.row,
                col=self.cursor.col,
                visible=self.cursor.visible,
                blink=self.cursor.blink,
                delayed_wrap=self.cursor.delayed_wrap,
                saved_position=self.cursor.saved_position
            ),
            attributes=AttributeState(
                current=self.attributes.current,
                foreground=self.attributes.foreground,
                background=self.attributes.background
            ),
            scroll_region=ScrollRegion(
                top=self.scroll_region.top,
                bottom=self.scroll_region.bottom,
                rows=self.scroll_region.rows
            ),
            modes=OperatingModes(
                origin=self.modes.origin,
                auto_wrap=self.modes.auto_wrap,
                application_keypad=self.modes.application_keypad,
                application_cursor=self.modes.application_cursor,
                bracketed_paste=self.modes.bracketed_paste
            ),
            mouse_tracking=MouseTrackingState(
                enabled=self.mouse_tracking.enabled,
                mode=self.mouse_tracking.mode,
                utf8_mode=self.mouse_tracking.utf8_mode,
                sgr_mode=self.mouse_tracking.sgr_mode
            ),
            focus_tracking=self.focus_tracking,
            scroll_value=self.scroll_value
        )

    def restore_from_snapshot(self, snapshot: 'TerminalBufferSnapshot') -> None:
        """
        Restore buffer state from a snapshot.

        Args:
            snapshot: TerminalBufferSnapshot to restore from
        """
        self.lines = snapshot.lines[:]
        self.cursor = snapshot.cursor
        self.attributes = snapshot.attributes
        self.scroll_region = snapshot.scroll_region
        self.modes = snapshot.modes
        self.mouse_tracking = snapshot.mouse_tracking
        self.focus_tracking = snapshot.focus_tracking
        self.scroll_value = snapshot.scroll_value
