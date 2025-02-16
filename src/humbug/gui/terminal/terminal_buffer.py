"""Terminal buffer state management."""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple, Set

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
class TabStopState:
    """State of terminal tab stops."""
    # Default tab stop width (8 characters)
    DEFAULT_TAB_WIDTH = 8

    # Width of terminal in columns
    cols: int

    # Set of custom tab stop positions (column numbers)
    # Empty set means using default tab stops every 8 chars
    custom_stops: Set[int] = field(default_factory=set)

    def __init__(self, cols: int):
        """
        Initialize tab stop state.

        Args:
            cols: Width of terminal in columns
        """
        self.cols = cols
        self.custom_stops = set()

    def copy_tab_stops(self) -> 'TabStopState':
        """
        Create a deep copy of tab stops state.

        Returns:
            A new TabStopState with copied data
        """
        new_state = TabStopState(cols=self.cols)
        new_state.custom_stops = self.custom_stops.copy()
        return new_state

    def set_tab_stop(self, col: int) -> None:
        """
        Set a tab stop at the specified column.

        Args:
            col: Column number for tab stop
        """
        if 0 <= col < self.cols:
            self.custom_stops.add(col)

    def clear_tab_stop(self, col: int) -> None:
        """
        Clear tab stop at specified column.

        Args:
            col: Column number to clear
        """
        self.custom_stops.discard(col)

    def clear_all_tab_stops(self) -> None:
        """Clear all custom tab stops."""
        self.custom_stops.clear()

    def get_next_tab_stop(self, current_col: int) -> Optional[int]:
        """
        Get next tab stop position from current column.

        Args:
            current_col: Current column position

        Returns:
            Next tab stop position or None if at end of line
        """
        if current_col >= self.cols - 1:
            return None

        # If using custom tab stops
        if self.custom_stops:
            # Find next custom stop after current position
            next_stops = [col for col in self.custom_stops if col > current_col]
            if next_stops:
                return min(next_stops)
            return None

        # Using default tab stops every 8 chars
        next_stop = ((current_col // self.DEFAULT_TAB_WIDTH) + 1) * self.DEFAULT_TAB_WIDTH
        if next_stop >= self.cols:
            return None
        return next_stop

    def resize(self, new_cols: int) -> None:
        """
        Handle terminal resize.

        Args:
            new_cols: New width in columns
        """
        # Remove any custom stops beyond new width
        self.custom_stops = {col for col in self.custom_stops if col < new_cols}
        self.cols = new_cols


@dataclass
class TerminalBufferSnapshot:
    """Snapshot of terminal buffer state."""
    lines: List[TerminalLine]
    cursor: CursorState
    attributes: AttributeState
    scroll_region: ScrollRegion
    modes: OperatingModes
    tab_stops: TabStopState
    history_scrollback: bool
    max_cursor_row: int


class TerminalBuffer:
    """Manages the state of a terminal screen buffer."""

    def __init__(self, rows: int, cols: int, history_scrollback: bool):
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
        self.tab_stops = TabStopState(cols)
        self.history_scrollback = history_scrollback
        self.max_cursor_row = 0

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

        # If we're shrinking the visible display, look to see if we have any lines at the bottom of
        # the screen that we've never visited.  If we do then start by removing them!
        delete_rows = 0
        if old_rows > new_rows:
            delete_rows = max(0, old_rows - max(new_rows, self.max_cursor_row + 1))
            if delete_rows:
                del self.lines[-delete_rows:]

        old_line_count = len(self.lines)

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
        add_rows = max(0, new_rows - len(new_lines))
        for _ in range(add_rows):
            new_lines.append(self.get_new_line())

        # If we don't have a history scrollback then clip the line count
        if not self.history_scrollback and self.rows < len(new_lines):
            new_lines = new_lines[-self.rows:]

        # Update buffer contents
        self.lines = new_lines

        # Adjust cursor position
        if old_line_count + add_rows >= new_rows:
            self.cursor.row = max(0, self.cursor.row + new_rows - old_rows - add_rows + delete_rows)

        self.cursor.col = min(self.cursor.col, new_cols - 1)
        self.cursor.row = min(self.cursor.row, new_rows - 1)

        # Our max cursor row position becomes "sticky" once it hits the bottom row of the screen
        if self.max_cursor_row >= old_rows - 1:
            self.max_cursor_row = new_rows - 1 - add_rows

        # Adjust scroll region
        self.scroll_region.bottom = min(
            self.scroll_region.bottom + new_rows - old_rows, new_rows
        )

        # Ensure minimum scroll region size of 2 lines
        if self.scroll_region.bottom < self.scroll_region.top + 1:
            self.scroll_region.bottom = min(
                self.scroll_region.top + 2,
                new_rows
            )

        self.scroll_region.rows = self.scroll_region.bottom - self.scroll_region.top

        self.tab_stops.resize(new_cols)

    def clear(self) -> None:
        """Clear the buffer contents."""
        self.lines.clear()
        self._add_new_lines(self.rows)
        self.cursor = CursorState()
        self.attributes = AttributeState()
        self.scroll_region = ScrollRegion(bottom=self.rows, rows=self.rows)

    def scroll_up(self, count: int) -> None:
        """
        Scroll up within current scroll region.

        Args:
            count: Number of lines to scroll
        """
        # Calculate actual lines in scroll region
        start = len(self.lines) - self.rows + self.scroll_region.top
        end = len(self.lines) - self.rows + self.scroll_region.bottom

        # Insert blank lines at the bottom of the scrolling region and remove lines from the top
        for _ in range(count):
            self.lines.insert(end, self.get_new_line())

            # If we're using the main screen and the scrolling region top is the top of the screen
            # then we don't actually delete anything, we simply let the scrolled line roll into
            # the history buffer
            if not self.history_scrollback or self.scroll_region.top != 0:
                scrolled_line = self.lines.pop(start)
                if self.history_scrollback:
                    self.lines.insert(len(self.lines) - self.rows, scrolled_line)

    def scroll_down(self, count: int) -> None:
        """
        Scroll down within current scroll region.

        Args:
            count: Number of lines to scroll
        """
        # Calculate actual lines in scroll region
        start = len(self.lines) - self.rows + self.scroll_region.top
        end = len(self.lines) - self.rows + self.scroll_region.bottom

        # Insert blank lines at the top of the scrolling region and remove lines from the bottom
        for _ in range(count):
            self.lines.insert(start, self.get_new_line())
            del self.lines[end]

    def clear_region(self, start_row: int, start_col: int, end_row: int, end_col: int) -> None:
        """
        Clear a rectangular region of the terminal.

        Args:
            start_row: Starting row
            start_col: Starting column
            end_row: Ending row
            end_col: Ending column
        """
        for row in range(start_row, end_row + 1):
            line_index = len(self.lines) - self.rows + row
            if 0 <= line_index < len(self.lines):
                line = self.lines[line_index]
                start = start_col if row == start_row else 0
                end = end_col if row == end_row else self.cols - 1
                for col in range(start, end + 1):
                    line.set_character(col, ' ')

    def insert_lines(self, count: int) -> None:
        """
        Insert blank lines at cursor position.

        Args:
            count: Number of lines to insert
        """
        cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top
        if not (self.scroll_region.top <= cursor_row < self.scroll_region.bottom):
            return

        # Calculate lines to move
        start = len(self.lines) - self.rows + cursor_row
        end = len(self.lines) - self.rows + self.scroll_region.bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the cursor and delete them at the end of the scrolling region
        for _ in range(count):
            self.lines.insert(start, self.get_new_line())
            del self.lines[end]

        self.cursor.col = 0
        self.cursor.delayed_wrap = False

    def delete_lines(self, count: int) -> None:
        """
        Delete lines at cursor position.

        Args:
            count: Number of lines to delete
        """
        cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top
        if not (self.scroll_region.top <= cursor_row < self.scroll_region.bottom):
            return

        # Calculate lines to remove
        start = len(self.lines) - self.rows + cursor_row
        end = len(self.lines) - self.rows + self.scroll_region.bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the end of the scrolling region and remove them at the cursor
        for _ in range(count):
            self.lines.insert(end, self.get_new_line())
            del self.lines[start]

        self.cursor.col = 0
        self.cursor.delayed_wrap = False

    def insert_chars(self, count: int) -> None:
        """
        Insert blank characters at cursor position.

        Args:
            count: Number of characters to insert
        """
        cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top
        cursor_col = self.cursor.col
        line_index = len(self.lines) - self.rows + cursor_row
        if 0 <= line_index < len(self.lines):
            line = self.lines[line_index]
            # Move existing characters right
            for col in range(self.cols - 1, cursor_col - 1, -1):
                if col >= cursor_col + count:
                    char, attrs, fg, bg = line.get_character(col - count)
                    line.set_character(col, char, attrs, fg, bg)

            # Insert spaces
            for col in range(cursor_col, min(cursor_col + count, self.cols)):
                line.set_character(col, ' ')

    def delete_chars(self, count: int) -> None:
        """
        Delete characters at cursor position.

        Args:
            count: Number of characters to delete
        """
        cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top
        cursor_col = self.cursor.col
        line_index = len(self.lines) - self.rows + cursor_row
        if 0 <= line_index < len(self.lines):
            line = self.lines[line_index]
            # Move characters left
            for col in range(cursor_col, self.cols):
                if col + count < self.cols:
                    char, attrs, fg, bg = line.get_character(col + count)
                    line.set_character(col, char, attrs, fg, bg)
                else:
                    line.set_character(col, ' ')

    def erase_chars(self, count: int) -> None:
        """
        Erase characters at cursor position.

        Args:
            count: Number of characters to erase
        """
        cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top
        cursor_col = self.cursor.col
        line_index = len(self.lines) - self.rows + cursor_row
        if 0 <= line_index < len(self.lines):
            line = self.lines[line_index]
            for col in range(cursor_col, min(cursor_col + count, self.cols)):
                line.set_character(col, ' ')

    def move_cursor_up(self, amount: int) -> None:
        """Move cursor up by specified amount."""
        self.cursor.row = max(0, self.cursor.row - amount)
        self.cursor.delayed_wrap = False

    def move_cursor_down(self, amount: int) -> None:
        """Move cursor down by specified amount."""
        max_rows = self.rows if not self.modes.origin else self.scroll_region.rows
        self.cursor.row = min(max_rows - 1, self.cursor.row + amount)
        self.max_cursor_row = max(self.max_cursor_row, self.cursor.row)
        self.cursor.delayed_wrap = False

    def move_cursor_forward(self, amount: int) -> None:
        """Move cursor forward by specified amount."""
        self.cursor.col = min(self.cols - 1, self.cursor.col + amount)
        self.cursor.delayed_wrap = False

    def move_cursor_back(self, amount: int) -> None:
        """Move cursor back by specified amount."""
        self.cursor.col = max(0, self.cursor.col - amount)
        self.cursor.delayed_wrap = False

    def set_cursor_position(self, row: int, col: int) -> None:
        """Set absolute cursor position."""
        max_rows = self.rows if not self.modes.origin else self.scroll_region.rows
        self.cursor.row = min(max_rows - 1, max(0, row))
        self.max_cursor_row = max(self.max_cursor_row, self.cursor.row)
        self.cursor.col = min(self.cols - 1, max(0, col))
        self.cursor.delayed_wrap = False

    def save_cursor(self) -> None:
        """Save the cursor state."""
        self.cursor.saved_position = (
            self.cursor.row,
            self.cursor.col,
            self.cursor.delayed_wrap,
            self.modes.origin
        )

    def restore_cursor(self) -> None:
        """Restore the cursor state."""
        if not self.cursor.saved_position:
            return

        self.cursor.row, self.cursor.col, self.cursor.delayed_wrap, origin = self.cursor.saved_position
        self.modes.origin = origin

    def write_char(self, char: str) -> None:
        """
        Write a single character at the current cursor position and update cursor.

        Args:
            char: Character to write
        """
        # Handle delayed wrapping for printable characters
        if self.cursor.delayed_wrap and ord(char) >= 32:
            self.cursor.col = 0
            self.cursor.delayed_wrap = False
            cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top
            if cursor_row != self.scroll_region.bottom - 1:
                max_rows = self.rows if not self.modes.origin else self.scroll_region.rows
                self.cursor.row = min(self.cursor.row + 1, max_rows - 1)
                self.max_cursor_row = max(self.max_cursor_row, self.cursor.row)
            else:
                self.scroll_up(1)

        # Get effective cursor row considering origin mode
        cursor_row = self.cursor.row if not self.modes.origin else self.cursor.row + self.scroll_region.top

        # Handle different character types
        if char == '\r':
            self.cursor.col = 0
            self.cursor.delayed_wrap = False
            return

        if char in '\n\f\v':
            if cursor_row != self.scroll_region.bottom - 1:
                max_rows = self.rows if not self.modes.origin else self.scroll_region.rows
                self.cursor.row = min(self.cursor.row + 1, max_rows - 1)
                self.max_cursor_row = max(self.max_cursor_row, self.cursor.row)
            else:
                self.scroll_up(1)
            self.cursor.delayed_wrap = False
            return

        if char == '\b':
            self.cursor.col = max(0, self.cursor.col - 1)
            self.cursor.delayed_wrap = False
            return

        if char == '\t':
            next_stop = self.tab_stops.get_next_tab_stop(self.cursor.col)
            if next_stop is not None:
                self.cursor.col = next_stop
            else:
                self.cursor.col = self.cols - 1
            return

        # Handle printable characters
        if ord(char) >= 32:
            line_index = len(self.lines) - self.rows + cursor_row
            if 0 <= line_index < len(self.lines):
                line = self.lines[line_index]

                # Write character with current attributes
                line.set_character(
                    self.cursor.col,
                    char,
                    self.attributes.current,
                    self.attributes.foreground if self.attributes.current & CharacterAttributes.CUSTOM_FG else None,
                    self.attributes.background if self.attributes.current & CharacterAttributes.CUSTOM_BG else None
                )

                # Update cursor position and handle wrapping
                if self.cursor.col == self.cols - 1:
                    self.cursor.delayed_wrap = self.modes.auto_wrap
                else:
                    self.cursor.col += 1

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
            tab_stops=self.tab_stops.copy_tab_stops(),
            history_scrollback=self.history_scrollback,
            max_cursor_row=self.max_cursor_row
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
        self.tab_stops = snapshot.tab_stops
        self.history_scrollback = snapshot.history_scrollback
        self.max_cursor_row = snapshot.max_cursor_row
