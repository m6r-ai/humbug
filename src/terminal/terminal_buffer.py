"""Terminal buffer state management."""

from dataclasses import dataclass, field
from typing import List, Tuple, Set

from terminal.terminal_line import TerminalCharacterAttributes, TerminalLine


@dataclass
class CursorState:
    """Cursor state information."""
    row: int = 0
    col: int = 0
    visible: bool = True
    blink: bool = True
    delayed_wrap: bool = False
    saved_position: Tuple[int, int, bool, bool] | None = None  # row, col, delayed_wrap, origin_mode


@dataclass
class AttributeState:
    """Character attribute state."""
    current: TerminalCharacterAttributes = TerminalCharacterAttributes.NONE
    foreground: int | None = None
    background: int | None = None


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

    def get_next_tab_stop(self, current_col: int) -> int | None:
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
class BufferState:
    """Serializable terminal buffer state."""
    lines: list[list[dict]]  # List of line states
    cursor: dict  # Cursor state
    attributes: dict  # Attribute state
    scroll_region: dict  # Scroll region state
    modes: dict  # Operating modes state
    history_scrollback: bool
    max_cursor_row: int
    dimensions: dict  # Buffer dimensions


class TerminalBuffer:
    """Manages the state of a terminal screen buffer."""

    def __init__(self, rows: int, cols: int, history_scrollback: bool):
        """
        Initialize terminal buffer.

        Args:
            rows: Number of rows in the buffer
            cols: Number of columns in the buffer
        """
        self._rows = rows
        self._cols = cols

        # Initialize state objects
        self._cursor = CursorState()
        self._attributes = AttributeState()
        self._scroll_region = ScrollRegion(bottom=rows, rows=rows)
        self._modes = OperatingModes()
        self._tab_stops = TabStopState(cols)
        self._history_scrollback = history_scrollback
        self._max_cursor_row = 0

        # Initialize line storage
        self._lines: List[TerminalLine] = []
        self._add_new_lines(rows)

    def get_state(self) -> BufferState:
        """
        Get serializable buffer state.

        Returns:
            BufferState containing complete buffer state
        """
        # Serialize line data
        lines_data = []
        for line in self._lines:
            line_data = []
            for col in range(line.width):
                char, attrs, fg, bg = line.get_character(col)
                line_data.append({
                    'char': char,
                    'attributes': attrs.value,
                    'fg_color': fg,
                    'bg_color': bg
                })
            lines_data.append(line_data)

        return BufferState(
            lines=lines_data,
            cursor={
                'row': self._cursor.row,
                'col': self._cursor.col,
                'visible': self._cursor.visible,
                'blink': self._cursor.blink,
                'delayed_wrap': self._cursor.delayed_wrap,
                'saved_position': self._cursor.saved_position
            },
            attributes={
                'current': self._attributes.current.value,
                'foreground': self._attributes.foreground,
                'background': self._attributes.background
            },
            scroll_region={
                'top': self._scroll_region.top,
                'bottom': self._scroll_region.bottom,
                'rows': self._scroll_region.rows
            },
            modes={
                'origin': self._modes.origin,
                'auto_wrap': self._modes.auto_wrap,
                'application_keypad': self._modes.application_keypad,
                'application_cursor': self._modes.application_cursor,
                'bracketed_paste': self._modes.bracketed_paste
            },
            history_scrollback=self._history_scrollback,
            max_cursor_row=self._max_cursor_row,
            dimensions={
                'rows': self._rows,
                'cols': self._cols
            }
        )

    def restore_state(self, state: BufferState) -> None:
        """
        Restore buffer state from saved state.

        Args:
            state: BufferState to restore from
        """
        # First resize buffer if needed
        if (state.dimensions['rows'] != self._rows or
            state.dimensions['cols'] != self._cols):
            self.resize(state.dimensions['rows'], state.dimensions['cols'])

        # Restore lines
        self._lines = []
        for line_data in state.lines:
            line = self._get_new_line(len(line_data))
            for col, char_data in enumerate(line_data):
                line.set_character(
                    col,
                    char_data['char'],
                    TerminalCharacterAttributes(char_data['attributes']),
                    char_data['fg_color'],
                    char_data['bg_color']
                )
            self._lines.append(line)

        # Restore cursor state
        self._cursor = CursorState(
            row=state.cursor['row'],
            col=state.cursor['col'],
            visible=state.cursor['visible'],
            blink=state.cursor['blink'],
            delayed_wrap=state.cursor['delayed_wrap'],
            saved_position=state.cursor['saved_position']
        )

        # Restore other state components
        self._attributes = AttributeState(
            current=TerminalCharacterAttributes(state.attributes['current']),
            foreground=state.attributes['foreground'],
            background=state.attributes['background']
        )

        self._scroll_region = ScrollRegion(
            top=state.scroll_region['top'],
            bottom=state.scroll_region['bottom'],
            rows=state.scroll_region['rows']
        )

        self._modes = OperatingModes(
            origin=state.modes['origin'],
            auto_wrap=state.modes['auto_wrap'],
            application_keypad=state.modes['application_keypad'],
            application_cursor=state.modes['application_cursor'],
            bracketed_paste=state.modes['bracketed_paste']
        )

        self._history_scrollback = state.history_scrollback
        self._max_cursor_row = state.max_cursor_row

    def _get_new_line(self, cols: int) -> TerminalLine:
        """"Get a new blank line."""
        line = TerminalLine(cols)
        # Fill line with spaces using default attributes

        fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None
        for i in range(cols):
            line.set_character(i, ' ', self._attributes.current, fg, bg)

        return line

    def _add_new_lines(self, count: int) -> None:
        """Add new empty lines to the buffer."""
        for _ in range(count):
            self._lines.append(self._get_new_line(self._cols))

    def resize(self, new_rows: int, new_cols: int) -> None:
        """
        Resize the buffer.

        Args:
            new_rows: New number of rows
            new_cols: New number of columns
        """
        old_rows = self._rows

        # Update buffer dimensions
        self._rows = new_rows
        self._cols = new_cols

        # If we're shrinking the visible display, look to see if we have any lines at the bottom of
        # the screen that we've never visited.  If we do then start by removing them!
        delete_rows = 0
        if old_rows > new_rows:
            delete_rows = max(0, old_rows - max(new_rows, self._max_cursor_row + 1))
            if delete_rows:
                del self._lines[-delete_rows:]

        old_line_count = len(self._lines)

        # Create new lines with new width
        new_lines = []

        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        # Copy content from old lines
        for old_line in self._lines:
            new_line = TerminalLine(max(new_cols, old_line.width))

            # Copy existing characters
            for col in range(old_line.width):
                char, attrs, fg, bg = old_line.get_character(col)
                new_line.set_character(col, char, attrs, fg, bg)

            # Pad with spaces if needed
            for col in range(old_line.width, new_cols):
                new_line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

            new_lines.append(new_line)

        # Add additional empty lines if needed
        add_rows = max(0, new_rows - len(new_lines))
        for _ in range(add_rows):
            new_lines.append(self._get_new_line(self._cols))

        # If we don't have a history scrollback then clip the line count
        if not self._history_scrollback and self._rows < len(new_lines):
            new_lines = new_lines[-self._rows:]

        # Update buffer contents
        self._lines = new_lines

        # Adjust cursor position
        if old_line_count + add_rows >= new_rows:
            self._cursor.row = max(0, self._cursor.row + new_rows - old_rows - add_rows + delete_rows)

        self._cursor.col = min(self._cursor.col, new_cols - 1)
        self._cursor.row = min(self._cursor.row, new_rows - 1)

        # Our max cursor row position becomes "sticky" once it hits the bottom row of the screen
        if self._max_cursor_row >= old_rows - 1:
            self._max_cursor_row = new_rows - 1 - add_rows

        # Adjust scroll region
        self._scroll_region.bottom = min(
            self._scroll_region.bottom + new_rows - old_rows, new_rows
        )

        # Ensure minimum scroll region size of 2 lines
        if self._scroll_region.bottom < self._scroll_region.top + 1:
            self._scroll_region.bottom = min(
                self._scroll_region.top + 2,
                new_rows
            )

        self._scroll_region.rows = self._scroll_region.bottom - self._scroll_region.top

        self._tab_stops.resize(new_cols)

    def clear(self) -> None:
        """Clear the buffer contents."""
        self._lines.clear()
        self._add_new_lines(self._rows)
        self._cursor = CursorState()
        self._attributes = AttributeState()
        self._scroll_region = ScrollRegion(bottom=self._rows, rows=self._rows)
        self._max_cursor_row = 0

    def scroll_up(self, count: int) -> None:
        """
        Scroll up within current scroll region.

        Args:
            count: Number of lines to scroll
        """
        # Calculate actual lines in scroll region
        start = len(self._lines) - self._rows + self._scroll_region.top
        end = len(self._lines) - self._rows + self._scroll_region.bottom

        # Insert blank lines at the bottom of the scrolling region and remove lines from the top
        for _ in range(count):
            self._lines.insert(end, self._get_new_line(self._cols))

            # If we're using the main screen and the scrolling region top is the top of the screen
            # then we don't actually delete anything, we simply let the scrolled line roll into
            # the history buffer
            if not self._history_scrollback or self._scroll_region.top != 0:
                scrolled_line = self._lines.pop(start)
                if self._history_scrollback:
                    self._lines.insert(len(self._lines) - self._rows, scrolled_line)

    def scroll_down(self, count: int) -> None:
        """
        Scroll down within current scroll region.

        Args:
            count: Number of lines to scroll
        """
        # Calculate actual lines in scroll region
        start = len(self._lines) - self._rows + self._scroll_region.top
        end = len(self._lines) - self._rows + self._scroll_region.bottom

        # Insert blank lines at the top of the scrolling region and remove lines from the bottom
        for _ in range(count):
            self._lines.insert(start, self._get_new_line(self._cols))
            del self._lines[end]

    def clear_region(self, start_row: int, start_col: int, end_row: int, end_col: int) -> None:
        """
        Clear a rectangular region of the terminal.

        Args:
            start_row: Starting row
            start_col: Starting column
            end_row: Ending row
            end_col: Ending column
        """
        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        for row in range(start_row, end_row + 1):
            line_index = len(self._lines) - self._rows + row
            if 0 <= line_index < len(self._lines):
                line = self._lines[line_index]
                start = start_col if row == start_row else 0
                end = end_col if row == end_row else self._cols - 1
                for col in range(start, end + 1):
                    line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

    def insert_lines(self, count: int) -> None:
        """
        Insert blank lines at cursor position.

        Args:
            count: Number of lines to insert
        """
        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        if not self._scroll_region.top <= cursor_row < self._scroll_region.bottom:
            return

        # Calculate lines to move
        start = len(self._lines) - self._rows + cursor_row
        end = len(self._lines) - self._rows + self._scroll_region.bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the cursor and delete them at the end of the scrolling region
        for _ in range(count):
            self._lines.insert(start, self._get_new_line(self._cols))
            del self._lines[end]

        self._cursor.col = 0
        self._cursor.delayed_wrap = False

    def delete_lines(self, count: int) -> None:
        """
        Delete lines at cursor position.

        Args:
            count: Number of lines to delete
        """
        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        if not self._scroll_region.top <= cursor_row < self._scroll_region.bottom:
            return

        # Calculate lines to remove
        start = len(self._lines) - self._rows + cursor_row
        end = len(self._lines) - self._rows + self._scroll_region.bottom

        # Clip the count
        count = min(count, end - start)

        # Insert blank lines at the end of the scrolling region and remove them at the cursor
        for _ in range(count):
            self._lines.insert(end, self._get_new_line(self._cols))
            del self._lines[start]

        self._cursor.col = 0
        self._cursor.delayed_wrap = False

    def insert_chars(self, count: int) -> None:
        """
        Insert blank characters at cursor position.

        Args:
            count: Number of characters to insert
        """
        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        cursor_col = self._cursor.col
        line_index = len(self._lines) - self._rows + cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            # Move existing characters right
            for col in range(self._cols - 1, cursor_col - 1, -1):
                if col >= cursor_col + count:
                    char, attrs, fg, bg = line.get_character(col - count)
                    line.set_character(col, char, attrs, fg, bg)

            # Insert spaces
            for col in range(cursor_col, min(cursor_col + count, self._cols)):
                line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

    def delete_chars(self, count: int) -> None:
        """
        Delete characters at cursor position.

        Args:
            count: Number of characters to delete
        """
        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        cursor_col = self._cursor.col
        line_index = len(self._lines) - self._rows + cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            # Move characters left
            for col in range(cursor_col, self._cols):
                if col + count < self._cols:
                    char, attrs, fg, bg = line.get_character(col + count)
                    line.set_character(col, char, attrs, fg, bg)

                else:
                    line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

    def erase_chars(self, count: int) -> None:
        """
        Erase characters at cursor position.

        Args:
            count: Number of characters to erase
        """
        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        cursor_col = self._cursor.col
        line_index = len(self._lines) - self._rows + cursor_row
        if 0 <= line_index < len(self._lines):
            line = self._lines[line_index]
            for col in range(cursor_col, min(cursor_col + count, self._cols)):
                line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

    def erase_in_display(self, mode: int) -> None:
        """Handle erase is display commands."""
        if mode == 0:  # Clear from cursor to end
            self.clear_region(
                self._cursor.row,
                self._cursor.col,
                self._rows - 1,
                self._cols - 1
            )

        elif mode == 1:  # Clear from start to cursor
            self.clear_region(0, 0, self._cursor.row, self._cursor.col)

        elif mode == 2:  # Clear entire screen
            self.clear_region(0, 0, self._rows - 1, self._cols - 1)

    def erase_in_line(self, mode: int) -> None:
        """Handle erase in line commands."""
        if mode == 0:  # Clear from cursor to end
            self.clear_region(
                self._cursor.row,
                self._cursor.col,
                self._cursor.row,
                self._cols - 1
            )

        elif mode == 1:  # Clear from start to cursor
            self.clear_region(
                self._cursor.row,
                0,
                self._cursor.row,
                self._cursor.col
            )

        elif mode == 2:  # Clear entire line
            self.clear_region(
                self._cursor.row,
                0,
                self._cursor.row,
                self._cols - 1
            )

    def move_cursor_up(self, amount: int) -> None:
        """Move cursor up by specified amount."""
        self._cursor.row = max(0, self._cursor.row - amount)
        self._cursor.delayed_wrap = False

    def move_cursor_down(self, amount: int) -> None:
        """Move cursor down by specified amount."""
        max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
        self._cursor.row = min(max_rows - 1, self._cursor.row + amount)
        self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)
        self._cursor.delayed_wrap = False

    def move_cursor_forward(self, amount: int) -> None:
        """Move cursor forward by specified amount."""
        self._cursor.col = min(self._cols - 1, self._cursor.col + amount)
        self._cursor.delayed_wrap = False

    def move_cursor_back(self, amount: int) -> None:
        """Move cursor back by specified amount."""
        self._cursor.col = max(0, self._cursor.col - amount)
        self._cursor.delayed_wrap = False

    def set_cursor_position(self, row: int, col: int) -> None:
        """Set absolute cursor position."""
        max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
        self._cursor.row = min(max_rows - 1, max(0, row - 1))
        self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)
        self._cursor.col = min(self._cols - 1, max(0, col - 1))
        self._cursor.delayed_wrap = False

    def set_cursor_horizontal(self, col: int) -> None:
        """Set the absolute horizontal cursor position."""
        self._cursor.col = col - 1
        self._cursor.delayed_wrap = False

    def set_cursor_vertical(self, row: int) -> None:
        """Set the absolute vertical cursor position."""
        max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
        self._cursor.row = min(row - 1, max_rows - 1)
        self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)
        self._cursor.delayed_wrap = False

    def save_cursor(self) -> None:
        """Save the cursor state."""
        self._cursor.saved_position = (
            self._cursor.row,
            self._cursor.col,
            self._cursor.delayed_wrap,
            self._modes.origin
        )

    def restore_cursor(self) -> None:
        """Restore the cursor state."""
        if not self._cursor.saved_position:
            return

        self._cursor.row, self._cursor.col, self._cursor.delayed_wrap, origin = self._cursor.saved_position
        self._modes.origin = origin

    def index(self) -> None:
        """Handle the index operation."""
        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        if cursor_row == self._scroll_region.bottom - 1:
            self.scroll_up(1)

        else:
            max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
            self._cursor.row = min(self._cursor.row + 1, max_rows - 1)
            self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)

        self._cursor.delayed_wrap = False

    def reverse_index(self) -> None:
        """Handle the reverse index operation."""
        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        if cursor_row == self._scroll_region.top:
            self.scroll_down(1)

        else:
            self._cursor.row = max(0, self._cursor.row - 1)

        self._cursor.delayed_wrap = False

    def next_line(self) -> None:
        """Handle the next line operation."""
        self._cursor.col = 0
        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
        if cursor_row == self._scroll_region.bottom - 1:
            self.scroll_up(1)

        else:
            max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
            self._cursor.row = min(self._cursor.row + 1, max_rows - 1)
            self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)

        self._cursor.delayed_wrap = False

    def set_top_and_bottom_margins(self, top: int, bottom: int) -> None:
        """Set the top and bottom margins."""
        top = min(max(0, top - 1), self._rows - 1)
        bottom = min(max(0, bottom), self._rows)
        if top < bottom:
            self._scroll_region.top = top
            self._scroll_region.bottom = bottom
            self._scroll_region.rows = bottom - top + 1

        self._cursor.col = 0
        self._cursor.row = 0
        self._cursor.delayed_wrap = False

    def set_origin(self, origin_mode: bool) -> None:
        """Set or reset origin mode."""
        self._modes.origin = origin_mode
        self._cursor.row = 0
        self._max_cursor_row = 0
        self._cursor.col = 0
        self._cursor.delayed_wrap = False

    def set_cursor_blink(self, blink_mode: bool) -> None:
        """Set or reset cursor blink mode."""
        self._cursor.blink = blink_mode

    def set_cursor_visible(self, visible_mode: bool) -> None:
        """Set or reset cursor visibility."""
        self._cursor.visible = visible_mode

    def set_tab_stop(self) -> None:
        """Set a tab stop at the current column."""
        self._tab_stops.set_tab_stop(self._cursor.col)

    def clear_tab_stop(self) -> None:
        """Clear a tab stop at the current column."""
        self._tab_stops.clear_tab_stop(self._cursor.col)

    def clear_all_tab_stops(self) -> None:
        """Clear all tab stops."""
        self._tab_stops.clear_all_tab_stops()

    def decaln(self) -> None:
        """Display the DECALN screen alignment pattern."""
        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        for r in range(self._rows):
            line_index = len(self._lines) - self._rows + r
            line = self._lines[line_index]
            for c in range(self._cols):
                line.set_character(c, 'E', self._attributes.current, default_fg, default_bg)

    def write_char(self, char: str) -> None:
        """
        Write a single character at the current cursor position and update cursor.

        Args:
            char: Character to write
        """
        # Handle delayed wrapping for printable characters
        if self._cursor.delayed_wrap and ord(char) >= 32:
            self._cursor.col = 0
            self._cursor.delayed_wrap = False
            cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
            if cursor_row == self._scroll_region.bottom - 1:
                self.scroll_up(1)

            else:
                max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
                self._cursor.row = min(self._cursor.row + 1, max_rows - 1)
                self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)

        # Get effective cursor row considering origin mode
        cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top

        # Handle different character types
        if char == '\r':
            self._cursor.col = 0
            self._cursor.delayed_wrap = False
            return

        if char in '\n\f\v':
            if cursor_row == self._scroll_region.bottom - 1:
                self.scroll_up(1)

            else:
                max_rows = self._rows if not self._modes.origin else self._scroll_region.rows
                self._cursor.row = min(self._cursor.row + 1, max_rows - 1)
                self._max_cursor_row = max(self._max_cursor_row, self._cursor.row)

            self._cursor.delayed_wrap = False
            return

        if char == '\b':
            self._cursor.col = max(0, self._cursor.col - 1)
            self._cursor.delayed_wrap = False
            return

        if char == '\t':
            next_stop = self._tab_stops.get_next_tab_stop(self._cursor.col)
            if next_stop is not None:
                self._cursor.col = next_stop

            else:
                self._cursor.col = self._cols - 1

            return

        # Handle printable characters
        if ord(char) >= 32:
            line_index = len(self._lines) - self._rows + cursor_row
            if 0 <= line_index < len(self._lines):
                line = self._lines[line_index]

                # Write character with current attributes
                line.set_character(
                    self._cursor.col,
                    char,
                    self._attributes.current,
                    self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None,
                    self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None
                )

                # Update cursor position and handle wrapping
                if self._cursor.col == self._cols - 1:
                    self._cursor.delayed_wrap = self._modes.auto_wrap

                else:
                    self._cursor.col += 1

    def history_lines(self) -> int:
        """Get the number of lines of history including the current display buffer"""
        return len(self._lines)

    def attributes(self) -> AttributeState:
        """Get the current attribute state."""
        return self._attributes

    def cursor(self) -> CursorState:
        """Get the current cursor state."""
        return self._cursor

    def modes(self) -> OperatingModes:
        """Get the current operating modes."""
        return self._modes

    def scroll_region(self) -> ScrollRegion:
        """Get the current scroll region."""
        return self._scroll_region

    def lines(self) -> List[TerminalLine]:
        """Get the list of terminal lines."""
        return self._lines

    def rows(self) -> int:
        """Get the number of rows in the buffer."""
        return self._rows

    def cols(self) -> int:
        """Get the number of columns in the buffer."""
        return self._cols

    def max_cursor_row(self) -> int:
        """Get the maximum cursor row reached."""
        return self._max_cursor_row

    def blinking_chars_on_screen(self) -> bool:
        """Determine if there are any blinking characters on-screen."""
        for line in self._lines[-self._rows:]:
            for col in range(self._cols):
                if line.get_character(col)[1] & TerminalCharacterAttributes.BLINK:
                    return True

        return False
