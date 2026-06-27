"""Terminal buffer state management."""

from dataclasses import dataclass, field

from terminal.terminal_line import TerminalCharacterAttributes, TerminalLine


@dataclass
class CursorState:
    """Cursor state information."""
    row: int = 0
    col: int = 0
    visible: bool = True
    blink: bool = True
    delayed_wrap: bool = False
    saved_position: tuple[int, int, bool, bool] | None = None  # row, col, delayed_wrap, origin_mode


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
    custom_stops: set[int] = field(default_factory=set)

    def __init__(self, cols: int):
        """
        Initialize tab stop state.

        Args:
            cols: Width of terminal in columns
        """
        self.cols = cols

        # When True, tab navigation uses default stops every DEFAULT_TAB_WIDTH
        # columns.  Set to False by clear_all_tab_stops(); set back to True
        # whenever a custom stop is explicitly added.
        self._use_defaults = True
        self.custom_stops = set()

    def copy_tab_stops(self) -> 'TabStopState':
        """
        Create a deep copy of tab stops state.

        Returns:
            A new TabStopState with copied data
        """
        new_state = TabStopState(cols=self.cols)
        new_state.custom_stops = self.custom_stops.copy()
        new_state._use_defaults = self._use_defaults  # pylint: disable=protected-access
        return new_state

    def set_tab_stop(self, col: int) -> None:
        """
        Set a tab stop at the specified column.

        Args:
            col: Column number for tab stop
        """
        if 0 <= col < self.cols:
            self.custom_stops.add(col)
            self._use_defaults = False

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
        self._use_defaults = False

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

        # If there are explicit custom stops, use them
        if self.custom_stops and not self._use_defaults:
            # Find next custom stop after current position
            next_stops = [col for col in self.custom_stops if col > current_col]
            if next_stops:
                return min(next_stops)

            return None

        if self.custom_stops:
            # Custom stops exist alongside defaults: find the nearest of either
            next_custom = [col for col in self.custom_stops if col > current_col]
            default_stop = ((current_col // self.DEFAULT_TAB_WIDTH) + 1) * self.DEFAULT_TAB_WIDTH
            candidates = next_custom
            if default_stop < self.cols:
                candidates = candidates + [default_stop]

            if candidates:
                return min(candidates)

            return None

        if not self._use_defaults:
            # All stops cleared — no tab stops at all
            return None

        # Pure default tab stops every DEFAULT_TAB_WIDTH columns
        next_stop = ((current_col // self.DEFAULT_TAB_WIDTH) + 1) * self.DEFAULT_TAB_WIDTH
        return next_stop if next_stop < self.cols else None

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
    lines: list[dict]  # List of line states, each {'continuation': bool, 'cells': list[dict]}
    cursor: dict  # Cursor state
    attributes: dict  # Attribute state
    scroll_region: dict  # Scroll region state
    modes: dict  # Operating modes state
    history_scrollback: bool
    scrollback_limit: int | None
    max_cursor_row: int
    dimensions: dict  # Buffer dimensions


class TerminalBuffer:
    """Manages the state of a terminal screen buffer."""

    def __init__(self, rows: int, cols: int, history_scrollback: bool, scrollback_limit: int | None = None):
        """
        Initialize terminal buffer.

        Args:
            rows: Number of rows in the buffer
            cols: Number of columns in the buffer
            history_scrollback: Whether to keep history (False for alternate buffer)
            scrollback_limit: Maximum total lines to keep (including visible rows), None for unlimited
        """
        self._rows = rows
        self._cols = cols
        self._history_scrollback = history_scrollback
        self._scrollback_limit = scrollback_limit

        # Initialize state objects
        self._cursor = CursorState()
        self._attributes = AttributeState()
        self._scroll_region = ScrollRegion(bottom=rows, rows=rows)
        self._modes = OperatingModes()
        self._tab_stops = TabStopState(cols)
        self._max_cursor_row = 0

        # Initialize line storage
        self._lines: list[TerminalLine] = []
        self._add_new_lines(rows)

    def _trim_scrollback(self) -> None:
        """Trim scrollback buffer to configured limit if needed."""
        if self._scrollback_limit is not None and len(self._lines) > self._scrollback_limit:
            excess = len(self._lines) - self._scrollback_limit
            del self._lines[:excess]

    def get_state(self) -> BufferState:
        """
        Get serializable buffer state.

        Returns:
            BufferState containing complete buffer state
        """
        # Serialize line data
        lines_data = []
        for line in self._lines:
            cells = []
            for col in range(line.width):
                char, attrs, fg, bg = line.get_character(col)
                cells.append({
                    'char': char,
                    'attributes': attrs.value,
                    'fg_color': fg,
                    'bg_color': bg
                })

            lines_data.append({'continuation': line.continuation, 'cells': cells})

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
            scrollback_limit=self._scrollback_limit,
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

        # Restore scrollback limit
        self._scrollback_limit = state.scrollback_limit

        # Restore lines
        self._lines = []
        for line_data in state.lines:
            cells: list[dict] = line_data['cells']
            line = self._get_new_line(len(cells))
            line.continuation = bool(line_data.get('continuation', False))
            for col, char_data in enumerate(cells):
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

        # If we have no actual work to do then return
        if new_rows == self._rows and new_cols == self._cols:
            return

        # If number of columns is unchanged then no reflow is needed.  Just adjust the number
        # of visible rows in-place.
        if new_cols == self._cols:
            # Remove unvisited blank lines at the bottom before shrinking, for
            # the same reason as the full reflow path.
            if new_rows < old_rows:
                unvisited = max(0, old_rows - (self._max_cursor_row + 1))
                to_remove = min(unvisited, old_rows - new_rows)
                if to_remove:
                    del self._lines[-to_remove:]

            # Capture absolute indices before appending or trimming changes len(self._lines).
            cursor_abs = len(self._lines) - min(old_rows, len(self._lines)) + self._cursor.row
            max_cursor_abs = len(self._lines) - min(old_rows, len(self._lines)) + self._max_cursor_row

            # Ensure there are at least new_rows lines total.
            shortfall = new_rows - len(self._lines)
            if shortfall > 0:
                for _ in range(shortfall):
                    self._lines.append(self._get_new_line(self._cols))

            self._rows = new_rows
            self._trim_scrollback()

            self._finish_resize(old_rows, new_rows, new_cols, cursor_abs, 0, False, max_cursor_abs)
            return

        # Remove unvisited blank lines at the bottom of the visible area when
        # resizing.  This prevents them from being pushed into scrollback history
        # during reflow or row reduction.
        if new_cols < self._cols or new_rows < old_rows:
            unvisited = max(0, old_rows - (self._max_cursor_row + 1))
            if unvisited:
                del self._lines[-unvisited:]

        # Locate the cursor in absolute line-list coordinates before reflow so
        # we can recompute its position afterwards.
        # Note: computed after unvisited-line deletion so len(self._lines) is correct.
        cursor_abs = len(self._lines) - min(old_rows, len(self._lines)) + self._cursor.row
        cursor_col_before = self._cursor.col
        # If delayed_wrap is set the cursor is logically one past the last column.
        if self._cursor.delayed_wrap:
            cursor_col_before = self._cols

        # Reflow: group physical lines into logical lines, then re-wrap at new_cols
        default_fg = self._attributes.foreground if self._attributes.current & TerminalCharacterAttributes.CUSTOM_FG else None
        default_bg = self._attributes.background if self._attributes.current & TerminalCharacterAttributes.CUSTOM_BG else None

        # Locate max_cursor_row in absolute line-list coordinates so it can be
        # mapped through the reflow, just like the cursor position.
        max_cursor_abs = len(self._lines) - min(old_rows, len(self._lines)) + self._max_cursor_row

        new_lines: list[TerminalLine] = []
        # Absolute index in new_lines where the cursor lands; resolved below.
        new_cursor_abs = 0
        new_cursor_col = 0
        new_cursor_delayed_wrap = False
        cursor_resolved = False
        new_max_cursor_abs = 0
        max_cursor_resolved = False

        # Walk the existing lines, collecting each logical line (the first
        # physical line plus all consecutive continuation lines).
        i = 0
        physical_lines = self._lines
        n = len(physical_lines)

        while i < n:
            # Collect all physical lines belonging to this logical line.
            logical_start = i
            cells: list[tuple] = []
            while i < n:
                phys = physical_lines[i]
                for col in range(phys.width):
                    cells.append(phys.get_character(col))

                i += 1
                if i >= n or not physical_lines[i].continuation:
                    break

            # Check whether the cursor falls inside this logical line.
            cursor_in_logical = (
                not cursor_resolved
                and logical_start <= cursor_abs < logical_start + (i - logical_start)
            )

            if cursor_in_logical:
                # Compute the cursor's offset within the concatenated cells.
                lines_before_cursor = cursor_abs - logical_start

                # Each old physical line had old_rows columns... no: we need
                # the actual widths of the physical lines we concatenated.
                cell_offset = 0
                for k in range(lines_before_cursor):
                    cell_offset += physical_lines[logical_start + k].width

                cell_offset += cursor_col_before

            # Strip trailing spaces from the logical line so that re-wrapping
            # produces clean lines, but never strip past the cursor position.
            strip_limit = cell_offset if cursor_in_logical else 0
            while len(cells) > strip_limit and cells[-1][0] == ' ':
                cells.pop()

            # Check whether max_cursor_row falls inside this logical line.
            max_cursor_in_logical = (
                not max_cursor_resolved
                and logical_start <= max_cursor_abs < logical_start + (i - logical_start)
            )
            if max_cursor_in_logical:
                # Compute the cell offset of max_cursor_row within the
                # concatenated logical line, analogous to cell_offset for the
                # cursor.  We only need the row boundary (col 0) because
                # max_cursor_row is a row-level high-water mark, not a column.
                max_lines_before = max_cursor_abs - logical_start
                max_cell_offset = 0
                for k in range(max_lines_before):
                    max_cell_offset += physical_lines[logical_start + k].width

            # Re-wrap the logical line into new_cols-wide physical lines.
            if not cells:
                # Blank logical line — emit one empty physical line.
                new_line = TerminalLine(new_cols)
                for col in range(new_cols):
                    new_line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

                new_line.continuation = physical_lines[logical_start].continuation
                if cursor_in_logical:
                    new_cursor_abs = len(new_lines)
                    new_cursor_col = 0
                    cursor_resolved = True

                if max_cursor_in_logical:
                    new_max_cursor_abs = len(new_lines)
                    max_cursor_resolved = True

                new_lines.append(new_line)

            else:
                offset = 0
                first_in_logical = True
                while offset <= len(cells):
                    chunk = cells[offset:offset + new_cols]
                    new_line = TerminalLine(new_cols)
                    for col, (ch, attrs, fg, bg) in enumerate(chunk):
                        new_line.set_character(col, ch, attrs, fg, bg)

                    # Pad remainder with spaces
                    for col in range(len(chunk), new_cols):
                        new_line.set_character(col, ' ', self._attributes.current, default_fg, default_bg)

                    # First physical line of a logical group inherits the
                    # original continuation flag; subsequent ones are True.
                    new_line.continuation = physical_lines[logical_start].continuation if first_in_logical else True
                    first_in_logical = False

                    if cursor_in_logical and not cursor_resolved:
                        if offset <= cell_offset < offset + new_cols or (not chunk and cell_offset >= offset):
                            new_cursor_abs = len(new_lines)
                            new_cursor_col = min(cell_offset - offset, new_cols - 1)
                            new_cursor_delayed_wrap = (
                                cell_offset - offset >= new_cols
                                or self._cursor.delayed_wrap and cell_offset == len(cells)
                            )
                            cursor_resolved = True

                    new_lines.append(new_line)

                    if max_cursor_in_logical and not max_cursor_resolved:
                        if offset <= max_cell_offset < offset + new_cols or (not chunk and max_cell_offset >= offset):
                            new_max_cursor_abs = len(new_lines) - 1
                            max_cursor_resolved = True

                    offset += new_cols
                    if offset >= len(cells):
                        break

        # Ensure there are at least new_rows lines total.
        add_rows = max(0, new_rows - len(new_lines))
        for _ in range(add_rows):
            new_lines.append(self._get_new_line(new_cols))

        # For non-history buffers (alternate screen) clip to new_rows.
        if not self._history_scrollback and len(new_lines) > new_rows:
            new_lines = new_lines[-new_rows:]

        # Commit.
        self._rows = new_rows
        self._cols = new_cols
        self._lines = new_lines
        self._trim_scrollback()

        if not cursor_resolved:
            new_cursor_abs = len(self._lines) - new_rows
            new_cursor_col = 0

        if not max_cursor_resolved:
            new_max_cursor_abs = len(self._lines) - new_rows

        self._finish_resize(
            old_rows, new_rows, new_cols, new_cursor_abs, new_cursor_col, new_cursor_delayed_wrap, new_max_cursor_abs
        )

    def _finish_resize(
        self,
        old_rows: int,
        new_rows: int,
        new_cols: int,
        cursor_abs: int,
        cursor_col: int,
        cursor_delayed_wrap: bool,
        max_cursor_abs: int,
    ) -> None:
        """
        Apply cursor, max_cursor, scroll region, and tab stop updates after a resize.

        Called by resize() after the line list has been committed and trimmed.
        Both the rows-only fast path and the full reflow path share this tail.

        Args:
            old_rows: Row count before the resize.
            new_rows: Row count after the resize.
            new_cols: Column count after the resize.
            cursor_abs: Absolute line-list index where the cursor landed.
            cursor_col: New cursor column.
            cursor_delayed_wrap: New cursor delayed-wrap flag.
            max_cursor_abs: Absolute line-list index of max_cursor_row.
        """
        self._cursor.row = max(0, min(cursor_abs - (len(self._lines) - new_rows), new_rows - 1))
        self._cursor.col = max(0, min(cursor_col, new_cols - 1))
        self._cursor.delayed_wrap = cursor_delayed_wrap

        self._max_cursor_row = max(0, min(max_cursor_abs - (len(self._lines) - new_rows), new_rows - 1))

        self._scroll_region.bottom = min(
            self._scroll_region.bottom + new_rows - old_rows, new_rows
        )
        if self._scroll_region.bottom < self._scroll_region.top + 1:
            self._scroll_region.bottom = min(self._scroll_region.top + 2, new_rows)

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

    def append_lines_to_visible(self, count: int) -> None:
        """
        Append blank lines to the end of the visible screen area.

        Args:
            count: Number of blank lines to insert
        """
        for _ in range(count):
            self._lines.append(self._get_new_line(self._cols))

        self._trim_scrollback()

    def remove_lines_from_visible(self, count: int) -> None:
        """
        Remove blank lines from the end of the visible screen area.

        Args:
            count: Number of blank lines to remove
        """
        if count > 0:
            del self._lines[-count:]

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

        # Trim scrollback if needed
        self._trim_scrollback()

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

            # Mark the line we just wrapped onto as a soft-wrap continuation
            cursor_row = self._cursor.row if not self._modes.origin else self._cursor.row + self._scroll_region.top
            line_index = len(self._lines) - self._rows + cursor_row
            if 0 <= line_index < len(self._lines):
                self._lines[line_index].continuation = True

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

    def lines(self) -> list[TerminalLine]:
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
