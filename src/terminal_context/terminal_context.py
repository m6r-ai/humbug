"""Terminal context model."""

from terminal.terminal_base import TerminalBase
from terminal.terminal_state import TerminalState


class TerminalContext:
    """
    Model for an open terminal context.

    Owns the terminal process (TerminalBase) and buffer state (TerminalState).
    The Qt TerminalWidget is a view of the TerminalState; this class provides
    the data interface the AI tool layer needs without any Qt dependency.
    """

    def __init__(
        self,
        context_id: str,
        terminal_process: TerminalBase,
        terminal_state: TerminalState,
    ) -> None:
        """
        Initialise the terminal context.

        Args:
            context_id: Stable identifier matching the ContextRegistry entry.
            terminal_process: The running PTY process.
            terminal_state: The terminal buffer and emulator state.
        """
        self._context_id = context_id
        self._process = terminal_process
        self._state = terminal_state

    def context_id(self) -> str:
        """Return the stable context identifier."""
        return self._context_id

    def get_buffer_content(self, max_lines: int | None = None) -> str:
        """
        Return the terminal scrollback buffer as plain text.

        Args:
            max_lines: Maximum number of lines to return, or None for all.

        Returns:
            Buffer content with lines joined by newlines, trailing spaces stripped.
        """
        buffer = self._state.current_buffer()
        history_lines = buffer.history_lines()
        cols = buffer.cols()

        cursor_row = buffer.cursor().row
        rows = buffer.rows()

        # The cursor line is always the lower bound of meaningful content —
        # it is where the shell prompt sits.  Rows below the cursor are blank
        # and should be excluded.
        cursor_abs = history_lines - rows + cursor_row
        end_line = cursor_abs + 1

        if max_lines is None:
            start_line = 0

        else:
            start_line = max(0, end_line - max_lines)

        extracted_lines = []
        lines = buffer.lines()
        for line_idx in range(start_line, end_line):
            if line_idx < len(lines):
                line = lines[line_idx]
                line_text = ""
                for col in range(cols):
                    char, _, _, _ = line.get_character(col)
                    line_text += char

                extracted_lines.append(line_text.rstrip())

        return '\n'.join(extracted_lines)

    def get_status(self) -> dict:
        """
        Return a plain-dict snapshot of terminal status.

        Returns:
            Dictionary containing terminal size, cursor position, buffer
            statistics, and process information.
        """
        buffer = self._state.current_buffer()
        rows, cols = self._state.get_terminal_size()
        cursor = buffer.cursor()

        return {
            "terminal_size": (rows, cols),
            "cursor_position": (cursor.row, cursor.col),
            "cursor_visible": cursor.visible,
            "buffer_lines": buffer.history_lines(),
            "context_id": self._context_id,
            "process_running": self._process.is_running(),
            "process_id": self._process.get_process_id(),
            "process_name": self._process.get_process_name(),
            "shell": self._process.get_process_name(),
            "platform": self._process.get_platform(),
        }

    async def send_keystrokes(self, keystrokes: str) -> None:
        """
        Send keystrokes to the terminal process.

        Args:
            keystrokes: String of keystrokes to send (may include control
                characters encoded as Unicode escapes).

        Raises:
            RuntimeError: If the process is not running.
        """
        if not self._process.is_running():
            raise RuntimeError("Terminal process is not running")

        await self._process.write_data(keystrokes.encode('utf-8'))
