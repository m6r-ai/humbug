"""Terminal tab implementation."""

import asyncio
import fcntl
import logging
import os
import select
import signal
import struct
import termios
from typing import Dict, Optional, Set

from PySide6.QtWidgets import QVBoxLayout
from PySide6.QtCore import Slot
from PySide6.QtGui import QFont

from humbug.gui.tab_base import TabBase
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.terminal.terminal_process import TerminalProcess
from humbug.gui.terminal.terminal_widget import TerminalWidget
from humbug.gui.status_message import StatusMessage


class TerminalTab(TabBase):
    """Tab containing a terminal emulator."""

    def __init__(self, tab_id: str, command: Optional[str] = None, parent=None):
        """
        Initialize terminal tab.

        Args:
            tab_id: Unique identifier for this tab
            command: Optional command to run in terminal
            parent: Optional parent widget
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("TerminalTab")
        self._command = command
        self._style_manager = StyleManager()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create terminal widget
        self._terminal = TerminalWidget(self)
        layout.addWidget(self._terminal)

        # Connect signals
        self._terminal.data_ready.connect(self._handle_data_ready)

        # Handle style changes
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Install activation tracking for the terminal
        self._install_activation_tracking(self._terminal)

        # Initialize process and task tracking
        self._terminal_process = TerminalProcess()
        self._tasks: Set[asyncio.Task] = set()
        self._running = True
        self._main_fd = None

        # Initialize window size handling
        self._terminal.size_changed.connect(self._handle_terminal_resize)

        # Start local shell process
        self._create_tracked_task(self._start_process())

    def _update_pty_size(self, fd: int) -> None:
        """Update PTY size using current terminal dimensions.

        Args:
            fd: File descriptor for PTY

        Raises:
            OSError: If ioctl call fails
        """
        try:
            rows, cols = self._terminal.get_terminal_size()
            fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack('HHHH', rows, cols, 0, 0))
        except OSError as e:
            self._logger.error(f"Failed to update PTY size: {e}")
            raise

    def _handle_terminal_resize(self):
        """Handle terminal window resize events."""
        if self._main_fd is not None:
            try:
                self._update_pty_size(self._main_fd)

                # Signal the shell process group
                pid = self._terminal_process.get_process_id()
                print(f"signal shell {os.getpgid(pid)}")
                if pid:
                    os.killpg(os.getpgid(pid), signal.SIGWINCH)
            except OSError as e:
                self._logger.error(f"Failed to handle window resize: {e}")

    def _create_tracked_task(self, coro) -> asyncio.Task:
        """
        Create a tracked asyncio task.

        Args:
            coro: Coroutine to create task from

        Returns:
            Created task
        """
        task = asyncio.create_task(coro)
        self._tasks.add(task)
        task.add_done_callback(self._tasks.discard)
        return task

    async def _start_process(self):
        """Start the terminal process."""
        try:
            self._logger.debug("Starting terminal process...")

            # Start process using our new implementation
            pid, main_fd = await self._terminal_process.start(self._command)
            self._main_fd = main_fd

            self._logger.debug(f"Process started with pid {pid}")

            # Update initial terminal size
            try:
                self._update_pty_size(main_fd)
            except OSError as e:
                self._logger.warning(f"Failed to set initial terminal size: {e}")

            # Create task for reading from main_fd
            self._create_tracked_task(self._read_loop(main_fd))

        except Exception as e:
            self._logger.error("Failed to start terminal process: %s", str(e))
            self._terminal.put_data(f"Failed to start terminal: {str(e)}\r\n".encode())

    async def _read_loop(self, main_fd):
        """Read data from the main end of the pty."""
        try:
            while self._running:
                try:
                    # Check if process has ended
                    if not self._terminal_process.is_running():
                        self._logger.info("Shell process ended")
                        break

                    r, w, e = await asyncio.get_event_loop().run_in_executor(
                        None,
                        select.select,
                        [main_fd],
                        [],
                        [],
                        0.1  # Add timeout to allow checking _running
                    )

                    if not r:
                        continue

                    if main_fd in r:
                        try:
                            data = os.read(main_fd, 1024)
                            if not data:
                                break

                            self._terminal.put_data(data)
                        except OSError as e:
                            self._logger.error(f"Error reading from PTY: {e}")
                            break
                except (OSError, select.error) as e:
                    if not self._running:
                        break
                    self._logger.error("Error reading from terminal: %s", str(e))
                    break
        except Exception as e:
            self._logger.error("Error in read loop: %s", str(e))

        # Only show completion message if not in cleanup
        if self._running:
            try:
                self._terminal.put_data(b"\r\n[Process completed]\r\n")
            except Exception as e:
                self._logger.debug(f"Could not write completion message: {e}")

    @Slot(bytes)
    def _handle_data_ready(self, data: bytes):
        """Handle data from terminal."""
        try:
            if self._main_fd is not None and self._running:
                os.write(self._main_fd, data)
        except Exception as e:
            self._logger.error("Failed to write to process: %s", str(e))

    def _handle_style_changed(self):
        """Handle style changes."""
        # Update terminal font
        font = QFont(self._style_manager.monospace_font_families)
        base_size = self._style_manager.base_font_size
        font.setPointSizeF(base_size * self._style_manager.zoom_factor)
        self._terminal.setFont(font)

        # Apply consistent styling to both the terminal widget and its viewport
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
            QAbstractScrollArea {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
            QAbstractScrollArea::viewport {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: none;
            }}
            QScrollBar:vertical, QScrollBar:horizontal {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
                height: 12px;
            }}
            QScrollBar::handle:vertical, QScrollBar::handle:horizontal {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
                min-width: 20px;
            }}
            QScrollBar::add-page, QScrollBar::sub-page {{
                background: none;
            }}
            QScrollBar::add-line, QScrollBar::sub-line {{
                height: 0px;
                width: 0px;
            }}
            QAbstractScrollArea::corner {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
        """)

        self._terminal.update_dimensions()

    async def _cleanup(self):
        """Clean up resources."""
        self._running = False

        # Cancel all pending tasks
        for task in self._tasks:
            task.cancel()

        if self._tasks:
            await asyncio.gather(*self._tasks, return_exceptions=True)

        # Use new termination method
        await self._terminal_process.terminate()
        self._main_fd = None

    def get_state(self, temp_state: bool = False) -> TabState:
        """Get serializable state."""
        return TabState(
            type=TabType.TERMINAL,
            tab_id=self._tab_id,
            path="terminal://local",
            metadata={
                "command": self._command
            } if temp_state else None
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent=None) -> 'TerminalTab':
        """Restore terminal from saved state."""
        if state.type != TabType.TERMINAL:
            raise ValueError(f"Invalid tab type for TerminalTab: {state.type}")

        command = None
        if state.metadata:
            command = state.metadata.get("command")

        return cls(state.tab_id, command, parent)

    def set_cursor_position(self, position: Dict[str, int]) -> None:
        """Set cursor position."""
        self._terminal._update_cursor_position(
            position.get("line", 0),
            position.get("column", 0)
        )

    def get_cursor_position(self) -> Dict[str, int]:
        """Get current cursor position."""
        cursor = self._terminal.textCursor()
        return {
            "line": cursor.blockNumber(),
            "column": cursor.columnNumber()
        }

    def can_close(self) -> bool:
        """Check if terminal can be closed."""
        return True

    def close(self) -> None:
        """Close the terminal."""
        asyncio.create_task(self._cleanup())

    def can_save(self) -> bool:
        """Check if terminal can be saved."""
        return False

    def save(self) -> bool:
        """Save terminal (not supported)."""
        return True

    def can_save_as(self) -> bool:
        """Check if terminal can be saved as (not supported)."""
        return False

    def save_as(self) -> bool:
        """Save terminal as (not supported)."""
        return True

    def can_undo(self) -> bool:
        """Check if terminal can undo (not supported)."""
        return False

    def undo(self) -> None:
        """Undo terminal operation (not supported)."""
        pass

    def can_redo(self) -> bool:
        """Check if terminal can redo (not supported)."""
        return False

    def redo(self) -> None:
        """Redo terminal operation (not supported)."""
        pass

    def can_cut(self) -> bool:
        """Check if terminal can cut (not supported)."""
        return False

    def cut(self) -> None:
        """Cut terminal selection (not supported)."""
        pass

    def can_copy(self) -> bool:
        """Check if terminal can copy."""
        return self._terminal.has_selection()

    def copy(self) -> None:
        """Copy terminal selection."""
        self._terminal.copy()

    def can_paste(self) -> bool:
        """Check if terminal can paste."""
        return True

    def paste(self) -> None:
        """Paste into terminal."""
        self._terminal.paste()

    def show_find(self):
        """Show the find widget (not supported)."""
        pass  # Terminal doesn't support find yet

    def can_submit(self) -> bool:
        """Check if terminal can submit (not supported)."""
        return False

    def update_status(self) -> None:
        """Update status bar."""
        message = StatusMessage("Terminal: local")
        self.status_message.emit(message)
