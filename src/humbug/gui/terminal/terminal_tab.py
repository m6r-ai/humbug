"""Terminal tab implementation."""

import asyncio
from asyncio.subprocess import Process
import logging
import os
import pty
import termios
import select
from typing import Dict, Optional

from PySide6.QtWidgets import QVBoxLayout
from PySide6.QtCore import Slot
from PySide6.QtGui import QFont

from humbug.gui.tab_base import TabBase
from humbug.gui.tab_state import TabState
from humbug.gui.tab_type import TabType
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
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

        # Initialize process
        self._process: Optional[Process] = None

        # Start local shell process
        asyncio.create_task(self._start_process())

    async def _start_process(self):
        """Start the terminal process."""
        try:
            # Debug logging
            print("Starting terminal process...")

            shell = os.environ.get('SHELL', '/bin/sh')

            # Create pseudo-terminal
            master_fd, slave_fd = pty.openpty()

            # Set raw mode
            mode = termios.tcgetattr(slave_fd)
            mode[3] &= ~(termios.ECHO | termios.ICANON)  # Turn off echo and canonical mode
            termios.tcsetattr(slave_fd, termios.TCSAFLUSH, mode)

            # Start process with the slave end of the pty
            self._process = await asyncio.create_subprocess_exec(
                shell,
                stdin=slave_fd,
                stdout=slave_fd,
                stderr=slave_fd,
                start_new_session=True
            )

            # Close slave fd as the subprocess has it
            os.close(slave_fd)

            print(f"Process started with pid {self._process.pid}")

            # Create task for reading from master_fd
            asyncio.create_task(self._read_loop(master_fd))

            # Store master fd for writing
            self._master_fd = master_fd

        except Exception as e:
            self._logger.error("Failed to start terminal process: %s", str(e))
            self._terminal.put_data(f"Failed to start terminal: {str(e)}\r\n".encode())

    async def _read_loop(self, master_fd):
        """Read data from the master end of the pty."""
        try:
            while True:
                r, w, e = await asyncio.get_event_loop().run_in_executor(
                    None,
                    select.select,
                    [master_fd],
                    [],
                    []
                )

                if master_fd in r:
                    try:
                        data = os.read(master_fd, 1024)
                        if not data:
                            break

                        self._terminal.put_data(data)
                    except OSError:
                        break
        except Exception as e:
            self._logger.error("Error in read loop: %s", str(e))
        finally:
            os.close(master_fd)

    @Slot(bytes)
    def _handle_data_ready(self, data: bytes):
        """Handle data from terminal."""
        try:
            os.write(self._master_fd, data)
        except Exception as e:
            self._logger.error("Failed to write to process: %s", str(e))

    def _handle_style_changed(self):
        """Handle style changes."""
        # Update terminal font
        font = QFont(self._style_manager.monospace_font_families)
        base_size = self._style_manager.base_font_size
        font.setPointSizeF(base_size * self._style_manager.zoom_factor)
        self._terminal.setFont(font)

        # Update terminal colors
        self._terminal.setStyleSheet(f"""
            QPlainTextEdit {{
                background-color: {self._style_manager.get_color_str(ColorRole.TERMINAL_BACKGROUND)};
                color: {self._style_manager.get_color_str(ColorRole.TERMINAL_TEXT)};
                border: none;
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                selection-color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
        """)

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
        cursor = self._terminal.textCursor()
        cursor.movePosition(cursor.Start)

        for _ in range(position.get("line", 0)):
            cursor.movePosition(cursor.NextBlock)

        cursor.movePosition(
            cursor.Right,
            cursor.MoveAnchor,
            position.get("column", 0)
        )

        self._terminal.setTextCursor(cursor)

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
        if self._process:
            self._process.terminate()

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        return False

    def undo(self) -> None:
        pass

    def can_redo(self) -> bool:
        return False

    def redo(self) -> None:
        pass

    def can_cut(self) -> bool:
        return self._terminal.textCursor().hasSelection()

    def cut(self) -> None:
        self._terminal.cut()

    def can_copy(self) -> bool:
        return self._terminal.textCursor().hasSelection()

    def copy(self) -> None:
        self._terminal.copy()

    def can_paste(self) -> bool:
        return True

    def paste(self) -> None:
        self._terminal.paste()

    def show_find(self):
        """Show the find widget."""
        pass  # Terminal doesn't support find yet

    def can_submit(self) -> bool:
        return False

    def update_status(self) -> None:
        """Update status bar."""
        message = StatusMessage("Terminal: local")
        self.status_message.emit(message)
