"""Terminal tab implementation."""

import asyncio
import logging
from typing import Any, Coroutine, Dict, Set

from PySide6.QtWidgets import QVBoxLayout, QWidget

from terminal import TerminalBase, create_terminal

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.status_message import StatusMessage
from humbug.style_manager import StyleManager
from humbug.tabs.find_widget import FindWidget
from humbug.tabs.tab_base import TabBase
from humbug.tabs.tab_state import TabState
from humbug.tabs.tab_type import TabType
from humbug.tabs.terminal.terminal_widget import TerminalWidget


class UTF8Buffer:
    """Class to handle UTF-8 character streams."""
    def __init__(self) -> None:
        self.buffer = b''

    def add_data(self, data: bytes) -> bytes:
        """Add data to buffer and return complete UTF-8 characters."""
        self.buffer += data
        # Find the last complete character
        boundary = self.find_last_complete_character()

        # Split at the boundary
        complete = self.buffer[:boundary]
        self.buffer = self.buffer[boundary:]
        return complete

    def find_last_complete_character(self) -> int:
        """Find index where last complete UTF-8 character ends."""
        i = len(self.buffer)
        while i > 0:
            i -= 1
            byte = self.buffer[i]

            # If we find a start byte (not continuation byte)
            if byte & 0b11000000 != 0b10000000:
                # Check if this starts a complete sequence
                if byte & 0b10000000 == 0:  # ASCII byte
                    return i + 1

                if (byte & 0b11100000) == 0b11000000:  # 2-byte sequence
                    if i + 2 <= len(self.buffer):
                        return i + 2

                elif (byte & 0b11100000) == 0b11100000:  # 3-byte sequence
                    if i + 3 <= len(self.buffer):
                        return i + 3

                elif (byte & 0b11110000) == 0b11110000:  # 4-byte sequence
                    if i + 4 <= len(self.buffer):
                        return i + 4

                # If we get here, we found an incomplete sequence
                return i
        return 0


class TerminalTab(TabBase):
    """Tab containing a terminal emulator."""

    def __init__(
        self,
        tab_id: str,
        command: str | None = None,
        parent: QWidget | None = None,
        start_process: bool = True
    ) -> None:
        """
        Initialize terminal tab.

        Args:
            tab_id: Unique identifier for this tab, or a UUID will be generated if not provided.
            command: Optional command to run in terminal
            parent: Optional parent widget
            start_process: Whether to start the terminal process immediately
        """
        super().__init__(tab_id, parent)
        self._logger = logging.getLogger("TerminalTab")
        self._command = command
        self._style_manager = StyleManager()
        self._mindspace_manager = MindspaceManager()

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Add find widget at top (initially hidden)
        self._find_widget = FindWidget()
        self._find_widget.hide()
        self._find_widget.closed.connect(self._close_find)
        self._find_widget.find_next.connect(lambda: self._find_next(True))
        self._find_widget.find_previous.connect(lambda: self._find_next(False))
        layout.addWidget(self._find_widget)

        # Create terminal widget
        self._terminal_widget = TerminalWidget(self)
        layout.addWidget(self._terminal_widget)

        # Connect signals
        self._terminal_widget.data_ready.connect(self._on_data_ready)

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Handle style changes
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        # Initialize process and task tracking
        self._terminal_process: TerminalBase = create_terminal(self._mindspace_manager.mindspace_path())
        self._tasks: Set[asyncio.Task] = set()
        self._running = True
        self._transferring = False

        # Initialize window size handling
        self._terminal_widget.size_changed.connect(self._on_terminal_size_changed)

        # Start local shell process
        if start_process:
            self._create_tracked_task(self._start_process())

    def activate(self) -> None:
        """Activate the tab."""
        self._terminal_widget.setFocus()

    def _on_language_changed(self) -> None:
        """Update language-specific elements when language changes."""
        # Update find widget text if visible
        if not self._find_widget.isHidden():
            current, total = self._terminal_widget.get_match_status()
            self._find_widget.set_match_status(current, total)

        # Update status bar
        self.update_status()

    def _on_terminal_size_changed(self) -> None:
        """Handle terminal window resize events."""
        rows, cols = self._terminal_widget.get_terminal_size()
        self._terminal_process.update_window_size(rows, cols)
        self.update_status()

    def _create_tracked_task(self, coro: Coroutine[Any, Any, None]) -> asyncio.Task:
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

    async def _start_process(self) -> None:
        """Start the terminal process."""
        try:
            self._logger.debug("Starting terminal process...")

            # Start process using platform-specific implementation
            pid, _main_fd = await self._terminal_process.start(self._command)
            self._logger.debug("Process started with pid %d", pid)

            # Update initial terminal size
            rows, cols = self._terminal_widget.get_terminal_size()
            self._terminal_process.update_window_size(rows, cols)
            self.update_status()

            # Create task for reading
            self._create_tracked_task(self._read_loop())

        except Exception as e:
            self._logger.exception("Failed to start terminal process: %s", str(e))
            self._terminal_widget.put_data(f"Failed to start terminal: {str(e)}\r\n".encode())

    async def _read_loop(self) -> None:
        """Read data from the terminal."""
        try:
            utf8_buffer = UTF8Buffer()

            while self._running:
                try:
                    # Check if process has ended
                    if not self._terminal_process.is_running():
                        self._logger.info("Shell process ended")
                        break

                    # Read using platform-specific implementation with timeout
                    try:
                        # Call read_data directly without wait_for since it's already async
                        data = await self._terminal_process.read_data(8192)

                        # Check we didn't stop running
                        if not self._running:
                            break

                        if data:
                            complete_data = utf8_buffer.add_data(data)
                            if complete_data:
                                self._terminal_widget.put_data(complete_data)

                    except EOFError:
                        # Terminal pipe closed/EOF reached
                        self._logger.info("Terminal pipe closed")
                        break

                    except OSError as e:
                        if not self._running:
                            break

                        self._logger.exception("Error reading from terminal: %s", str(e))
                        break

                except asyncio.CancelledError:
                    self._logger.debug("Read loop task cancelled")
                    raise  # Re-raise to ensure proper task cancellation

                except Exception as e:
                    if not self._running:
                        break

                    self._logger.exception("Error in read loop: %s", str(e))
                    break

        except asyncio.CancelledError:
            self._logger.debug("Read loop task cancelled")
            raise  # Re-raise to ensure proper task cancellation

        except Exception as e:
            self._logger.exception("Error in read loop: %s", str(e))

        finally:
            if self._running and not self._transferring:
                try:
                    self._terminal_widget.put_data(b"\r\n[Process completed]\r\n")

                except Exception as e:
                    self._logger.debug("Could not write completion message: %s", e)

    def _on_data_ready(self, data: bytes) -> None:
        """Handle data from terminal."""
        try:
            if self._running:
                asyncio.create_task(self._terminal_process.write_data(data))

        except Exception as e:
            self._logger.error("Failed to write to process: %s", str(e))

    def _on_style_changed(self) -> None:
        """Handle style changes."""
        # Apply consistent styling to both the terminal widget and its viewport
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
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

        self._terminal_widget.update_dimensions()

    def get_state(self, temp_state: bool = False) -> TabState:
        """
        Get serializable state.

        For temporary state (like moving tabs), this includes:
        1. Terminal visual state (buffers, cursor, etc.)
        2. Process state (open file descriptors and PIDs)
        3. Command used to start the terminal

        For permanent state, only includes:
        1. Command used to start the terminal
        2. Basic terminal settings

        Args:
            temp_state: Whether this is for temporary state capture

        Returns:
            TabState containing tab state
        """
        metadata: Dict[str, Any] = {}

        # Store command for both persistent and temporary state
        if self._command:
            metadata["command"] = self._command

        # Only store process/display state for temporary moves
        if temp_state:
            metadata.update(self._terminal_widget.create_state_metadata())
            # Store reference to this tab for process transfer
            metadata['source_tab'] = self
            metadata['is_ephemeral'] = True

        return TabState(
            type=TabType.TERMINAL,
            tab_id=self._tab_id,
            path="terminal://local",
            metadata=metadata
        )

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> 'TerminalTab':
        """
        Restore terminal from saved state.

        Args:
            state: State to restore from
            parent: Parent widget

        Returns:
            New TerminalTab instance
        """
        command = None
        start_process = True
        if state.metadata:
            command = state.metadata.get("command")

            if state.metadata.get('is_ephemeral') and 'source_tab' in state.metadata:
                start_process = False

        tab = cls(state.tab_id, command, parent, start_process=start_process)

        # Only restore process/display state if this is ephemeral (temp) state
        if state.metadata and state.metadata.get('is_ephemeral'):
            tab._terminal_widget.restore_from_metadata(state.metadata)

            # If we have a source tab, transfer the process
            if 'source_tab' in state.metadata:
                source_tab = state.metadata['source_tab']
                tab._transfer_process_from(source_tab)

        return tab

    # pylint: disable=protected-access
    def _transfer_process_from(self, source_tab: 'TerminalTab') -> None:
        """
        Transfer terminal process state from another tab.

        This method safely transfers ownership of a running terminal process
        from one tab to another, ensuring proper cleanup and preventing
        resource leaks.

        Args:
            source_tab: Source terminal tab to transfer from
        """
        if source_tab._terminal_process is None:
            return

        # Mark source tab as transferring to prevent completion message
        source_tab._transferring = True

        # Stop process management in source tab without terminating process
        source_tab._running = False

        # Cancel all tasks in source tab
        for task in source_tab._tasks:
            task.cancel()

        source_tab._tasks.clear()

        # Create a new terminal implementation for this tab
        self._terminal_process = create_terminal(self._mindspace_manager.mindspace_path())

        # Transfer the process state
        source_tab._terminal_process.transfer_to(self._terminal_process)

        # Create new read loop task in this tab
        self._create_tracked_task(self._read_loop())

        # Log transfer for debugging
        self._logger.debug(
            "Transferred process %d from tab %s to tab %s",
            self._terminal_process.get_process_id(),
            source_tab._tab_id,
            self._tab_id
        )

    def set_path(self, path: str) -> None:
        """
        Set the path associated with this tab.

        Args:
            path: Path to associate with this tab
        """
        # Do nothing for terminal tabs

    def can_close_tab(self) -> bool:
        """Check if terminal can be closed."""
        return True

    def close_tab(self) -> None:
        """Close the terminal."""
        # Immediately stop running and prevent further operations
        self._running = False
        self._transferring = False

        # Cancel all existing tasks
        for task in self._tasks:
            if not task.done():
                task.cancel()

        # Clear task set
        self._tasks.clear()

        # Terminate process
        if self._terminal_process:
            try:
                loop = asyncio.get_event_loop()
                if loop.is_running():
                    # Create and run termination task directly
                    loop.create_task(self._terminal_process.terminate())

            except Exception as e:
                self._logger.exception("Error terminating process: %s", str(e))

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

    def can_redo(self) -> bool:
        """Check if terminal can redo (not supported)."""
        return False

    def redo(self) -> None:
        """Redo terminal operation (not supported)."""

    def can_cut(self) -> bool:
        """Check if terminal can cut (not supported)."""
        return False

    def cut(self) -> None:
        """Cut terminal selection (not supported)."""

    def can_copy(self) -> bool:
        """Check if terminal can copy."""
        return self._terminal_widget.has_selection()

    def copy(self) -> None:
        """Copy terminal selection."""
        self._terminal_widget.copy()

    def can_paste(self) -> bool:
        """Check if terminal can paste."""
        return True

    def paste(self) -> None:
        """Paste into terminal."""
        self._terminal_widget.paste()

    def show_find(self) -> None:
        """Show the find widget."""
        # Get the selected text if any
        if self._terminal_widget.has_selection():
            text = self._terminal_widget.get_selected_text()
            # Only use selection if it's on a single line
            if '\n' not in text:
                self._find_widget.set_search_text(text)
            else:
                self._find_widget.set_search_text("")

        self._find_widget.show()
        self._find_widget.setFocus()

    def can_submit(self) -> bool:
        """Check if terminal can submit (not supported)."""
        return False

    def submit(self) -> None:
        """Submit terminal input (not supported)."""

    def update_status(self) -> None:
        """Update status bar."""
        name = self._terminal_process.get_process_name()
        rows, columns = self._terminal_widget.get_terminal_size()
        message = StatusMessage(
            self._language_manager.strings().terminal_status.format(
                name=name,
                columns=columns,
                rows=rows
            )
        )
        self.status_message.emit(message)

    def _close_find(self) -> None:
        """Close the find widget and clear search state."""
        self._find_widget.hide()
        self._terminal_widget.clear_find()

    def _find_next(self, forward: bool = True) -> None:
        """Find next/previous match."""
        text = self._find_widget.get_search_text()
        self._terminal_widget.find_text(text, forward)
        current, total = self._terminal_widget.get_match_status()
        self._find_widget.set_match_status(current, total)
