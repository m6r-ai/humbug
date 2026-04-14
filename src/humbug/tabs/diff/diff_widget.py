"""Side-by-side diff widget."""

import logging
import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QScrollBar, QSplitter, QLabel
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QResizeEvent

from diff import DiffParser, DiffParseError

from git import GitCommandError, GitNotFoundError, GitNotRepositoryError, find_repo_root, get_file_diff

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.diff.diff_pane import DiffPane
from humbug.tabs.diff.diff_row import DiffRow
from humbug.tabs.diff.diff_view_builder import DiffViewBuilder


class DiffWidget(QWidget):
    """Widget displaying a side-by-side diff between the working tree and HEAD.

    The widget owns two DiffPane instances arranged in a QSplitter.  A single
    external QScrollBar drives both panes' vertical position simultaneously.
    Horizontal scrolling is independent per pane.

    When the file has no differences against HEAD, or when git is unavailable,
    an appropriate message is displayed instead of the panes.
    """

    status_updated = Signal()

    def __init__(self, path: str, parent: QWidget | None = None) -> None:
        """
        Initialise the diff widget for the given file path.

        Args:
            path: Absolute path to the file to diff.
        """
        super().__init__(parent)
        self._logger = logging.getLogger("DiffWidget")
        self._path = path
        self._style_manager = StyleManager()
        self._rows: list[DiffRow] = []
        self._syncing = False

        outer_layout = QVBoxLayout(self)
        outer_layout.setContentsMargins(0, 0, 0, 0)
        outer_layout.setSpacing(0)

        # Message label shown when there is nothing to diff.
        self._message_label = QLabel()
        self._message_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._message_label.hide()
        outer_layout.addWidget(self._message_label)

        # Pane area: splitter + shared scrollbar side by side.
        self._pane_container = QWidget()
        pane_layout = QHBoxLayout(self._pane_container)
        pane_layout.setContentsMargins(0, 0, 0, 0)
        pane_layout.setSpacing(0)

        self._splitter = QSplitter(Qt.Orientation.Horizontal)
        self._splitter.setHandleWidth(1)
        self._splitter.setChildrenCollapsible(False)

        self._left_pane = DiffPane()
        self._right_pane = DiffPane()
        self._splitter.addWidget(self._left_pane)
        self._splitter.addWidget(self._right_pane)

        self._scrollbar = QScrollBar(Qt.Orientation.Vertical)
        self._scrollbar.setSingleStep(1)

        pane_layout.addWidget(self._splitter)
        pane_layout.addWidget(self._scrollbar)

        outer_layout.addWidget(self._pane_container)

        # Wire up scroll sync.
        self._left_pane.verticalScrollBar().valueChanged.connect(self._on_left_scrolled)
        self._right_pane.verticalScrollBar().valueChanged.connect(self._on_right_scrolled)
        self._scrollbar.valueChanged.connect(self._on_shared_scrollbar_moved)

        # Keep the shared scrollbar range in sync with the left pane's range
        # (both panes always have the same row count so either would do).
        self._left_pane.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    def load_diff(self) -> None:
        """Compute and display the diff for the current file path."""
        diff_text = self._compute_diff()
        if diff_text is None:
            return

        if not diff_text.strip():
            self._show_message(self._no_changes_message())
            return

        rows = self._parse_diff(diff_text)
        if rows is None:
            return

        self._rows = rows
        self._left_pane.load_rows(rows, use_left=True)
        self._right_pane.load_rows(rows, use_left=False)
        self._show_panes()
        self.status_updated.emit()

    def refresh(self) -> None:
        """Re-run the diff and update the display."""
        self.load_diff()

    def path(self) -> str:
        """Return the file path this widget is diffing."""
        return self._path

    def row_count(self) -> int:
        """Return the number of diff rows currently displayed."""
        return len(self._rows)

    def apply_style(self) -> None:
        """Reapply colours and fonts after a theme or zoom change."""
        self._left_pane.apply_style()
        self._right_pane.apply_style()
        self._on_style_changed()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self._update_shared_scrollbar()

    def _compute_diff(self) -> str | None:
        """Run git to obtain the diff text.

        Returns the diff string (possibly empty), or None if an error occurred
        that has already been surfaced to the user via the message label.
        """
        try:
            repo_root = find_repo_root(self._path)
            return get_file_diff(repo_root, self._path)

        except GitNotFoundError:
            self._show_message("git is not available on this system.")
            return None

        except GitNotRepositoryError:
            self._show_message("This file is not inside a git repository.")
            return None

        except GitCommandError as e:
            self._logger.error("git error for '%s': %s", self._path, e)
            self._show_message(f"git error: {e}")
            return None

        except OSError as e:
            self._logger.error("OS error reading '%s': %s", self._path, e)
            self._show_message(f"Could not read file: {e}")
            return None

    def _parse_diff(self, diff_text: str) -> list[DiffRow] | None:
        """Parse diff text into rows.

        Returns the row list, or None if parsing failed.
        """
        try:
            parser = DiffParser()
            hunks = parser.parse(diff_text)
            builder = DiffViewBuilder()
            return builder.build(hunks)

        except DiffParseError as e:
            self._logger.error("Failed to parse diff for '%s': %s", self._path, e)
            self._show_message("Could not parse the diff output.")
            return None

    def _show_panes(self) -> None:
        """Switch the display to show the diff panes."""
        self._message_label.hide()
        self._pane_container.show()

    def _show_message(self, text: str) -> None:
        """Switch the display to show a plain message instead of diff panes."""
        self._message_label.setText(text)
        self._message_label.show()
        self._pane_container.hide()
        self._rows = []

    def _no_changes_message(self) -> str:
        """Return the message to show when the file is identical to HEAD."""
        rel = os.path.basename(self._path)
        return f"No differences — '{rel}' is identical to HEAD."

    def _on_left_scrolled(self, value: int) -> None:
        """Propagate left-pane scroll to the right pane and shared scrollbar."""
        if self._syncing:
            return

        self._syncing = True
        self._right_pane.verticalScrollBar().setValue(value)
        self._scrollbar.setValue(value)
        self._syncing = False

    def _on_right_scrolled(self, value: int) -> None:
        """Propagate right-pane scroll to the left pane and shared scrollbar."""
        if self._syncing:
            return

        self._syncing = True
        self._left_pane.verticalScrollBar().setValue(value)
        self._scrollbar.setValue(value)
        self._syncing = False

    def _on_shared_scrollbar_moved(self, value: int) -> None:
        """Propagate shared scrollbar movement to both panes."""
        if self._syncing:
            return

        self._syncing = True
        self._left_pane.verticalScrollBar().setValue(value)
        self._right_pane.verticalScrollBar().setValue(value)
        self._syncing = False

    def _on_scroll_range_changed(self, minimum: int, maximum: int) -> None:
        """Keep the shared scrollbar range in sync with the pane content."""
        self._scrollbar.setRange(minimum, maximum)
        self._scrollbar.setPageStep(self._left_pane.verticalScrollBar().pageStep())

    def _update_shared_scrollbar(self) -> None:
        """Refresh the shared scrollbar range and page step."""
        vbar = self._left_pane.verticalScrollBar()
        self._scrollbar.setRange(vbar.minimum(), vbar.maximum())
        self._scrollbar.setPageStep(vbar.pageStep())
        self._scrollbar.setValue(vbar.value())

    def _on_style_changed(self) -> None:
        """Reapply stylesheet when the theme or zoom changes."""
        bg = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        fg = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        splitter_color = self._style_manager.get_color_str(ColorRole.SPLITTER)
        base_size = self._style_manager.base_font_size()
        zoom = self._style_manager.zoom_factor()

        self.setStyleSheet(f"""
            QWidget {{
                background-color: {bg};
                color: {fg};
            }}
            QLabel {{
                color: {fg};
                font-size: {base_size * zoom}pt;
            }}
            QSplitter::handle {{
                background-color: {splitter_color};
            }}
            {self._style_manager.get_scrollbar_stylesheet()}
        """)

        self._left_pane.apply_style()
        self._right_pane.apply_style()
