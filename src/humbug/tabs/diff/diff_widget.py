"""Side-by-side diff widget."""

import logging
import os
from typing import List, Optional, Set, Tuple

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QScrollBar, QSplitter, QLabel, QSizePolicy
from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtGui import QKeyEvent, QResizeEvent

from diff import DiffParser, DiffParseError
from diff.diff_types import DiffHunk

from git import GitCommandError, GitNotFoundError, GitNotRepositoryError, find_repo_root, get_file_at_head, get_file_diff

from syntax import ProgrammingLanguageUtils

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager
from humbug.tabs.diff.diff_pane import DiffPane
from humbug.tabs.diff.diff_row import DiffRow, DiffRowType
from humbug.tabs.diff.diff_view_builder import DiffViewBuilder
from humbug.tabs.smooth_scroll import SMOOTH_SCROLL_DURATION_MS, SMOOTH_SCROLL_INTERVAL_MS


class DiffWidget(QWidget):
    """
    Widget displaying a side-by-side diff between the working tree and HEAD.

    The widget owns two DiffPane instances arranged in a QSplitter.  A single
    external QScrollBar drives both panes' vertical position simultaneously.
    Horizontal scrolling is independent per pane.

    Both the HEAD version and the working-tree version of the file are loaded in
    full so that syntax highlighting can process each pane's document from top to
    bottom with correct parser-state propagation.

    When the file has no differences against HEAD, or when git is unavailable,
    an appropriate message is displayed instead of the panes.
    """

    status_updated = Signal()
    open_in_editor_requested = Signal()
    open_in_preview_requested = Signal()

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
        self._cached_hunks: List[Tuple[int, int]] = []
        self._current_hunk_index: int = -1
        self._hunk_scroll_target: int = -1
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

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

        # Wire pane open requests up to widget-level signals.
        self._left_pane.open_in_editor_requested.connect(self.open_in_editor_requested)
        self._right_pane.open_in_editor_requested.connect(self.open_in_editor_requested)
        self._left_pane.open_in_preview_requested.connect(self.open_in_preview_requested)
        self._right_pane.open_in_preview_requested.connect(self.open_in_preview_requested)

        # Keep the shared scrollbar range in sync with the left pane's range
        # (both panes always have the same row count so either would do).
        self._left_pane.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)
        self._scrollbar.valueChanged.connect(self._update_active_hunk)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

        # Find state: flat list of (pane, start, end) tuples across both panes,
        # ordered by row position so navigation feels natural.
        self._find_matches: List[Tuple[str, int, int]] = []  # ('left'|'right', start, end)
        self._find_current: int = -1
        self._find_text: str = ""
        self._find_key: tuple = ("", False, False)

        # Smooth scrolling
        self._smooth_scroll_timer = QTimer(self)
        self._smooth_scroll_timer.setInterval(SMOOTH_SCROLL_INTERVAL_MS)
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)
        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = SMOOTH_SCROLL_DURATION_MS
        self._smooth_scroll_time: int = 0

    def load_diff(self, initial_load: bool = False) -> None:
        """
        Fetch both file versions and display the full side-by-side diff.

        Args:
            initial_load: If True, scroll to the first hunk after loading.
        """
        result = self._fetch_content()
        if result is None:
            return

        old_lines, new_lines, diff_text = result

        if not diff_text.strip():
            self._show_message(self._no_changes_message())
            return

        hunks = self._parse_diff(diff_text)
        if hunks is None:
            return

        builder = DiffViewBuilder()
        self._rows = builder.build(old_lines, new_lines, hunks)

        # Load rows first so every block has its _BlockData attached before the
        # highlighter runs.  set_syntax() triggers a full rehighlight, by which
        # point all blocks carry the metadata the highlighter needs.
        self._left_pane.load_rows(self._rows, use_left=True)
        self._right_pane.load_rows(self._rows, use_left=False)

        language = ProgrammingLanguageUtils.from_file_extension(self._path)
        self._left_pane.set_syntax(language)
        self._right_pane.set_syntax(language)
        self._show_panes()
        self.status_updated.emit()
        self._cached_hunks = self._hunks()
        self._current_hunk_index = -1
        self._update_active_hunk()

        if initial_load and self._cached_hunks:
            start = self._cached_hunks[0][0]
            self._current_hunk_index = 0
            self._set_active_hunk(self._cached_hunks[0][0], self._cached_hunks[0][1])
            QTimer.singleShot(0, lambda: self._start_smooth_scroll(
                self._left_pane.target_scroll_for_block(start)
            ))

        # Re-run the active search against the new document content, if any.
        if self._find_text:
            self._run_find(self._find_text, forward=True, reset=True)

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

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle keyboard scrolling for the shared scrollbar."""
        key = event.key()
        bar = self._scrollbar

        _NAVIGATION_MODIFIERS = (
            Qt.KeyboardModifier.AltModifier
            | Qt.KeyboardModifier.ControlModifier
            | Qt.KeyboardModifier.ShiftModifier
        )
        if event.modifiers() & _NAVIGATION_MODIFIERS:
            super().keyPressEvent(event)
            return

        if key == Qt.Key.Key_Up:
            bar.setValue(bar.value() - bar.singleStep())
            event.accept()

        elif key == Qt.Key.Key_Down:
            bar.setValue(bar.value() + bar.singleStep())
            event.accept()

        elif key == Qt.Key.Key_PageUp:
            bar.setValue(bar.value() - bar.pageStep())
            event.accept()

        elif key == Qt.Key.Key_PageDown:
            bar.setValue(bar.value() + bar.pageStep())
            event.accept()

        else:
            super().keyPressEvent(event)

    def _fetch_content(self) -> Optional[tuple[List[str], List[str], str]]:
        """
        Retrieve the HEAD content, working-tree content, and diff text.

        Returns a tuple of (old_lines, new_lines, diff_text), or None if an
        error occurred that has already been surfaced via the message label.
        Old lines are empty for untracked files (no HEAD version exists).
        """
        try:
            repo_root = find_repo_root(self._path)
            diff_text = get_file_diff(repo_root, self._path)

            head_content = get_file_at_head(repo_root, self._path)
            old_lines = head_content.splitlines() if head_content is not None else []

            with open(self._path, encoding="utf-8", errors="replace") as f:
                new_lines = f.read().splitlines()

            return old_lines, new_lines, diff_text

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

    def _parse_diff(self, diff_text: str) -> Optional[List[DiffHunk]]:
        """Parse diff text into hunks.

        Returns the hunk list, or None if parsing failed.
        """
        try:
            parser = DiffParser()
            return parser.parse(diff_text)

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

    def find_text(self, text: str, forward: bool = True, case_sensitive: bool = False, regexp: bool = False) -> Tuple[int, int]:
        """
        Search for *text* across both panes and navigate to the next match.

        Matches from the left pane and the right pane are merged in document
        order (by character position) so that navigation follows the visual
        top-to-bottom flow of the diff.  Both panes are highlighted
        simultaneously: the active match is bright, all others are dim.

        Args:
            text: Text to search for.
            forward: If True move to the next match; if False move to the
                previous match.
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.

        Returns:
            Tuple of (current_match_1based, total_matches).  Both values are
            0 when there are no matches.
        """
        if (text, case_sensitive, regexp) != self._find_key:
            # New search term — rebuild the match list from scratch.
            self._run_find(text, forward=forward, reset=True, case_sensitive=case_sensitive, regexp=regexp)
        else:
            self._run_find(text, forward=forward, reset=False, case_sensitive=case_sensitive, regexp=regexp)

        return self.get_match_status()

    def _run_find(self, text: str, forward: bool, reset: bool, case_sensitive: bool = False, regexp: bool = False) -> None:
        """
        Internal helper that (re)builds matches and advances the cursor.

        Args:
            text: Search string.
            forward: Direction of navigation.
            reset: If True, rebuild the match list; if False, only advance.
            case_sensitive: If True, match case exactly.
            regexp: If True, treat text as a regular expression.
        """
        self._find_text = text
        self._find_key = (text, case_sensitive, regexp)

        if reset or not self._find_matches:
            self._find_current = -1
            self._find_matches = []

            if text:
                # Collect matches from the left pane …
                left_matches = self._left_pane.find_matches(text, case_sensitive, regexp)
                for start, end in left_matches:
                    self._find_matches.append(("left", start, end))

                # … and the right pane, interleaved by block (row) number so
                # that navigation follows the visual order of the diff.
                right_matches = self._right_pane.find_matches(text, case_sensitive, regexp)
                for start, end in right_matches:
                    self._find_matches.append(("right", start, end))

                # Sort by the block number of the match start so that left and
                # right matches on the same row appear together, left first.
                def _sort_key(item: Tuple[str, int, int]) -> Tuple[int, int]:
                    pane_id, start, _end = item
                    pane = self._left_pane if pane_id == "left" else self._right_pane
                    block = pane.document().findBlock(start)
                    return (block.blockNumber(), 0 if pane_id == "left" else 1)

                self._find_matches.sort(key=_sort_key)

        if not self._find_matches:
            self._left_pane.clear_find()
            self._right_pane.clear_find()
            return

        # Advance the current match index.
        total = len(self._find_matches)
        if self._find_current == -1:
            self._find_current = 0 if forward else total - 1

        elif forward:
            self._find_current = (self._find_current + 1) % total

        else:
            self._find_current = (self._find_current - 1) % total

        self._apply_highlights()

        # Scroll the active pane to the current match.
        pane_id, start, _end = self._find_matches[self._find_current]
        pane = self._left_pane if pane_id == "left" else self._right_pane
        self._start_smooth_scroll(pane.target_scroll_for_match(start))

    def _apply_highlights(self) -> None:
        """Repaint all match highlights in both panes."""
        left_matches = [(s, e) for p, s, e in self._find_matches if p == "left"]
        right_matches = [(s, e) for p, s, e in self._find_matches if p == "right"]

        # Convert global current index to per-pane index.
        pane_id = self._find_matches[self._find_current][0] if self._find_current != -1 else ""
        left_current_local = -1
        right_current_local = -1
        if pane_id == "left":
            left_current_local = sum(1 for p, s, e in self._find_matches[:self._find_current] if p == "left")
        elif pane_id == "right":
            right_current_local = sum(1 for p, s, e in self._find_matches[:self._find_current] if p == "right")

        self._left_pane.highlight_matches(left_matches, left_current_local)
        self._right_pane.highlight_matches(right_matches, right_current_local)

    def _start_smooth_scroll(self, target_value: int) -> None:
        """
        Start smooth scrolling animation to target value.

        Args:
            target_value: Target scrollbar position
        """
        if self._smooth_scroll_timer.isActive():
            self._smooth_scroll_timer.stop()

        self._smooth_scroll_start = self._scrollbar.value()
        self._smooth_scroll_target = max(
            self._scrollbar.minimum(), min(self._scrollbar.maximum(), target_value)
        )
        self._smooth_scroll_distance = self._smooth_scroll_target - self._smooth_scroll_start
        self._smooth_scroll_time = 0
        self._smooth_scroll_timer.start()

    def _update_smooth_scroll(self) -> None:
        """Update the smooth scrolling animation."""
        self._smooth_scroll_time += self._smooth_scroll_timer.interval()
        progress = min(1.0, self._smooth_scroll_time / self._smooth_scroll_duration)
        t = 1 - (1 - progress) ** 3

        # Add 0.5 lines of bias so that int() truncation crosses each line boundary
        # slightly early, avoiding a visible "jump" at the very end of the animation
        # where the easing curve decelerates so slowly that the final line is only
        # reached on the last tick, well after the scroll appears to have stopped.
        new_position = min(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t + 0.5),
        ) if self._smooth_scroll_distance > 0 else max(
            self._smooth_scroll_target,
            self._smooth_scroll_start + int(self._smooth_scroll_distance * t - 0.5),
        )
        self._scrollbar.setValue(new_position)
        if progress >= 1.0 or new_position == self._smooth_scroll_target:
            self._smooth_scroll_timer.stop()

    def get_match_status(self) -> Tuple[int, int]:
        """Return (current_1based, total) for the find widget status label."""
        total = len(self._find_matches)
        if total == 0:
            return 0, 0

        return self._find_current + 1, total

    def clear_find(self) -> None:
        """Clear all find state and remove highlights from both panes."""
        self._find_matches = []
        self._find_current = -1
        self._find_text = ""
        self._find_key = ("", False, False)
        self._left_pane.clear_find()
        self._right_pane.clear_find()

    def clear_highlights(self) -> None:
        """Remove find highlights without resetting match state."""
        self._left_pane.clear_find()
        self._right_pane.clear_find()

    def get_selected_text(self) -> str:
        """Return the selected text from whichever pane has an active selection.

        If neither pane has a selection, returns an empty string.  If both
        panes somehow have a selection, the left pane takes priority.
        """
        for pane in (self._left_pane, self._right_pane):
            cursor = pane.textCursor()
            if cursor.hasSelection():
                return cursor.selectedText().replace('\u2029', '\n')

        return ""

    def copy(self) -> None:
        """Copy the selected text from whichever pane has an active selection."""
        for pane in (self._left_pane, self._right_pane):
            cursor = pane.textCursor()
            if cursor.hasSelection():
                pane.copy()
                return

    def can_navigate_next_hunk(self) -> bool:
        """Return True if there is a hunk after the current scroll position."""
        if not self._cached_hunks:
            return False

        if self._current_hunk_index == -1:
            return True

        if self._current_hunk_index < len(self._cached_hunks) - 1:
            return True

        return not self._current_hunk_is_centred()

    def can_navigate_previous_hunk(self) -> bool:
        """Return True if there is a hunk before the current scroll position."""
        if not self._cached_hunks:
            return False

        if self._current_hunk_index > 0:
            return True

        if self._current_hunk_index == -1:
            return False

        return not self._current_hunk_is_centred()

    def navigate_next_hunk(self, forward: bool) -> None:
        """
        Scroll to the first row of the next (or previous) hunk.

        Args:
            forward: If True move to the next hunk; if False move to the previous.
        """
        if not self._cached_hunks:
            return

        if forward:
            if self._current_hunk_index < len(self._cached_hunks) - 1:
                self._current_hunk_index += 1

            elif self._current_hunk_is_centred():
                return

        else:
            if self._current_hunk_index > 0:
                self._current_hunk_index -= 1

            elif self._current_hunk_index == -1 or self._current_hunk_is_centred():
                return

        start, end = self._cached_hunks[self._current_hunk_index]
        self._set_active_hunk(start, end)
        self._start_smooth_scroll(self._left_pane.target_scroll_for_block(start))
        self._hunk_scroll_target = self._smooth_scroll_target

    def _current_hunk_is_centred(self) -> bool:
        """Return True if the scrollbar is already at the centred position for the current hunk."""
        if self._current_hunk_index < 0 or not self._cached_hunks:
            return False

        return self._scrollbar.value() == self._hunk_scroll_target

    def _hunks(self) -> List[Tuple[int, int]]:
        """
        Return (start, end) row index pairs for every hunk, in document order.

        Both indices are inclusive.
        """
        changed_types: Set[DiffRowType] = {DiffRowType.ADDED, DiffRowType.REMOVED, DiffRowType.CHANGED}
        hunks: List[Tuple[int, int]] = []
        hunk_start: int = -1
        prev_was_changed = False
        for i, row in enumerate(self._rows):
            is_changed = row.row_type in changed_types
            if is_changed and not prev_was_changed:
                hunk_start = i

            elif not is_changed and prev_was_changed:
                hunks.append((hunk_start, i - 1))

            prev_was_changed = is_changed

        if prev_was_changed and hunk_start >= 0:
            hunks.append((hunk_start, len(self._rows) - 1))

        return hunks

    def _set_active_hunk(self, start: int, end: int) -> None:
        """Push a hunk range to both panes."""
        self._left_pane.set_active_hunk(start, end)
        self._right_pane.set_active_hunk(start, end)

    def _update_active_hunk(self) -> None:
        """
        Highlight the hunk whose start row is nearest the centre of the viewport.

        Called on every scroll-position change so the gutter colouring always
        reflects what the user is looking at.
        """
        if self._smooth_scroll_timer.isActive():
            return

        if not self._cached_hunks:
            self._set_active_hunk(-1, -1)
            return

        current = self._scrollbar.value()
        visible_lines = (
            self._left_pane.viewport().height()
            // max(1, self._left_pane.fontMetrics().lineSpacing())
        )
        centre = current + visible_lines // 2

        # Pick the hunk whose start is closest to the viewport centre.
        nearest_start, nearest_end = min(self._cached_hunks, key=lambda h: abs(h[0] - centre))
        # Update the tracked index to stay in sync with free-scroll position.
        new_index = next(
            i for i, h in enumerate(self._cached_hunks) if h[0] == nearest_start
        )
        self._current_hunk_index = new_index
        self._set_active_hunk(nearest_start, nearest_end)
