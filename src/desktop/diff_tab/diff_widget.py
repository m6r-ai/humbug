"""Diff widget supporting inline and side-by-side layouts."""

import logging
import os

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QScrollBar, QSplitter, QLabel, QSizePolicy
from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtGui import QKeyEvent, QResizeEvent

from diff import DiffParser, DiffParseError
from diff.diff_types import DiffHunk

from git import GitCommandError, GitNotFoundError, GitRepository, find_repo_root

from syntax import ProgrammingLanguageUtils

from desktop.style_manager import StyleManager
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.language.language_manager import LanguageManager
from desktop.diff_tab.diff_pane import DiffPane
from desktop.diff_tab.diff_row import DiffRow, DiffRowType, DiffViewMode
from desktop.diff_tab.diff_view_builder import DiffViewBuilder
from desktop.widgets import SMOOTH_SCROLL_DURATION_MS, SMOOTH_SCROLL_INTERVAL_MS


class DiffWidget(QWidget):
    """
    Widget displaying a diff between the working tree and HEAD.

    In SIDE_BY_SIDE mode, two DiffPane instances are arranged in a QSplitter with
    a single external QScrollBar driving both panes' vertical position
    simultaneously.  Horizontal scrolling is independent per pane.

    In INLINE mode, a single DiffPane displays a unified diff.  The pane's own
    vertical scrollbar is visible and no external scrollbar is used.

    Both the HEAD version and the working-tree version of the file are loaded in
    full so that syntax highlighting can process each pane's document from top to
    bottom with correct parser-state propagation.

    When the file has no differences against HEAD, or when git is unavailable,
    an appropriate message is displayed instead of the panes.
    """

    status_updated = Signal()
    mode_changed = Signal()
    open_in_editor_requested = Signal(int, int)
    open_in_preview_requested = Signal()

    def __init__(self, path: str, parent: QWidget | None = None, mode: DiffViewMode = DiffViewMode.INLINE) -> None:
        """
        Initialise the diff widget for the given file path.

        Args:
            path: Absolute path to the file to diff.
            parent: Optional parent widget.
            mode: Initial layout mode — INLINE (narrow, default) or SIDE_BY_SIDE.
        """
        super().__init__(parent)
        self._logger = logging.getLogger("DiffWidget")
        self._path = path
        self._mode = mode
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self.setObjectName("DiffWidget")
        self._rows: list[DiffRow] = []
        self._syncing = False
        self._cached_hunks: list[tuple[int, int]] = []
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

        # Pane area: built differently depending on mode.
        self._pane_container = QWidget()
        pane_layout = QHBoxLayout(self._pane_container)
        pane_layout.setContentsMargins(0, 0, 0, 0)
        pane_layout.setSpacing(0)

        self._splitter: QSplitter | None = None
        self._left_pane: DiffPane | None = None
        self._right_pane: DiffPane | None = None
        self._scrollbar: QScrollBar | None = None
        self._inline_pane: DiffPane | None = None

        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            self._build_side_by_side(pane_layout)

        else:
            self._build_inline(pane_layout)

        outer_layout.addWidget(self._pane_container)

        # Find state: flat list of (pane_id, start, end) tuples across all panes,
        # ordered by row position so navigation feels natural.
        self._find_matches: list[tuple[str, int, int]] = []  # (pane_id, start, end)
        self._find_current: int = -1
        self._find_text: str = ""
        self._find_key: tuple = ("", False, False)

        # Smooth scrolling
        self._smooth_scroll_timer = QTimer(self)
        self._smooth_scroll_timer.setInterval(SMOOTH_SCROLL_INTERVAL_MS)
        self._smooth_scroll_timer.timeout.connect(self._update_smooth_scroll)

        self._deferred_scroll_timer = QTimer(self)
        self._deferred_scroll_timer.setSingleShot(True)
        self._deferred_scroll_timer.setInterval(0)
        self._deferred_scroll_timer.timeout.connect(self._on_deferred_scroll)
        self._deferred_scroll_target: int = 0

        self._restore_scroll_timer = QTimer(self)
        self._restore_scroll_timer.setSingleShot(True)
        self._restore_scroll_timer.setInterval(0)
        self._restore_scroll_timer.timeout.connect(self._on_restore_scroll)
        self._restore_scroll_value: int = 0

        # Scroll centre preservation across style/zoom changes
        self._centre_block_before_style: int | None = None

        self._smooth_scroll_target: int = 0
        self._smooth_scroll_start: int = 0
        self._smooth_scroll_distance: int = 0
        self._smooth_scroll_duration: int = SMOOTH_SCROLL_DURATION_MS
        self._smooth_scroll_time: int = 0

    def _build_side_by_side(self, pane_layout: QHBoxLayout) -> None:
        """Build the side-by-side layout with splitter and shared scrollbar."""
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

        # Wire up scroll sync.
        self._left_pane.verticalScrollBar().valueChanged.connect(self._on_left_scrolled)
        self._right_pane.verticalScrollBar().valueChanged.connect(self._on_right_scrolled)
        self._scrollbar.valueChanged.connect(self._on_shared_scrollbar_moved)

        # Wire pane open requests up to widget-level signals.
        self._left_pane.open_in_editor_requested.connect(self._on_left_pane_open_in_editor_requested)
        self._right_pane.open_in_editor_requested.connect(self._on_right_pane_open_in_editor_requested)
        self._left_pane.open_in_preview_requested.connect(self.open_in_preview_requested)
        self._right_pane.open_in_preview_requested.connect(self.open_in_preview_requested)

        # Keep the shared scrollbar range in sync with the left pane's range
        # (both panes always have the same row count so either would do).
        self._left_pane.verticalScrollBar().rangeChanged.connect(self._on_scroll_range_changed)
        self._scrollbar.valueChanged.connect(self._update_active_hunk)

        # Wire context menu toggle.
        toggle_label = self._language_manager.strings().diff_show_inline
        self._left_pane.set_toggle_mode_label(toggle_label)
        self._right_pane.set_toggle_mode_label(toggle_label)
        self._left_pane.toggle_view_mode_requested.connect(self._defer_switch_to_inline)
        self._right_pane.toggle_view_mode_requested.connect(self._defer_switch_to_inline)

    def _build_inline(self, pane_layout: QHBoxLayout) -> None:
        """Build the single-pane inline layout."""
        self._inline_pane = DiffPane(show_scrollbar=True)
        pane_layout.addWidget(self._inline_pane)

        self._inline_pane.open_in_editor_requested.connect(self._on_inline_pane_open_in_editor_requested)
        self._inline_pane.open_in_preview_requested.connect(self.open_in_preview_requested)
        self._inline_pane.verticalScrollBar().valueChanged.connect(self._update_active_hunk)

        # Wire context menu toggle.
        self._inline_pane.set_toggle_mode_label(self._language_manager.strings().diff_show_side_by_side)
        self._inline_pane.toggle_view_mode_requested.connect(self._defer_switch_to_side_by_side)

    def _defer_switch_to_inline(self) -> None:
        """Defer the mode switch so the context menu event loop can exit first."""
        QTimer.singleShot(0, lambda: self.set_mode(DiffViewMode.INLINE))

    def _defer_switch_to_side_by_side(self) -> None:
        """Defer the mode switch so the context menu event loop can exit first."""
        QTimer.singleShot(0, lambda: self.set_mode(DiffViewMode.SIDE_BY_SIDE))

    def mode(self) -> DiffViewMode:
        """Return the current layout mode."""
        return self._mode

    def set_mode(self, mode: DiffViewMode) -> None:
        """
        Switch the layout mode, rebuilding the pane layout.

        Reloads the diff content into the new layout and restores the scroll
        position.

        Args:
            mode: The new layout mode.
        """
        if mode == self._mode:
            return

        # Cancel any pending deferred callbacks that reference the panes
        # we are about to destroy.
        self._restore_scroll_timer.stop()
        self._deferred_scroll_timer.stop()
        self._smooth_scroll_timer.stop()

        # Invalidate any pending single-shot _restore_centre_block callback.
        self._centre_block_before_style = None

        # Capture scroll position before tearing down.
        if self._scrollbar is not None:
            saved_scroll = self._scrollbar.value()

        elif self._inline_pane is not None:
            saved_scroll = self._inline_pane.verticalScrollBar().value()

        else:
            saved_scroll = 0

        # Tear down old layout.  We null the Python references first so
        # any Qt events triggered during teardown cannot reach deleted
        # objects through our accessors (_primary_pane, _all_panes, etc.).
        old_splitter, self._splitter = self._splitter, None
        old_scrollbar, self._scrollbar = self._scrollbar, None
        old_left, self._left_pane = self._left_pane, None
        old_right, self._right_pane = self._right_pane, None
        old_inline, self._inline_pane = self._inline_pane, None

        for old_widget in (old_splitter, old_scrollbar, old_left, old_right, old_inline):
            if old_widget is not None:
                old_widget.setParent(None)
                old_widget.deleteLater()

        self._mode = mode

        # Build new layout.
        pane_layout = self._pane_container.layout()
        assert isinstance(pane_layout, QHBoxLayout)

        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            self._build_side_by_side(pane_layout)

        else:
            self._build_inline(pane_layout)

        # Clear find state since panes were rebuilt.
        self._find_matches = []
        self._find_current = -1

        # Reload content into the new panes.
        self.load_diff()

        # Restore scroll position.
        scrollbar = self._scrollbar if self._scrollbar is not None else self._primary_pane().verticalScrollBar()
        scrollbar.setValue(saved_scroll)

        self.mode_changed.emit()

    def _on_left_pane_open_in_editor_requested(self, block_number: int, column: int) -> None:
        """
        Translate a left-pane editor request to a working-tree line number and emit.

        The left pane shows the HEAD version.  Since the editor opens the working-tree
        file, we map the clicked row to its right_line_no.  When that row has no
        corresponding right-side line (a pure removal), we walk forward to find the
        nearest row that does.

        Args:
            block_number: Zero-based block index of the clicked row in the left pane.
            column: One-based column position of the click within the block.
        """
        line = self._nearest_right_line_no(block_number)
        self.open_in_editor_requested.emit(line, column)

    def _on_right_pane_open_in_editor_requested(self, block_number: int, column: int) -> None:
        """
        Translate a right-pane editor request to a working-tree line number and emit.

        Args:
            block_number: Zero-based block index of the clicked row in the right pane.
            column: One-based column position of the click within the block.
        """
        line = self._nearest_right_line_no(block_number)
        self.open_in_editor_requested.emit(line, column)

    def _on_inline_pane_open_in_editor_requested(self, block_number: int, column: int) -> None:
        """
        Translate an inline-pane editor request to a working-tree line number and emit.

        Args:
            block_number: Zero-based block index of the clicked row in the inline pane.
            column: One-based column position of the click within the block.
        """
        line = self._nearest_right_line_no(block_number)
        self.open_in_editor_requested.emit(line, column)

    def _nearest_right_line_no(self, block_number: int) -> int:
        """
        Return the working-tree (right-side) line number for the given row index.

        If the row at *block_number* has no right_line_no (a pure removal), walk
        forward through subsequent rows until one is found.  Falls back to 1.

        Args:
            block_number: Zero-based row index into self._rows.

        Returns:
            A 1-based line number for the working-tree file.
        """
        for i in range(block_number, len(self._rows)):
            line_no = self._rows[i].right_line_no
            if line_no is not None:
                return line_no

        return 1

    def load_diff(self, initial_load: bool = False) -> None:
        """
        Fetch both file versions and display the full diff.

        Args:
            initial_load: If True, scroll to the first hunk after loading.
        """
        # Capture the current scroll position before rebuilding the document so
        # we can restore it afterwards on a refresh (i.e. when initial_load is
        # False).  The value is meaningless on a true initial load (it will be 0)
        # but we read it unconditionally to keep the code simple.
        scrollbar = self._scrollbar if self._scrollbar is not None else (
            self._inline_pane.verticalScrollBar() if self._inline_pane is not None else None
        )
        saved_scroll = scrollbar.value() if scrollbar is not None else 0

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
        self._rows = builder.build(old_lines, new_lines, hunks, mode=self._mode)

        language = ProgrammingLanguageUtils.from_file_extension(self._path)

        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            assert self._left_pane is not None and self._right_pane is not None
            # Load rows first so every block has its _BlockData attached before the
            # highlighter runs.  set_syntax() triggers a full rehighlight, by which
            # point all blocks carry the metadata the highlighter needs.
            self._left_pane.load_rows(self._rows, use_left=True)
            self._right_pane.load_rows(self._rows, use_left=False)
            self._left_pane.set_syntax(language)
            self._right_pane.set_syntax(language)

        else:
            assert self._inline_pane is not None
            self._inline_pane.load_rows_inline(self._rows)
            self._inline_pane.set_syntax(language)

        self._show_panes()
        self.status_updated.emit()
        self._cached_hunks = self._hunks()
        self._current_hunk_index = -1
        self._update_active_hunk()

        if not initial_load:
            # Restore the scroll position we had before the reload.  Use the
            # deferred timer so the target is clamped against the updated
            # scrollbar range (which Qt finalises after the document layout pass).
            self._restore_scroll_value = saved_scroll
            self._restore_scroll_timer.start()

        if initial_load and self._cached_hunks:
            start = self._cached_hunks[0][0]
            self._current_hunk_index = 0
            self._set_active_hunk(self._cached_hunks[0][0], self._cached_hunks[0][1])
            self._deferred_scroll_target = self._primary_pane().target_scroll_for_block(start)
            self._deferred_scroll_timer.start()

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

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        if self._scrollbar is not None:
            self._update_shared_scrollbar()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle keyboard scrolling for the shared scrollbar."""
        if self._scrollbar is None:
            super().keyPressEvent(event)
            return

        key = event.key()
        scroll_bar = self._scrollbar

        if event.modifiers() & (Qt.KeyboardModifier.AltModifier | Qt.KeyboardModifier.ControlModifier |
                                Qt.KeyboardModifier.ShiftModifier):
            super().keyPressEvent(event)
            return

        if key == Qt.Key.Key_Up:
            scroll_bar.setValue(scroll_bar.value() - scroll_bar.singleStep())
            event.accept()

        elif key == Qt.Key.Key_Down:
            scroll_bar.setValue(scroll_bar.value() + scroll_bar.singleStep())
            event.accept()

        elif key == Qt.Key.Key_PageUp:
            scroll_bar.setValue(scroll_bar.value() - scroll_bar.pageStep())
            event.accept()

        elif key == Qt.Key.Key_PageDown:
            scroll_bar.setValue(scroll_bar.value() + scroll_bar.pageStep())
            event.accept()

        else:
            super().keyPressEvent(event)

    def _primary_pane(self) -> DiffPane:
        """Return the primary pane for the current mode (left in side-by-side, inline otherwise)."""
        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            assert self._left_pane is not None
            return self._left_pane

        assert self._inline_pane is not None
        return self._inline_pane

    def _all_panes(self) -> list[DiffPane]:
        """Return all active panes for the current mode."""
        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            assert self._left_pane is not None and self._right_pane is not None
            return [self._left_pane, self._right_pane]

        assert self._inline_pane is not None
        return [self._inline_pane]

    def _fetch_content(self) -> tuple[list[str], list[str], str] | None:
        """
        Retrieve the HEAD content, working-tree content, and diff text.

        Returns a tuple of (old_lines, new_lines, diff_text), or None if an
        error occurred that has already been surfaced via the message label.
        Old lines are empty for untracked files (no HEAD version exists).
        """
        try:
            mindspace_manager = MindspaceManager()
            mindspace_path = mindspace_manager.mindspace_path()
            repo_root = find_repo_root(self._path, mindspace_path)

            if repo_root is None:
                self._show_message("This file is not inside a git repository.")
                return None

            repo = GitRepository(repo_root)
            diff_text = repo.get_file_diff(self._path)

            head_content = repo.get_file_at_head(self._path)
            old_lines = head_content.splitlines() if head_content is not None else []

            with open(self._path, encoding="utf-8", errors="replace") as f:
                new_lines = f.read().splitlines()

            return old_lines, new_lines, diff_text

        except GitNotFoundError:
            self._show_message("git is not available on this system.")
            return None

        except GitCommandError as e:
            self._logger.error("git error for '%s': %s", self._path, e)
            self._show_message(f"git error: {e}")
            return None

        except OSError as e:
            self._logger.error("OS error reading '%s': %s", self._path, e)
            self._show_message(f"Could not read file: {e}")
            return None

    def _parse_diff(self, diff_text: str) -> list[DiffHunk] | None:
        """
        Parse diff text into hunks.

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

        assert self._right_pane is not None and self._scrollbar is not None
        self._syncing = True
        self._right_pane.verticalScrollBar().setValue(value)
        self._scrollbar.setValue(value)
        self._syncing = False

    def _on_right_scrolled(self, value: int) -> None:
        """Propagate right-pane scroll to the left pane and shared scrollbar."""
        if self._syncing:
            return

        assert self._left_pane is not None and self._scrollbar is not None
        self._syncing = True
        self._left_pane.verticalScrollBar().setValue(value)
        self._scrollbar.setValue(value)
        self._syncing = False

    def _on_shared_scrollbar_moved(self, value: int) -> None:
        """Propagate shared scrollbar movement to both panes."""
        if self._syncing:
            return

        assert self._left_pane is not None and self._right_pane is not None
        self._syncing = True
        self._left_pane.verticalScrollBar().setValue(value)
        self._right_pane.verticalScrollBar().setValue(value)
        self._syncing = False

    def _on_scroll_range_changed(self, minimum: int, maximum: int) -> None:
        """Keep the shared scrollbar range in sync with the pane content."""
        assert self._scrollbar is not None and self._left_pane is not None
        self._scrollbar.setRange(minimum, maximum)
        self._scrollbar.setPageStep(self._left_pane.verticalScrollBar().pageStep())

    def _update_shared_scrollbar(self) -> None:
        """Refresh the shared scrollbar range and page step."""
        assert self._left_pane is not None and self._scrollbar is not None
        vbar = self._left_pane.verticalScrollBar()
        self._scrollbar.setRange(vbar.minimum(), vbar.maximum())
        self._scrollbar.setPageStep(vbar.pageStep())
        self._scrollbar.setValue(vbar.value())

    def apply_style(self) -> None:
        """Apply current style settings."""
        # Panes may be momentarily absent during a mode switch teardown.
        if self._left_pane is None and self._inline_pane is None:
            return

        # Capture the centre block before the panes' fonts change so we can
        # restore it to the midpoint after async re-layout.
        primary = self._primary_pane()
        visible_lines = primary.viewport().height() // max(1, primary.fontMetrics().lineSpacing())

        scrollbar = self._scrollbar if self._scrollbar is not None else primary.verticalScrollBar()
        self._centre_block_before_style = scrollbar.value() + visible_lines // 2

        base_size = self._style_manager.base_font_size()
        zoom = self._style_manager.zoom_factor()

        label_font = self.font()
        label_font.setPointSizeF(base_size * zoom)
        self._message_label.setFont(label_font)

        for pane in self._all_panes():
            pane.apply_style()

        # Re-layout from setFont() is async, so defer the scroll restoration.
        QTimer.singleShot(0, self._restore_centre_block)

    def _restore_centre_block(self) -> None:
        """Scroll so that the pre-style centre block sits at the viewport midpoint."""
        if self._centre_block_before_style is None:
            return

        primary = self._primary_pane()
        scrollbar = self._scrollbar if self._scrollbar is not None else primary.verticalScrollBar()
        visible_lines = primary.viewport().height() // max(1, primary.fontMetrics().lineSpacing())
        target = max(scrollbar.minimum(), min(scrollbar.maximum(),
                     self._centre_block_before_style - visible_lines // 2))
        scrollbar.setValue(target)
        self._centre_block_before_style = None

    def find_text(
        self, text: str, forward: bool = True, case_sensitive: bool = False, regexp: bool = False
    ) -> tuple[int, int, bool]:
        """
        Search for *text* across all panes and navigate to the next match.

        Matches from all panes are merged in document order (by character
        position) so that navigation follows the visual top-to-bottom flow of
        the diff.  All panes are highlighted simultaneously: the active match is
        bright, all others are dim.

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
                pane_ids: list[tuple[str, DiffPane]]
                if self._mode == DiffViewMode.SIDE_BY_SIDE:
                    assert self._left_pane is not None and self._right_pane is not None
                    pane_ids = [("left", self._left_pane), ("right", self._right_pane)]

                else:
                    assert self._inline_pane is not None
                    pane_ids = [("inline", self._inline_pane)]

                for pane_id, pane in pane_ids:
                    matches = pane.find_matches(text, case_sensitive, regexp)
                    for start, end in matches:
                        self._find_matches.append((pane_id, start, end))

                # Sort by the block number of the match start so that matches
                # appear in visual order.  In side-by-side mode, left/right
                # matches on the same row appear together, left first.
                panes_by_id = dict(pane_ids)

                def _sort_key(item: tuple[str, int, int]) -> tuple[int, int]:
                    pane_id, start, _end = item
                    pane = panes_by_id[pane_id]
                    block = pane.document().findBlock(start)
                    order = {"left": 0, "inline": 0, "right": 1}
                    return (block.blockNumber(), order[pane_id])

                self._find_matches.sort(key=_sort_key)

        if not self._find_matches:
            for pane in self._all_panes():
                pane.clear_find()

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
        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            if pane_id == "left":
                assert self._left_pane is not None
                pane = self._left_pane

            else:
                assert self._right_pane is not None
                pane = self._right_pane

        else:
            assert self._inline_pane is not None
            pane = self._inline_pane

        self._start_smooth_scroll(pane.target_scroll_for_match(start))

    def _apply_highlights(self) -> None:
        """Repaint all match highlights in all panes."""
        if self._mode == DiffViewMode.SIDE_BY_SIDE:
            assert self._left_pane is not None and self._right_pane is not None
            left_matches = [(s, e) for p, s, e in self._find_matches if p == "left"]
            right_matches = [(s, e) for p, s, e in self._find_matches if p == "right"]

            pane_id = self._find_matches[self._find_current][0] if self._find_current != -1 else ""
            left_current_local = -1
            right_current_local = -1
            if pane_id == "left":
                left_current_local = sum(1 for p, s, e in self._find_matches[:self._find_current] if p == "left")

            elif pane_id == "right":
                right_current_local = sum(1 for p, s, e in self._find_matches[:self._find_current] if p == "right")

            self._left_pane.highlight_matches(left_matches, left_current_local)
            self._right_pane.highlight_matches(right_matches, right_current_local)

        else:
            assert self._inline_pane is not None
            inline_matches = [(s, e) for p, s, e in self._find_matches if p == "inline"]
            inline_current = self._find_current if self._find_current != -1 else -1
            self._inline_pane.highlight_matches(inline_matches, inline_current)

    def _on_deferred_scroll(self) -> None:
        """Fire the deferred smooth scroll to the stored target position."""
        self._start_smooth_scroll(self._deferred_scroll_target)

    def _on_restore_scroll(self) -> None:
        """Restore the scroll position saved before a diff reload."""
        scrollbar = self._scrollbar if self._scrollbar is not None else self._primary_pane().verticalScrollBar()
        scrollbar.setValue(max(scrollbar.minimum(), min(scrollbar.maximum(), self._restore_scroll_value)))

    def _start_smooth_scroll(self, target_value: int) -> None:
        """
        Start smooth scrolling animation to target value.

        Args:
            target_value: Target scrollbar position
        """
        scrollbar = self._scrollbar if self._scrollbar is not None else self._primary_pane().verticalScrollBar()

        if self._smooth_scroll_timer.isActive():
            self._smooth_scroll_timer.stop()

        self._smooth_scroll_start = scrollbar.value()
        self._smooth_scroll_target = max(
            scrollbar.minimum(), min(scrollbar.maximum(), target_value)
        )
        self._smooth_scroll_distance = self._smooth_scroll_target - self._smooth_scroll_start
        self._smooth_scroll_time = 0
        self._smooth_scroll_timer.start()

    def _update_smooth_scroll(self) -> None:
        """Update the smooth scrolling animation."""
        scrollbar = self._scrollbar if self._scrollbar is not None else self._primary_pane().verticalScrollBar()
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
        scrollbar.setValue(new_position)
        if progress >= 1.0 or new_position == self._smooth_scroll_target:
            self._smooth_scroll_timer.stop()

    def get_match_status(self) -> tuple[int, int, bool]:
        """Return (current_1based, total) for the find widget status label."""
        total = len(self._find_matches)
        if total == 0:
            return 0, 0, False

        return self._find_current + 1, total, total == 500

    def clear_find(self) -> None:
        """Clear all find state and remove highlights from all panes."""
        self._find_matches = []
        self._find_current = -1
        self._find_text = ""
        self._find_key = ("", False, False)
        for pane in self._all_panes():
            pane.clear_find()

    def clear_highlights(self) -> None:
        """Remove find highlights without resetting match state."""
        for pane in self._all_panes():
            pane.clear_find()

    def get_selected_text(self) -> str:
        """
        Return the selected text from whichever pane has an active selection.

        If no pane has a selection, returns an empty string.  In side-by-side
        mode, the left pane takes priority.
        """
        for pane in self._all_panes():
            cursor = pane.textCursor()
            if cursor.hasSelection():
                return cursor.selectedText().replace('\u2029', '\n')

        return ""

    def copy(self) -> None:
        """Copy the selected text from whichever pane has an active selection."""
        for pane in self._all_panes():
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
        self._start_smooth_scroll(self._primary_pane().target_scroll_for_block(start))
        self._hunk_scroll_target = self._smooth_scroll_target

    def _current_hunk_is_centred(self) -> bool:
        """Return True if the scrollbar is already at the centred position for the current hunk."""
        if self._current_hunk_index < 0 or not self._cached_hunks:
            return False

        scrollbar = self._scrollbar if self._scrollbar is not None else self._primary_pane().verticalScrollBar()
        return scrollbar.value() == self._hunk_scroll_target

    def _hunks(self) -> list[tuple[int, int]]:
        """
        Return (start, end) row index pairs for every hunk, in document order.

        Both indices are inclusive.
        """
        changed_types: set[DiffRowType] = {DiffRowType.ADDED, DiffRowType.REMOVED, DiffRowType.CHANGED}
        hunks: list[tuple[int, int]] = []
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
        """Push a hunk range to all panes."""
        for pane in self._all_panes():
            pane.set_active_hunk(start, end)

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

        scrollbar = self._scrollbar if self._scrollbar is not None else self._primary_pane().verticalScrollBar()
        primary = self._primary_pane()
        current = scrollbar.value()
        visible_lines = (
            primary.viewport().height()
            // max(1, primary.fontMetrics().lineSpacing())
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
