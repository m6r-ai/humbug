"""Container that coordinates the breadcrumb bar and tree view geometry."""

import os
import logging
from typing import Callable
from PySide6.QtCore import QEvent, QModelIndex, QObject, QPoint, QRect, QSize, Qt, QTimer
from PySide6.QtGui import QFont, QResizeEvent
from PySide6.QtWidgets import QApplication, QScrollBar, QSizePolicy, QWidget

from desktop.sidebar.sidebar_breadcrumb_bar import SidebarBreadcrumbBar
from desktop.sidebar.sidebar_tree_view import SidebarTreeView


class SidebarBreadcrumbContainer(QWidget):
    """
    A container that coordinates the breadcrumb bar and tree view as a single unit.

    Responsibilities:
    - Geometry: positions the breadcrumb bar above the tree view, with an external
      scrollbar to the right.
    - Scroll ownership: owns a single external QScrollBar that is the sole authority
      over scroll position.  The tree view's internal scrollbar is hidden and driven
      as a slave.  Wheel events on the tree viewport are intercepted and forwarded to
      the external scrollbar so that all input flows through one path.
    - Breadcrumb updates: on each scroll tick, reads the item at the top of the tree
      viewport and updates the breadcrumb bar to reflect the current ancestor context.
      The tree view is always set to `value + breadcrumb_height` so the breadcrumb bar
      rows are never visible inside the tree viewport.

    None of this logic belongs in the individual child widgets, which remain unaware
    of each other.
    """

    def __init__(
        self,
        breadcrumb_bar: SidebarBreadcrumbBar,
        tree_view: SidebarTreeView,
        parent: QWidget | None = None,
    ) -> None:
        """
        Initialise the container.

        Args:
            breadcrumb_bar: The breadcrumb bar widget.
            tree_view: The main file/conversation tree view.
            parent: Optional parent widget.
        """
        super().__init__(parent)

        self._breadcrumb_bar = breadcrumb_bar
        self._tree_view = tree_view

        self._logger = logging.getLogger("SidebarBreadcrumbContainer")

        self._last_spine_path: str = ""
        self._breadcrumb_rows: int = 0
        self._row_height: int = 0
        self._bc_row_height: int = 0
        self._scrolling: bool = False
        self._root_path: str = ""
        self._pending_expand_path: str = ""
        self._dot_click_handler: Callable[[], None] | None = None
        self._dot_double_click_handler: Callable[[], None] | None = None

        self._ballistic_timer = QTimer(self)
        self._ballistic_timer.setInterval(16)  # ~60 fps
        self._ballistic_timer.timeout.connect(self._update_ballistic_scroll)
        self._ballistic_start: int = 0
        self._ballistic_target: int = 0
        self._ballistic_distance: int = 0
        self._ballistic_time: int = 0
        self._ballistic_duration: int = 300  # ms

        self._breadcrumb_refresh_timer = QTimer(self)
        self._breadcrumb_refresh_timer.setSingleShot(True)
        self._breadcrumb_refresh_timer.setInterval(0)
        self._breadcrumb_refresh_timer.timeout.connect(self._on_breadcrumb_refresh)

        self._page_step_timer = QTimer(self)
        self._page_step_timer.setSingleShot(True)
        self._page_step_timer.setInterval(0)
        self._page_step_timer.timeout.connect(self._on_page_step_refresh)

        breadcrumb_bar.setParent(self)
        tree_view.setParent(self)

        tree_view.setMinimumSize(QSize(0, 0))
        tree_view.setMaximumSize(QSize(16777215, 16777215))  # QWIDGETSIZE_MAX
        tree_view.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)
        tree_view.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        self._scrollbar = QScrollBar(Qt.Orientation.Vertical, self)
        self._scrollbar.setRange(0, 0)
        self._scrollbar.valueChanged.connect(self._on_external_scroll)

        tree_view.verticalScrollBar().rangeChanged.connect(self._on_tree_range_changed)
        tree_view.viewport().installEventFilter(self)
        breadcrumb_bar.viewport().installEventFilter(self)
        tree_view.expanded.connect(self._on_tree_expanded)

        breadcrumb_bar.set_collapse_handler(self._on_breadcrumb_collapse)
        breadcrumb_bar.set_scroll_handler(self.scroll_to_path)
        breadcrumb_bar.set_double_click_handler(self._on_breadcrumb_double_clicked)

    def set_dot_click_handler(self, handler: Callable[[], None]) -> None:
        """
        Set a callable invoked when the user clicks the "." breadcrumb item.

        Args:
            handler: Callable to invoke with no arguments.
        """
        self._dot_click_handler = handler

    def set_dot_double_click_handler(self, handler: Callable[[], None]) -> None:
        """
        Set a callable invoked when the user double-clicks the "." breadcrumb item.

        Args:
            handler: Callable to invoke with no arguments.
        """
        self._dot_double_click_handler = handler

    def set_root_path(self, root_path: str) -> None:
        """
        Set the root path used when populating the breadcrumb bar.

        Args:
            root_path: Absolute path of the tree root (mindspace or conversations root).
        """
        self._root_path = root_path
        self._breadcrumb_rows = 0
        self._row_height = 0
        self._bc_row_height = 0
        self._last_spine_path = ""

    def refresh_viewport(self) -> None:
        """
        Repaint both the tree viewport and the breadcrumb bar viewport.

        Use this instead of calling viewport().update() directly on either child
        widget, so that both surfaces are always refreshed together.
        """
        self._tree_view.viewport().update()
        self._breadcrumb_bar.viewport().update()

    def apply_tree_style(self, icon_size: int, font: QFont) -> None:
        """
        Apply icon size, indentation, and font to the tree view.

        Routing these through the container keeps the view layer from holding
        direct references to the tree view for styling purposes.

        Args:
            icon_size: Pixel size for tree icons and indentation.
            font: QFont to apply to the tree view.
        """
        self._tree_view.setIconSize(QSize(icon_size, icon_size))
        self._tree_view.setIndentation(icon_size)
        self._tree_view.setFont(font)

    def configure_tree_for_path(self, path: str) -> None:
        """
        Forward a path configuration request to the tree view.

        Args:
            path: Path to configure the tree view for.
        """
        self._tree_view.configure_for_path(path)

    def sizeHint(self) -> QSize:
        """Return a zero size hint — the parent layout expands us via size policy."""
        return QSize(0, 0)

    def minimumSizeHint(self) -> QSize:
        """Return a zero minimum so the layout is free to size us as needed."""
        return QSize(0, 0)

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Redistribute geometry when the container is resized."""
        super().resizeEvent(event)
        self._apply_geometry()

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """
        Forward wheel events from the tree and breadcrumb viewports to the external scrollbar.

        Args:
            obj: The object that received the event.
            event: The event to filter.

        Returns:
            True if the event was consumed, False to pass it on.
        """
        if event.type() == QEvent.Type.Wheel and obj in (
            self._tree_view.viewport(), self._breadcrumb_bar.viewport()
        ):
            QApplication.sendEvent(self._scrollbar, event)
            return True

        if obj is self._tree_view.viewport() and event.type() == QEvent.Type.Paint:
            if self._row_height == 0:
                index = self._tree_view.indexAt(QPoint(0, 0))
                if index.isValid():
                    rh = self._tree_view.rowHeight(index)
                    if rh > 0:
                        self._row_height = rh
                        bc_index = self._breadcrumb_bar.indexAt(QPoint(0, 0))
                        if bc_index.isValid():
                            self._bc_row_height = self._breadcrumb_bar.rowHeight(bc_index)

                        if self._bc_row_height <= 0:
                            self._bc_row_height = rh

                        self._on_external_scroll(self._scrollbar.value())

        return super().eventFilter(obj, event)

    def _on_tree_range_changed(self, minimum: int, maximum: int) -> None:
        """
        Mirror the tree's scroll range directly onto the external scrollbar.

        Also re-syncs the external scrollbar value to match the tree's actual
        current scroll position.  This is necessary because a model reset
        (beginResetModel/endResetModel) silently resets the tree's internal
        scrollbar to 0 without emitting a value-changed signal, leaving the
        external scrollbar stranded at its previous position.

        Args:
            minimum: New minimum value from the tree's internal scrollbar.
            maximum: New maximum value from the tree's internal scrollbar.
        """
        tree_sb = self._tree_view.verticalScrollBar()
        self._scrollbar.setRange(minimum, maximum)
        page_step = tree_sb.pageStep()
        self._scrollbar.setPageStep(page_step)
        single_step = self._row_height if self._row_height > 0 else tree_sb.singleStep()
        self._scrollbar.setSingleStep(single_step)

        if page_step == 0 and maximum > minimum:
            self._page_step_timer.start()

        bc_h = max(0, self._breadcrumb_rows - 1) * self._row_height
        external_value = max(minimum, tree_sb.value() - bc_h)
        if self._scrollbar.value() != external_value:
            self._scrollbar.setValue(external_value)

        else:
            self._breadcrumb_refresh_timer.start()

    def _on_breadcrumb_refresh(self) -> None:
        """Recalculate the breadcrumb after a tree expand/collapse has settled."""
        self._last_spine_path = ""
        self._on_external_scroll(self._scrollbar.value())

        if self._pending_expand_path:
            path = self._pending_expand_path
            self._pending_expand_path = ""
            index = self._tree_view.index_for_path(path)
            if index.isValid() and self._tree_view.model().rowCount(index) > 0:
                first_child = self._tree_view.model().index(0, 0, index)
                if first_child.isValid():
                    self._tree_view.scrollTo(first_child, self._tree_view.ScrollHint.PositionAtTop)

    def _on_tree_expanded(self, index: QModelIndex) -> None:
        """Record an expansion that happened behind the breadcrumb for deferred scroll."""
        path = self._tree_view.get_path_from_index(index)
        if path and self._tree_view.visualRect(index).top() < 0:
            self._pending_expand_path = path

    def _on_page_step_refresh(self) -> None:
        """
        Re-sync the external scrollbar page step after Qt has completed its layout pass.

        Qt emits rangeChanged before it has finished recalculating pageStep, so the
        value read in _on_tree_range_changed can be 0 even when the viewport is visible.
        This single-shot callback fires on the next event-loop iteration by which time
        the correct pageStep is available.
        """
        tree_sb = self._tree_view.verticalScrollBar()
        page_step = tree_sb.pageStep()
        if page_step > 0:
            self._scrollbar.setPageStep(page_step)

    def _on_external_scroll(self, value: int) -> None:
        """
        Drive the tree view and update the breadcrumb bar from the external scrollbar.

        The external scrollbar value represents a virtual position in the full content.
        The tree is set to show content starting at value + breadcrumb_height, so the
        rows represented by the breadcrumb bar are never visible inside the tree viewport.
        The item at the top of the tree viewport (indexAt(0,0)) is the item that has
        just scrolled under the breadcrumb bar — that is what drives the breadcrumb.

        Args:
            value: New scrollbar position in tree-internal pixel units.
        """
        if self._scrolling:
            return

        self._scrolling = True
        try:
            self._do_external_scroll(value)

        finally:
            self._scrolling = False

    def _do_external_scroll(self, value: int) -> None:
        """Perform the actual scroll update. Called only when not re-entering."""
        tree_sb = self._tree_view.verticalScrollBar()
        # The "." sentinel row is always present in the breadcrumb bar but does
        # not represent a scrolled-away tree row.  Only the real directory rows
        # above it (breadcrumb_rows - 1) contribute to the tree scroll offset.
        bc_h = max(0, self._breadcrumb_rows - 1) * self._row_height
        tree_sb.setValue(value + bc_h)

        # When scrolling fast, the tree viewport may not have settled by the time
        # we read it.  If indexAt returns nothing usable, defer a refresh so the
        # breadcrumb converges to the correct state on the next event-loop tick.
        # Without this, the re-entrancy guard in _on_external_scroll consumes the
        # scroll event and no subsequent breadcrumb update ever runs.
        index = self._tree_view.indexAt(QPoint(0, 0))
        if not index.isValid():
            self._breadcrumb_refresh_timer.start()
            return

        row_height = self._tree_view.rowHeight(index)
        if row_height <= 0:
            self._breadcrumb_refresh_timer.start()
            return

        self._row_height = row_height
        bc_index = self._breadcrumb_bar.indexAt(QPoint(0, 0))
        if bc_index.isValid():
            bc_rh = self._breadcrumb_bar.rowHeight(bc_index)
            if bc_rh > 0:
                self._bc_row_height = bc_rh

        topmost_path = self._tree_view.get_path_from_index(index) or ""
        topmost_is_expanded = self._tree_view.isExpanded(index)

        # If any ancestor of the first visible item (at the same depth as the second
        # visible item) is a sibling of the second visible item, the viewport is
        # straddling a sibling boundary — the spine should be their common parent.
        second_index = self._tree_view.indexAt(QPoint(0, row_height))
        second_path = (self._tree_view.get_path_from_index(second_index) or "") if second_index.isValid() else ""

        straddling_sibling_boundary = False
        if second_path and second_path != topmost_path:
            second_parent = os.path.dirname(second_path)
            ancestor = topmost_path
            while ancestor:
                if os.path.dirname(ancestor) == second_parent and ancestor != second_path:
                    straddling_sibling_boundary = True
                    break

                parent_of_ancestor = os.path.dirname(ancestor)
                if parent_of_ancestor == ancestor:
                    break

                ancestor = parent_of_ancestor

        if straddling_sibling_boundary:
            # Spine is the common parent of the straddled siblings.
            spine_path = os.path.dirname(second_path)
            spine_path = spine_path if spine_path and spine_path != second_path else ""
            # Only apply the straddling result if it doesn't move the spine upward —
            # moving up is handled by the normal unlatch logic.
            if self._last_spine_path and not spine_path.startswith(self._last_spine_path):
                straddling_sibling_boundary = False

        if not straddling_sibling_boundary:
            if topmost_path == self._last_spine_path:
                # Unlatch: the latched folder's header has scrolled back into view.
                parent = os.path.dirname(topmost_path)
                spine_path = parent if parent and parent != topmost_path else ""

            elif os.path.isdir(topmost_path) and topmost_is_expanded and self._tree_view.visualRect(index).top() < 0:
                spine_path = topmost_path

            else:
                parent = os.path.dirname(topmost_path)
                spine_path = parent if parent and parent != topmost_path else ""

        if spine_path == self._last_spine_path:
            return

        self._last_spine_path = spine_path
        self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(spine_path)
        bc_h = max(0, self._breadcrumb_rows - 1) * self._row_height
        self._tree_view.verticalScrollBar().setValue(value + bc_h)
        self._apply_geometry()

        # _apply_geometry resizes the tree viewport, which causes the tree's
        # internal rangeChanged to fire with a new maximum.  Re-read the tree's
        # actual range now and clamp the external scrollbar to it so the
        # external bar never exceeds the tree's real scrollable extent.
        tree_sb = self._tree_view.verticalScrollBar()
        self._scrollbar.setRange(tree_sb.minimum(), tree_sb.maximum())
        self._scrollbar.setPageStep(tree_sb.pageStep())

    def scroll_to_path(self, path: str) -> None:
        """
        Scroll the tree so the given folder's own row is at the top of the viewport.

        Recalculates the breadcrumb to reflect the new context.  This is the same
        action as clicking a breadcrumb row, and can also be called programmatically.

        Args:
            path: Absolute file system path of the folder that was clicked.
        """
        if self._root_path and os.path.normpath(path) == os.path.normpath(self._root_path):
            # "." was clicked — jump straight to the top and reset the breadcrumb.
            if self._dot_click_handler:
                self._dot_click_handler()

            self._last_spine_path = ""
            self._breadcrumb_rows = self._breadcrumb_bar.update_from_path("")
            self._apply_geometry()
            self._start_ballistic_scroll(0)
            return

        index = self._tree_view.index_for_path(path)
        if index.isValid():
            self._tree_view.scrollTo(index, self._tree_view.ScrollHint.PositionAtTop)

        # The folder is now at the top of the viewport so it is no longer scrolled
        # above it — recalculate the breadcrumb to show its parent, exactly as the
        # collapse handler does.  Bypass _on_external_scroll to avoid stale bc_h.
        self._last_spine_path = ""
        new_rows = self._breadcrumb_bar.update_from_path(os.path.dirname(path))
        self._breadcrumb_rows = new_rows
        self._apply_geometry()

        if index.isValid():
            tree_sb = self._tree_view.verticalScrollBar()
            bc_h = max(0, new_rows - 1) * self._row_height
            external_value = max(0, tree_sb.value() - bc_h)
            tree_sb.setValue(external_value + bc_h)
            self._last_spine_path = os.path.dirname(path)
            self._scrollbar.setValue(external_value)

    def _on_breadcrumb_double_clicked(self, path: str) -> None:
        """Handle a double-click on a breadcrumb row."""
        if self._root_path and os.path.normpath(path) == os.path.normpath(self._root_path):
            if self._dot_double_click_handler:
                self._dot_double_click_handler()

        else:
            self.scroll_to_path(path)

    def _on_breadcrumb_collapse(self, path: str) -> None:
        """
        Handle a collapse request from the breadcrumb bar.

        Args:
            path: Absolute file system path of the folder to collapse in the tree.
        """
        index = self._tree_view.collapse_path(path)

        if index.isValid():
            # Scroll the collapsed item to the top of the tree's internal viewport.
            # We do this before recalculating the breadcrumb so the tree is in its
            # final scroll position when we read indexAt(0,0).
            self._tree_view.scrollTo(index, self._tree_view.ScrollHint.PositionAtTop)

        # Recalculate the breadcrumb from the collapsed item's path directly,
        # bypassing _on_external_scroll entirely to avoid stale bc_h arithmetic.
        self._last_spine_path = ""
        new_rows = self._breadcrumb_bar.update_from_path(os.path.dirname(path))
        self._breadcrumb_rows = new_rows
        self._apply_geometry()

        # Now set the tree's internal scrollbar to account for the new breadcrumb
        # height, and sync the external scrollbar to match.
        if index.isValid():
            tree_sb = self._tree_view.verticalScrollBar()
            bc_h = max(0, new_rows - 1) * self._row_height
            self._start_ballistic_scroll(max(0, tree_sb.value() - bc_h))
            self._last_spine_path = os.path.dirname(path)

    def _start_ballistic_scroll(self, target: int) -> None:
        """
        Begin a ballistic (cubic ease-out) animation of the external scrollbar.

        If an animation is already running it is cancelled and a new one starts
        from the current scrollbar position so the motion is always continuous.

        Args:
            target: Destination value for the external scrollbar.
        """
        if self._ballistic_timer.isActive():
            self._ballistic_timer.stop()

        self._ballistic_start = self._scrollbar.value()
        self._ballistic_target = target
        self._ballistic_distance = target - self._ballistic_start
        self._ballistic_time = 0

        if self._ballistic_distance == 0:
            return

        self._ballistic_timer.start()

    def _update_ballistic_scroll(self) -> None:
        """Advance the ballistic scroll animation by one timer tick."""
        self._ballistic_time += self._ballistic_timer.interval()
        progress = min(1.0, self._ballistic_time / self._ballistic_duration)
        t = 1 - (1 - progress) ** 3  # cubic ease-out

        if self._ballistic_distance > 0:
            new_value = min(
                self._ballistic_target,
                self._ballistic_start + int(self._ballistic_distance * t + 0.5),
            )

        else:
            new_value = max(
                self._ballistic_target,
                self._ballistic_start + int(self._ballistic_distance * t - 0.5),
            )

        self._scrollbar.setValue(new_value)
        if progress >= 1.0 or new_value == self._ballistic_target:
            self._ballistic_timer.stop()

    def _apply_geometry(self) -> None:
        """Assign geometry to all child widgets to exactly fill the container."""
        w = self.width()
        h = self.height()
        scrollbar = self._scrollbar

        bc_row_height = self._bc_row_height if self._bc_row_height > 0 else self._row_height
        bc_h = self._breadcrumb_rows * bc_row_height

        # Disconnect rangeChanged while adjusting geometry to prevent the tree's
        # internal range change from causing a spurious external scrollbar value
        # recalculation during an active drag.
        self._tree_view.verticalScrollBar().rangeChanged.disconnect(self._on_tree_range_changed)

        # First pass: lay out with the current scrollbar visibility so the tree
        # gets its new height and can recalculate its internal scroll range.
        scrollbar_needed = scrollbar.maximum() > scrollbar.minimum()
        sb_w = scrollbar.sizeHint().width() if scrollbar_needed else 0
        tree_w = max(0, w - sb_w)
        tree_h = max(0, h - bc_h)
        self._breadcrumb_bar.setFixedHeight(bc_h)
        self._tree_view.setGeometry(QRect(0, bc_h, tree_w, tree_h))

        # After the tree has been resized, re-read its actual internal range.
        # The range may have changed because the viewport height changed (e.g.
        # going in/out of full screen), but rangeChanged was disconnected so we
        # never received the update.  Sync the external scrollbar now so that
        # scrollbar_needed reflects reality.
        tree_sb = self._tree_view.verticalScrollBar()
        self._scrollbar.setRange(tree_sb.minimum(), tree_sb.maximum())
        self._scrollbar.setPageStep(tree_sb.pageStep())

        scrollbar_needed = scrollbar.maximum() > scrollbar.minimum()
        scrollbar.setVisible(scrollbar_needed)
        sb_w = scrollbar.sizeHint().width() if scrollbar_needed else 0
        tree_w = max(0, w - sb_w)
        self._tree_view.setGeometry(QRect(0, bc_h, tree_w, tree_h))
        self._scrollbar.setGeometry(QRect(tree_w, 0, sb_w, h))

        self._tree_view.verticalScrollBar().rangeChanged.connect(self._on_tree_range_changed)
