"""Container that coordinates the breadcrumb bar and tree view geometry."""

import os

from PySide6.QtCore import QEvent, QObject, QPoint, QRect, QSize, Qt
from PySide6.QtWidgets import QApplication, QScrollBar, QSizePolicy, QWidget

from humbug.mindspace.mindspace_breadcrumb_bar import MindspaceBreadcrumbBar
from humbug.mindspace.mindspace_tree_view import MindspaceTreeView


class MindspaceBreadcrumbContainer(QWidget):
    """
    A container that coordinates the breadcrumb bar and tree view as a single unit.

    Responsibilities:
    - Geometry: positions the breadcrumb bar above the tree view, with an external
      scrollbar to the right.
    - Scroll ownership: owns a single external QScrollBar that is the sole authority
      over scroll position.  The tree view's internal scrollbar is hidden and driven
      as a slave.  Wheel events on the tree viewport are intercepted and forwarded to
      the external scrollbar so that all input flows through one path.
    - Breadcrumb updates: on each scroll tick, reads the item at virtual position
      `value` in the content and updates the breadcrumb bar to reflect the current
      ancestor context.  The tree view is always set to `value + breadcrumb_height`
      so the breadcrumb bar rows are never visible inside the tree viewport.

    None of this logic belongs in the individual child widgets, which remain unaware
    of each other.
    """

    def __init__(
        self,
        breadcrumb_bar: MindspaceBreadcrumbBar,
        tree_view: MindspaceTreeView,
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

        self._last_spine_path: str = ""
        self._breadcrumb_rows: int = 0
        self._row_height: int = 0

        breadcrumb_bar.setParent(self)
        tree_view.setParent(self)

        tree_view.setMinimumSize(QSize(0, 0))
        tree_view.setMaximumSize(QSize(16777215, 16777215))  # QWIDGETSIZE_MAX
        tree_view.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)
        tree_view.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        self._scrollbar = QScrollBar(Qt.Orientation.Vertical, self)
        self._scrollbar.valueChanged.connect(self._on_external_scroll)

        tree_view.verticalScrollBar().rangeChanged.connect(self._on_tree_range_changed)
        tree_view.viewport().installEventFilter(self)

    def set_root_path(self, root_path: str) -> None:
        """
        Set the root path used when populating the breadcrumb bar.

        Args:
            root_path: Absolute path of the tree root (mindspace or conversations root).
        """

    def sizeHint(self) -> QSize:
        """Return a zero size hint — the parent layout expands us via size policy."""
        return QSize(0, 0)

    def minimumSizeHint(self) -> QSize:
        """Return a zero minimum so the layout is free to size us as needed."""
        return QSize(0, 0)

    def resizeEvent(self, event) -> None:  # type: ignore[override]
        """Redistribute geometry when the container is resized."""
        super().resizeEvent(event)
        self._apply_geometry()

    def eventFilter(self, obj: QObject, event: QEvent) -> bool:
        """
        Forward wheel events from the tree viewport to the external scrollbar.

        Args:
            obj: The object that received the event.
            event: The event to filter.

        Returns:
            True if the event was consumed, False to pass it on.
        """
        if obj is self._tree_view.viewport() and event.type() == QEvent.Type.Wheel:
            QApplication.sendEvent(self._scrollbar, event)
            return True

        return super().eventFilter(obj, event)

    def _on_tree_range_changed(self, minimum: int, maximum: int) -> None:
        """
        Mirror the tree's scroll range and page step directly onto the external scrollbar.

        Args:
            minimum: New minimum value from the tree's internal scrollbar.
            maximum: New maximum value from the tree's internal scrollbar.
        """
        tree_sb = self._tree_view.verticalScrollBar()
        self._scrollbar.setRange(minimum, maximum)
        self._scrollbar.setPageStep(tree_sb.pageStep())
        self._scrollbar.setSingleStep(tree_sb.singleStep())

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
        tree_sb = self._tree_view.verticalScrollBar()
        bc_h = self._breadcrumb_rows * self._row_height
        tree_sb.setValue(value + bc_h)

        index = self._tree_view.indexAt(QPoint(0, 0))
        if not index.isValid():
            return

        row_height = self._tree_view.rowHeight(index)
        if row_height <= 0:
            return

        self._row_height = row_height
        topmost_path = self._tree_view.get_path_from_index(index) or ""
        topmost_is_expanded = self._tree_view.isExpanded(index)

        # If any ancestor of the first visible item (at the same depth as the second
        # visible item) is a sibling of the second visible item, the viewport is
        # straddling a sibling boundary — the spine should be their common parent.
        second_index = self._tree_view.indexAt(QPoint(0, row_height))
        second_path = (self._tree_view.get_path_from_index(second_index) or "") if second_index.isValid() else ""

        print(f"TICK value={value} bc_rows={self._breadcrumb_rows} first={os.path.basename(topmost_path)} second={os.path.basename(second_path) if second_path else ''}")

        straddling_sibling_boundary = False
        if second_path and second_path != topmost_path:
            second_parent = os.path.dirname(second_path)
            # Walk up the ancestor chain of topmost_path looking for a sibling of second_path.
            ancestor = topmost_path
            while ancestor:
                if os.path.dirname(ancestor) == second_parent and ancestor != second_path:
                    straddling_sibling_boundary = True
                    break
                parent_of_ancestor = os.path.dirname(ancestor)
                if parent_of_ancestor == ancestor:
                    break
                ancestor = parent_of_ancestor

        print(f"  straddling={straddling_sibling_boundary} spine_would_be={'(unlatch)' if topmost_path == self._last_spine_path else (topmost_path if os.path.isdir(topmost_path) and topmost_is_expanded else os.path.dirname(topmost_path))!r}")

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
            elif os.path.isdir(topmost_path) and topmost_is_expanded:
                spine_path = topmost_path
            else:
                parent = os.path.dirname(topmost_path)
                spine_path = parent if parent and parent != topmost_path else ""

        if spine_path == self._last_spine_path:
            return

        old_spine = self._last_spine_path
        if old_spine and not spine_path.startswith(old_spine):
            print(f"UNLATCH value={value} row_h={self._row_height} bc_rows={self._breadcrumb_rows}: {os.path.basename(old_spine)} -> {os.path.basename(spine_path) if spine_path else 'root'}")
        else:
            print(f"LATCH   value={value} row_h={self._row_height} bc_rows={self._breadcrumb_rows}: {os.path.basename(spine_path) if spine_path else 'root'} (was {os.path.basename(old_spine) if old_spine else 'root'})")

        self._last_spine_path = spine_path
        self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(spine_path)
        self._tree_view.verticalScrollBar().setValue(value + self._breadcrumb_rows * self._row_height)
        self._apply_geometry()
        self._debug_print_visible_items(value)

    def _apply_geometry(self) -> None:
        """Assign geometry to all child widgets to exactly fill the container."""
        w = self.width()
        h = self.height()

        bc_h = self._breadcrumb_rows * self._row_height
        sb_w = self._scrollbar.sizeHint().width()
        tree_w = max(0, w - sb_w)
        tree_h = max(0, h - bc_h)

        self._breadcrumb_bar.setFixedHeight(bc_h)
        self._tree_view.setGeometry(QRect(0, bc_h, tree_w, tree_h))
        self._scrollbar.setGeometry(QRect(tree_w, 0, sb_w, h))

    def _debug_print_visible_items(self, value: int) -> None:
        """Print the content offset of every visible item in the tree viewport."""
        tree_sb = self._tree_view.verticalScrollBar()
        tree_pos = tree_sb.value()
        viewport_h = self._tree_view.viewport().height()
        print(f"  tree_sb={tree_pos} viewport_h={viewport_h} bc_h={self._breadcrumb_rows * self._row_height}")
        print(f"  visible items (content_offset, name):")
        y = 0
        seen = set()
        while y < viewport_h:
            index = self._tree_view.indexAt(QPoint(0, y))
            if not index.isValid():
                break
            path = self._tree_view.get_path_from_index(index) or ""
            if path in seen:
                break
            seen.add(path)
            content_offset = tree_pos + y
            print(f"    y={y:4d} content={content_offset:6d} {os.path.basename(path)!r}")
            rh = self._tree_view.rowHeight(index)
            y += rh if rh > 0 else 24
