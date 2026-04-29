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

        To determine which item is at virtual position value, we briefly set the tree
        to value (no breadcrumb offset), read the topmost item, then restore the tree
        to value + bc_h.  This correctly identifies the item at the virtual position
        regardless of how tall the breadcrumb bar is.

        Args:
            value: New scrollbar position in tree-internal pixel units.
        """
        tree_sb = self._tree_view.verticalScrollBar()

        # Briefly position the tree at the virtual position to identify the topmost item.
        tree_sb.setValue(value)
        index = self._tree_view.indexAt(QPoint(0, 0))

        # Restore the tree to the offset position.
        bc_h = self._breadcrumb_rows * self._row_height
        tree_sb.setValue(value + bc_h)

        if not index.isValid():
            return

        row_height = self._tree_view.rowHeight(index)
        if row_height <= 0:
            return

        self._row_height = row_height
        topmost_path = self._tree_view.get_path_from_index(index) or ""
        topmost_is_expanded = self._tree_view.isExpanded(index)

        if os.path.isdir(topmost_path) and topmost_is_expanded:
            spine_path = topmost_path
        else:
            parent = os.path.dirname(topmost_path)
            spine_path = parent if parent and parent != topmost_path else ""

        if spine_path == self._last_spine_path:
            return

        self._last_spine_path = spine_path
        self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(spine_path)
        self._tree_view.verticalScrollBar().setValue(value + self._breadcrumb_rows * self._row_height)
        self._apply_geometry()

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
