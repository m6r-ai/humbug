"""Container that coordinates the breadcrumb bar, transition widget, and tree view geometry."""

import os

from PySide6.QtCore import QEvent, QObject, QRect, QSize, Qt
from PySide6.QtWidgets import QApplication, QScrollBar, QSizePolicy, QWidget

from humbug.mindspace.mindspace_breadcrumb_bar import MindspaceBreadcrumbBar
from humbug.mindspace.mindspace_breadcrumb_transition import MindspaceBreadcrumbTransition
from humbug.mindspace.mindspace_tree_view import MindspaceTreeView


class MindspaceBreadcrumbContainer(QWidget):
    """
    A container that coordinates the breadcrumb bar, transition widget, and tree
    view as a single unit.

    Responsibilities:
    - Geometry: manually positions all four widgets (breadcrumb bar, transition,
      tree view, scrollbar) so they fill the container exactly.
    - Scroll ownership: owns a single external QScrollBar that is the sole authority
      over scroll position.  The tree view's internal scrollbar is hidden and driven
      as a slave.  Wheel events on the tree viewport are intercepted and forwarded to
      the external scrollbar so that all input flows through one path.
    - Sibling transition: detects when the topmost visible tree item changes from
      one expanded sibling folder to another, and drives the transition widget to
      animate the departing folder out of view.

    None of this logic belongs in the individual child widgets, which remain unaware
    of each other.
    """

    def __init__(
        self,
        breadcrumb_bar: MindspaceBreadcrumbBar,
        transition: MindspaceBreadcrumbTransition,
        tree_view: MindspaceTreeView,
        parent: QWidget | None = None,
    ) -> None:
        """
        Initialise the container.

        Args:
            breadcrumb_bar: The breadcrumb bar widget.
            transition: The breadcrumb transition widget.
            tree_view: The main file/conversation tree view.
            parent: Optional parent widget.
        """
        super().__init__(parent)

        self._breadcrumb_bar = breadcrumb_bar
        self._transition = transition
        self._tree_view = tree_view

        self._root_path: str = ""

        self._breadcrumb_rows: int = 0
        self._transition_height: int = 0
        self._last_spine_path: str = ""
        self._row_height: int = 0

        # Reference scrollbar value captured when a sibling transition begins.
        # The transition height is a pure function of (current_value - _transition_ref_value).
        self._transition_ref_value: int = 0
        self._transition_max_height: int = 0

        # Re-entrancy guard: set while we are handling an external scroll event so
        # that the internal scrollbar's valueChanged (fired by setValue below) does
        # not recurse back into _on_external_scroll via _on_tree_internal_scroll.
        self._handling_scroll: bool = False

        # Re-parent all three widgets into this container.
        breadcrumb_bar.setParent(self)
        transition.setParent(self)
        tree_view.setParent(self)

        # Breadcrumb and transition are fixed-height, self-reporting via sizeHint.
        breadcrumb_bar.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        transition.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

        # Tree view fills remaining space; geometry managed exclusively via setGeometry.
        tree_view.setMinimumSize(QSize(0, 0))
        tree_view.setMaximumSize(QSize(16777215, 16777215))  # QWIDGETSIZE_MAX
        tree_view.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)

        # Hide the tree's own scrollbar — the external scrollbar is the sole authority.
        tree_view.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        # The transition starts hidden.
        transition.hide()

        # The container expands to fill whatever the parent layout gives it.
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        # External scrollbar — the single authority over vertical scroll position.
        self._scrollbar = QScrollBar(Qt.Orientation.Vertical, self)
        self._scrollbar.valueChanged.connect(self._on_external_scroll)

        # Keep external scrollbar range in sync with the tree's computed content range.
        tree_view.verticalScrollBar().rangeChanged.connect(self._on_tree_range_changed)

        # When the tree's internal scrollbar moves (e.g. keyboard nav, programmatic
        # scrollTo calls), mirror the value back to the external scrollbar.
        tree_view.verticalScrollBar().valueChanged.connect(self._on_tree_internal_scroll)

        # Intercept wheel events on the tree viewport so they reach our scrollbar.
        tree_view.viewport().installEventFilter(self)

    def set_root_path(self, root_path: str) -> None:
        """
        Set the root path used when populating the transition widget.

        Args:
            root_path: Absolute path of the tree root (mindspace or conversations root).
        """
        self._root_path = root_path

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
        Mirror the tree's scroll range onto the external scrollbar.

        Args:
            minimum: New minimum value from the tree's internal scrollbar.
            maximum: New maximum value from the tree's internal scrollbar.
        """
        tree_sb = self._tree_view.verticalScrollBar()
        self._scrollbar.setRange(minimum, maximum)
        self._scrollbar.setPageStep(tree_sb.pageStep())
        self._scrollbar.setSingleStep(tree_sb.singleStep())

    def _on_tree_internal_scroll(self, value: int) -> None:
        """
        Mirror the tree's internal scrollbar back to the external scrollbar.

        This handles cases where the tree scrolls without going through the external
        scrollbar — for example programmatic scrollTo() calls or keyboard navigation.
        The _handling_scroll guard prevents this from recursing when we ourselves
        drive the internal scrollbar from _on_external_scroll.

        Args:
            value: New value of the tree's internal vertical scrollbar.
        """
        if not self._handling_scroll:
            self._scrollbar.setValue(value)

    def _on_external_scroll(self, value: int) -> None:
        """
        Drive the tree view and update breadcrumb state from the external scrollbar.

        This is the single entry point for all scroll input (wheel, drag, programmatic).
        Setting the internal scrollbar value causes Qt to call scrollContentsBy which
        moves the viewport.  We guard against the resulting valueChanged signal looping
        back via _on_tree_internal_scroll with _handling_scroll.

        Args:
            value: New scrollbar position in tree-internal pixel units.
        """
        if self._handling_scroll:
            return

        self._handling_scroll = True
        # Drive the tree's internal scrollbar.  We do NOT block its signals because
        # Qt's internal connection from valueChanged -> scrollContentsBy (which actually
        # moves the viewport) must fire.  The _handling_scroll guard above prevents
        # _on_tree_internal_scroll from recursing back into this method.
        tree_sb = self._tree_view.verticalScrollBar()
        tree_sb.setValue(value)
        self._handling_scroll = False

        # Read the topmost visible item now that the tree viewport has updated.
        index = self._tree_view.indexAt(self._tree_view.viewport().rect().topLeft())
        if not index.isValid():
            return

        row_height = self._tree_view.rowHeight(index)
        if row_height <= 0:
            return

        self._row_height = row_height
        topmost_path = self._tree_view.get_path_from_index(index) or ""
        topmost_is_expanded = self._tree_view.isExpanded(index)
        visual_top = self._tree_view.visualRect(index).top()

        # Update an active sibling transition: height is a pure function of how far
        # we have scrolled since the transition began.
        if self._transition.is_active():
            delta = value - self._transition_ref_value
            new_height = max(0, self._transition_max_height - delta)

            if new_height <= 0:
                # Scrolled forward far enough — complete the transition.
                self._transition.update_height(0)
                self._transition_height = 0
                self._transition.setVisible(False)
                self._apply_geometry()
            elif new_height >= self._transition_max_height:
                # Scrolled back past the start — cancel the transition, re-latch departing folder.
                departing = self._transition.active_path()
                self._transition.update_height(0)
                self._transition_height = 0
                self._transition.setVisible(False)
                self._last_spine_path = departing
                old_breadcrumb_rows = self._breadcrumb_rows
                self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(departing)
                self._apply_geometry()
                self._tree_view.verticalScrollBar().setValue(value + (self._breadcrumb_rows - old_breadcrumb_rows) * self._row_height)
            else:
                self._transition_height = new_height
                self._transition.update_height(new_height)
                self._transition.setVisible(True)
                self._apply_geometry()
            return

        # Detect sibling transition: topmost item is a sibling of the currently latched folder.
        if (self._last_spine_path
                and os.path.dirname(topmost_path) == os.path.dirname(self._last_spine_path)
                and topmost_path != self._last_spine_path):
            # Capture the initial transition height and reference scrollbar value.
            initial_height = row_height + visual_top
            self._transition_max_height = initial_height
            self._transition_ref_value = value
            self._transition.populate(self._last_spine_path, self._root_path)
            self._transition.update_height(initial_height)
            self._transition_height = initial_height
            self._transition.setVisible(initial_height > 0)
            self._last_spine_path = os.path.dirname(self._last_spine_path)
            old_breadcrumb_rows = self._breadcrumb_rows
            self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(self._last_spine_path)
            self._apply_geometry()
            self._tree_view.verticalScrollBar().setValue(value + (self._breadcrumb_rows - old_breadcrumb_rows) * self._row_height)
            return

        spine_path = self._spine_path_for(topmost_path, topmost_is_expanded)
        if spine_path == self._last_spine_path:
            return

        self._last_spine_path = spine_path
        old_breadcrumb_rows = self._breadcrumb_rows
        self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(spine_path)
        self._apply_geometry()
        self._tree_view.verticalScrollBar().setValue(value + (self._breadcrumb_rows - old_breadcrumb_rows) * self._row_height)

    def _spine_path_for(self, topmost_path: str, topmost_is_expanded: bool) -> str:
        """
        Derive the spine context path from the topmost visible tree item.

        Unlatch takes priority: if topmost_path matches the current spine
        (_last_spine_path), that folder has scrolled back into view and must be
        removed — return its parent instead.

        Otherwise, if the topmost item is an expanded folder it becomes the spine
        context; otherwise its parent does.

        Args:
            topmost_path: Absolute path of the topmost visible item.
            topmost_is_expanded: Whether the topmost item is currently expanded.

        Returns:
            Absolute path of the spine context folder, or empty string if at root level.
        """
        if not topmost_path:
            return ""

        if topmost_path == self._last_spine_path:
            parent = os.path.dirname(topmost_path)
            return parent if parent and parent != topmost_path else ""

        if os.path.isdir(topmost_path) and topmost_is_expanded:
            return topmost_path

        parent = os.path.dirname(topmost_path)
        if not parent or parent == topmost_path:
            return ""

        return parent

    def _apply_geometry(self) -> None:
        """Assign geometry to all four child widgets to exactly fill the container."""
        w = self.width()
        h = self.height()

        bc_h = self._breadcrumb_rows * self._row_height
        tr_h = self._transition_height
        tree_top = bc_h + tr_h
        sb_w = self._scrollbar.sizeHint().width()
        tree_w = max(0, w - sb_w)
        tree_h = max(0, h - tree_top)

        self._breadcrumb_bar.setGeometry(QRect(0, 0, tree_w, bc_h))
        self._transition.setGeometry(QRect(0, bc_h, tree_w, tr_h))
        self._tree_view.setGeometry(QRect(0, tree_top, tree_w, tree_h))
        self._scrollbar.setGeometry(QRect(tree_w, 0, sb_w, h))
