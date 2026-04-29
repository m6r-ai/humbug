"""Container that coordinates the breadcrumb bar, transition widget, and tree view geometry."""

import os

from PySide6.QtCore import QRect, QSize
from PySide6.QtWidgets import QSizePolicy, QWidget

from humbug.mindspace.mindspace_breadcrumb_bar import MindspaceBreadcrumbBar
from humbug.mindspace.mindspace_breadcrumb_transition import MindspaceBreadcrumbTransition
from humbug.mindspace.mindspace_tree_view import MindspaceTreeView


class MindspaceBreadcrumbContainer(QWidget):
    """
    A container that coordinates the breadcrumb bar, transition widget, and tree
    view as a single unit.

    Responsibilities:
    - Geometry: manually positions all three widgets so they stack vertically and
      fill the container exactly.
    - Scroll compensation: when the breadcrumb bar or transition widget changes
      height, the tree view's scroll position is adjusted atomically so the visible
      content does not jump.
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

        # Sibling transition tracking.
        self._breadcrumb_height: int = 0
        self._transition_height: int = 0
        self._last_spine_path: str = ""
        self._last_topmost_child: str = ""
        self._last_topmost_child_expanded: bool = False

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

        # The transition starts hidden.
        transition.hide()

        # The container expands to fill whatever the parent layout gives it.
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        breadcrumb_bar.height_changed.connect(self._on_breadcrumb_height_changed)
        transition.height_changed.connect(self._on_transition_height_changed)
        tree_view.scroll_position_changed.connect(self._on_scroll_position_changed)

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

    def _on_scroll_position_changed(
        self,
        topmost_path: str,
        topmost_is_expanded: bool,
        fractional_offset: int,
        row_height: int,
    ) -> None:
        """
        Drive the transition widget on every scroll tick.

        Derives the spine context from topmost_path, updates the breadcrumb bar
        when the spine changes, and detects sibling transitions.

        Args:
            topmost_path: Absolute path of the topmost visible item in the main tree.
            topmost_is_expanded: Whether the topmost item is currently expanded.
            fractional_offset: Pixels the topmost item has scrolled above the viewport top.
            row_height: Height in pixels of one tree row.
        """
        spine_path = self._spine_path_for(topmost_path, topmost_is_expanded)
        if spine_path != self._last_spine_path:
            self._last_spine_path = spine_path
            self._last_topmost_child = ""
            self._last_topmost_child_expanded = False
            self._transition.clear_item()
            self._breadcrumb_bar.update_from_path(spine_path)

        # Drive active transition.
        if self._transition.is_active():
            self._transition.update_height(fractional_offset, row_height)
            return

        if not spine_path or not topmost_path:
            self._last_topmost_child = topmost_path
            self._last_topmost_child_expanded = False
            return

        # Only consider direct children of the spine (one level deeper).
        if os.path.dirname(topmost_path) != spine_path:
            self._last_topmost_child = topmost_path
            self._last_topmost_child_expanded = False
            return

        # Detect sibling transition: topmost child changed from a previous expanded folder.
        prev = self._last_topmost_child
        prev_was_expanded = self._last_topmost_child_expanded
        self._last_topmost_child = topmost_path
        self._last_topmost_child_expanded = topmost_is_expanded

        if prev and prev_was_expanded and prev != topmost_path and os.path.dirname(prev) == spine_path:
            self._transition.populate(prev, self._root_path)
            self._transition.update_height(fractional_offset, row_height)

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

    def _on_breadcrumb_height_changed(self, new_height: int) -> None:
        """
        Handle a breadcrumb bar height change.

        On latch (delta > 0): scroll forward by delta so the content that was
        below the newly latched row stays at the top of the tree viewport.

        On unlatch (delta < 0): scroll back by abs(delta) - 1 so the reappearing
        row is visible from its first pixel.

        Args:
            new_height: New required height of the breadcrumb bar in pixels.
        """
        delta = new_height - self._breadcrumb_height

        self._breadcrumb_height = new_height
        self._tree_view.suppress_scroll_signals(True)
        self._apply_geometry()
        if delta != 0:
            sb = self._tree_view.verticalScrollBar()
            sb.setValue(sb.value() + delta)

        self._tree_view.suppress_scroll_signals(False)

    def _on_transition_height_changed(self, new_height: int) -> None:
        """
        Handle a transition widget height change.

        Applies the new geometry.  No scroll compensation is needed for the same
        reason as breadcrumb height changes.

        Args:
            new_height: New height in pixels for the transition widget (0 = hidden).
        """
        self._transition_height = new_height
        self._transition.setVisible(new_height > 0)
        self._tree_view.suppress_scroll_signals(True)
        self._apply_geometry()
        self._tree_view.suppress_scroll_signals(False)

    def _apply_geometry(self) -> None:
        """Assign geometry to all three child widgets to exactly fill the container."""
        w = self.width()
        h = self.height()

        bc_h = self._breadcrumb_height
        tr_h = self._transition_height
        tree_top = bc_h + tr_h
        tree_h = max(0, h - tree_top)

        self._breadcrumb_bar.setGeometry(QRect(0, 0, w, bc_h))
        self._transition.setGeometry(QRect(0, bc_h, w, tr_h))
        self._tree_view.setGeometry(QRect(0, tree_top, w, tree_h))
