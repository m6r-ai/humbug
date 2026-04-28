"""Container that coordinates the breadcrumb bar, transition widget, and tree view geometry."""

import os

from PySide6.QtCore import QRect, QSize, QTimer
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
        tree_view.visible_top_changed.connect(self._on_visible_top_changed)
        tree_view.scroll_position_changed.connect(self._on_scroll_position_changed)

    # ------------------------------------------------------------------ #
    # Public API                                                           #
    # ------------------------------------------------------------------ #

    def set_root_path(self, root_path: str) -> None:
        """
        Set the root path used when populating the transition widget.

        Args:
            root_path: Absolute path of the tree root (mindspace or conversations root).
        """
        self._root_path = root_path

    # ------------------------------------------------------------------ #
    # Qt overrides                                                         #
    # ------------------------------------------------------------------ #

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

    # ------------------------------------------------------------------ #
    # Signal handlers                                                      #
    # ------------------------------------------------------------------ #

    def _on_visible_top_changed(self, path: str) -> None:
        """
        Handle a spine change from the tree view.

        A spine change means the depth changed — a true latch or unlatch along the
        same branch.  We clear any active sibling transition and update the breadcrumb
        bar.

        Args:
            path: New spine context path.
        """
        if path != self._last_spine_path:
            self._last_spine_path = path
            self._last_topmost_child = ""
            self._last_topmost_child_expanded = False
            self._transition.clear_item()

        self._breadcrumb_bar.update_from_path(path)

    def _on_scroll_position_changed(
        self,
        spine_path: str,
        topmost_path: str,
        topmost_is_expanded: bool,
        fractional_offset: int,
        row_height: int,
    ) -> None:
        """
        Drive the transition widget on every scroll tick.

        Sibling transition detection: the spine is stable, the topmost item is a
        direct child of the spine, and it has changed from a previous topmost child
        that was an expanded folder.

        Args:
            spine_path: Current spine context path.
            topmost_path: Absolute path of the topmost visible item in the main tree.
            topmost_is_expanded: Whether the topmost item is currently expanded.
            fractional_offset: Pixels the topmost item has scrolled above the viewport top.
            row_height: Height in pixels of one tree row.
        """
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

    def _on_breadcrumb_height_changed(self, new_height: int) -> None:
        """
        Handle a breadcrumb bar height change.

        Compensates the scroll position by the height delta so visible content
        stays stable regardless of whether the breadcrumb grew or shrank.

        Args:
            new_height: New required height of the breadcrumb bar in pixels.
        """
        old_height = self._breadcrumb_bar.height()
        delta = new_height - old_height

        self._tree_view.suppress_scroll_signals(True)
        self._apply_geometry()
        if delta != 0:
            sb = self._tree_view.verticalScrollBar()
            sb.setValue(sb.value() + delta)
        self._tree_view.suppress_scroll_signals(False)

    def _on_transition_height_changed(self, new_height: int) -> None:
        """
        Handle a transition widget height change.

        Compensates the scroll position for the delta so visible content stays stable.
        When the transition closes (new_height == 0) no compensation is applied here
        because the breadcrumb bar's simultaneous height change handles it.

        Args:
            new_height: New height in pixels for the transition widget (0 = hidden).
        """
        old_height = self._transition.minimumSizeHint().height() if self._transition.isVisible() else 0
        delta = new_height - old_height

        self._transition.setVisible(new_height > 0)
        self._tree_view.suppress_scroll_signals(True)
        self._apply_geometry()
        if delta != 0 and new_height > 0:
            sb = self._tree_view.verticalScrollBar()
            sb.setValue(sb.value() + delta)
        self._tree_view.suppress_scroll_signals(False)

    # ------------------------------------------------------------------ #
    # Internal                                                             #
    # ------------------------------------------------------------------ #

    def _apply_geometry(self) -> None:
        """Assign geometry to all three child widgets to exactly fill the container."""
        w = self.width()
        h = self.height()

        bc_h = self._breadcrumb_bar.minimumSizeHint().height()
        tr_h = self._transition.minimumSizeHint().height() if self._transition.isVisible() else 0
        tree_top = bc_h + tr_h
        tree_h = max(0, h - tree_top)

        self._breadcrumb_bar.setGeometry(QRect(0, 0, w, bc_h))
        self._transition.setGeometry(QRect(0, bc_h, w, tr_h))
        self._tree_view.setGeometry(QRect(0, tree_top, w, tree_h))
