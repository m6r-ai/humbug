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

        self._breadcrumb_height: int = 0
        self._breadcrumb_rows: int = 0
        self._transition_height: int = 0
        self._last_spine_path: str = ""
        self._row_height: int = 0
        self._transition_active: bool = False
        self._transition_start_sb: int = 0
        self._transition_row_height: int = 0

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
        visual_top: int,
        row_height: int,
    ) -> None:
        """
        Update the breadcrumb bar and drive the sibling transition when appropriate.

        Args:
            topmost_path: Absolute path of the topmost visible item in the main tree.
            topmost_is_expanded: Whether the topmost item is currently expanded.
            visual_top: Y coordinate of the topmost item's top edge in viewport space (0 or negative).
            row_height: Height in pixels of one tree row.
        """
        self._row_height = row_height
        # While the transition is active, update its height and hold the spine steady.
        if self._transition.is_active():
            sb = self._tree_view.verticalScrollBar().value()
            new_height = self._transition_row_height - (sb - self._transition_start_sb)
            if new_height <= 0:
                # Scrolled forward far enough — complete the transition.
                self._transition.update_height(0)
                self._transition_active = False
                print(f"TRANSITION: went inactive after update_height")

            elif new_height >= self._transition_row_height:
                # Scrolled back — cancel the transition, re-latch A.
                departing = self._transition.active_path()
                self._transition.update_height(0)
                self._transition_active = False
                self._last_spine_path = departing
                self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(departing)
                self._apply_geometry()
                print(f"TRANSITION: cancelled, re-latching {os.path.basename(departing)!r} - rows: {self._breadcrumb_rows}")

            else:
                self._transition.update_height(new_height)

            return

        # Detect sibling transition: topmost item is a sibling of the currently
        # latched folder, regardless of whether it is expanded or not.
        print(f"spine path: {self._last_spine_path}, topmost: {topmost_path}")
        if (self._last_spine_path
                and os.path.dirname(topmost_path) == os.path.dirname(self._last_spine_path)
                and topmost_path != self._last_spine_path):
            self._transition_active = True
            sb = self._tree_view.verticalScrollBar().value()
            self._transition_start_sb = sb + visual_top  # row-aligned start position
            self._transition_row_height = row_height
            self._transition.populate(self._last_spine_path, self._root_path)
            self._transition.update_height(row_height)
            self._last_spine_path = os.path.dirname(self._last_spine_path)
            self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(self._last_spine_path)
            self._apply_geometry()
            print(f"TRANSITION: went active, departing={os.path.basename(self._last_spine_path)!r} - rows: {self._breadcrumb_rows}")
            return

        spine_path = self._spine_path_for(topmost_path, topmost_is_expanded)
        if spine_path == self._last_spine_path:
            return

        self._last_spine_path = spine_path
        self._breadcrumb_rows = self._breadcrumb_bar.update_from_path(spine_path)
        self._apply_geometry()
        print(f"update: for {spine_path} - rows: {self._breadcrumb_rows}")

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

    def _on_transition_height_changed(self, new_height: int) -> None:
        """
        Handle a transition widget height change.

        Applies the new geometry with scroll signals suppressed so the tree
        position does not jump.

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

        bc_h = self._breadcrumb_rows * self._row_height
        tr_h = self._transition_height
        tree_top = bc_h + tr_h
        tree_h = max(0, h - tree_top)

        self._breadcrumb_bar.setGeometry(QRect(0, 0, w, bc_h))
        self._transition.setGeometry(QRect(0, bc_h, w, tr_h))
        self._tree_view.setGeometry(QRect(0, tree_top, w, tree_h))
