"""Container that coordinates the breadcrumb bar, transition widget, and tree view geometry."""

from PySide6.QtCore import QRect, QSize
from PySide6.QtWidgets import QSizePolicy, QWidget

from humbug.mindspace.mindspace_breadcrumb_bar import MindspaceBreadcrumbBar
from humbug.mindspace.mindspace_breadcrumb_transition import MindspaceBreadcrumbTransition
from humbug.mindspace.mindspace_tree_view import MindspaceTreeView


class MindspaceBreadcrumbContainer(QWidget):
    """
    A container that manually manages the geometry of the breadcrumb bar, transition
    widget, and tree view as a single coordinated unit.

    The three widgets are stacked vertically and fill the container exactly:

        ┌─────────────────────────┐
        │   breadcrumb bar        │  breadcrumb_height px  (Fixed, self-reporting)
        ├─────────────────────────┤
        │   transition widget     │  transition_height px  (Fixed, self-reporting)
        ├─────────────────────────┤
        │   tree view             │  remainder             (Expanding)
        └─────────────────────────┘

    The breadcrumb bar and transition widget report their required height via
    sizeHint()/minimumSizeHint() and emit height_changed when it changes.  The
    container listens to height_changed and performs the geometry update plus
    scroll compensation atomically with scroll signals suppressed, so no spurious
    spine-change events are emitted.
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

        # Re-parent all three widgets into this container.
        breadcrumb_bar.setParent(self)
        transition.setParent(self)
        tree_view.setParent(self)

        # Breadcrumb and transition are fixed-height, self-reporting via sizeHint.
        breadcrumb_bar.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        transition.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

        # Tree view expands to fill remaining space; we manage its geometry directly.
        tree_view.setMinimumSize(QSize(0, 0))
        tree_view.setMaximumSize(QSize(16777215, 16777215))  # QWIDGETSIZE_MAX
        tree_view.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)

        # The transition starts hidden.
        transition.hide()

        # The container itself expands to fill whatever space the parent layout gives it.
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)

        breadcrumb_bar.height_changed.connect(self._on_breadcrumb_height_changed)
        transition.height_changed.connect(self._on_transition_height_changed)

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
    # Internal                                                             #
    # ------------------------------------------------------------------ #

    def _on_breadcrumb_height_changed(self, new_height: int) -> None:
        """
        Handle a breadcrumb bar height change.

        Adjusts all three widget geometries and compensates the tree view scroll
        position atomically — scroll signals are suppressed during the adjustment
        so no spurious spine-change events are emitted.

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

        The transition widget grows or shrinks between the breadcrumb bar and the tree
        view.  When it grows the tree view's viewport shrinks from the top, hiding
        content — we compensate by scrolling forward by the delta.  When it shrinks
        the viewport grows from the top, revealing content — we compensate by scrolling
        back by the delta.

        When the height reaches zero the transition is closing.  At that moment the
        breadcrumb bar simultaneously gains or loses a row and its own height_changed
        fires with the compensating delta — so we must not also compensate here or
        the scroll position will be adjusted twice.

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
