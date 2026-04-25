"""Custom splitter for main window that uses direct painting instead of stylesheets."""

from typing import Any

from PySide6.QtCore import QEasingCurve, Qt, QVariantAnimation
from PySide6.QtWidgets import QSplitter, QSplitterHandle
from PySide6.QtGui import QPainter

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class MainWindowSplitterHandle(QSplitterHandle):
    """Custom splitter handle that paints itself without using stylesheets."""

    def __init__(self, orientation: Qt.Orientation, parent: QSplitter) -> None:
        super().__init__(orientation, parent)

    def paintEvent(self, _event: Any) -> None:
        """Paint the handle with the current style."""
        style_manager = StyleManager()
        painter = QPainter(self)
        painter.fillRect(self.rect(), style_manager.get_color(ColorRole.SPLITTER))
        painter.end()


class MainWindowSplitter(QSplitter):
    """
    Custom splitter for the main window that uses custom painted handles
    to avoid stylesheet cascade through child widgets.
    """

    def __init__(self, orientation: Qt.Orientation) -> None:
        super().__init__(orientation)
        self.setHandleWidth(1)
        self.setChildrenCollapsible(False)
        self._animation = QVariantAnimation(self)
        self._animation.setDuration(160)
        self._animation.setEasingCurve(QEasingCurve.Type.InOutCubic)
        self._animation.valueChanged.connect(self._on_animation_value_changed)
        self._animation.finished.connect(self._on_animation_finished)
        self._animating = False

    def createHandle(self) -> QSplitterHandle:
        """Create a custom handle that paints itself without stylesheets."""
        return MainWindowSplitterHandle(self.orientation(), self)

    def toggle_mindspace(self) -> None:
        """Animate the mindspace view between collapsed and expanded states."""
        from humbug.mindspace.mindspace_view import MindspaceView

        if self.count() == 0:
            return

        widget = self.widget(0)
        if not isinstance(widget, MindspaceView):
            return

        rail_width = widget.rail_width
        sizes = self.sizes()
        current = sizes[0]
        collapsed = widget._sidebar_collapsed

        if collapsed:
            target = max(widget._expanded_sidebar_width, rail_width + widget._content_min_width)

        else:
            widget._expanded_sidebar_width = current
            target = rail_width
            super().moveSplitter(rail_width, 1)

        widget.set_collapsed(not collapsed)

        self._animation.stop()
        self._animating = True
        self._animation.setStartValue(current)
        self._animation.setEndValue(target)
        self._animation.start()

    def _on_animation_value_changed(self, value: object) -> None:
        """Move the splitter to the animated position."""
        self.moveSplitter(int(value), 1)

    def _on_animation_finished(self) -> None:
        """Clean up after animation completes."""
        self._animating = False

    def moveSplitter(self, pos: int, index: int) -> None:
        """Handle splitter drags, auto-collapsing and expanding the mindspace view."""
        from humbug.mindspace.mindspace_view import MindspaceView

        if index != 1 or self.count() == 0:
            super().moveSplitter(pos, index)
            return

        widget = self.widget(0)
        if not isinstance(widget, MindspaceView):
            super().moveSplitter(pos, index)
            return

        rail_width = widget.rail_width
        collapse_threshold = rail_width * 3

        if self._animating:
            super().moveSplitter(pos, index)
            return

        if widget._sidebar_collapsed:
            if pos > collapse_threshold:
                widget._expanded_sidebar_width = pos
                widget.set_collapsed(False)
                super().moveSplitter(pos, index)

            # else: below threshold while collapsed — splitter minimum enforces rail_width

       else:
            if pos <= collapse_threshold:
                widget._expanded_sidebar_width = self.sizes()[0]
                widget.set_collapsed(True)
                super().moveSplitter(rail_width, index)

            else:
                super().moveSplitter(pos, index)
