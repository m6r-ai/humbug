from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Signal
from PySide6.QtGui import QDragEnterEvent, QDragLeaveEvent, QDragMoveEvent, QDropEvent, QPainter, QPaintEvent

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class SpacerDropWidget(QWidget):
    """A spacer widget that accepts path drops."""

    path_dropped = Signal(str, str)  # source_type, path
    tab_dropped = Signal(str, str)  # tab_id, source_manager_id

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the spacer drop widget."""
        super().__init__(parent)
        self._drag_active = False
        self._style_manager = StyleManager()
        self.setAcceptDrops(True)

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Accept path drops and activate the highlight."""
        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            self._drag_active = True
            self.update()
            return

        if event.mimeData().hasFormat("application/x-humbug-tab"):
            event.acceptProposedAction()
            self._drag_active = True
            self.update()
            return

        event.ignore()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Accept path drops during move, keeping the highlight active."""
        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            return

        if event.mimeData().hasFormat("application/x-humbug-tab"):
            event.acceptProposedAction()
            return

        event.ignore()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        """Clear the highlight when the drag leaves."""
        self._drag_active = False
        self.update()
        super().dragLeaveEvent(event)

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle path drops by emitting signal."""
        self._drag_active = False
        self.update()

        if event.mimeData().hasFormat("application/x-humbug-path"):
            mime_data = event.mimeData().data("application/x-humbug-path").data()

            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            path = mime_data.decode()

            source_type = None
            if event.mimeData().hasFormat("application/x-humbug-source"):
                source_data = event.mimeData().data("application/x-humbug-source").data()

                if not isinstance(source_data, bytes):
                    source_data = bytes(source_data)

                source_type = source_data.decode()

            self.path_dropped.emit(source_type, path)
            event.acceptProposedAction()
            return

        if event.mimeData().hasFormat("application/x-humbug-tab"):
            mime_data = event.mimeData().data("application/x-humbug-tab").data()

            if not isinstance(mime_data, bytes):
                mime_data = bytes(mime_data)

            tab_id = mime_data.decode()
            source_manager_id = ""
            if event.mimeData().hasFormat("application/x-humbug-tab-manager"):
                source_data = event.mimeData().data("application/x-humbug-tab-manager").data()
                if not isinstance(source_data, bytes):
                    source_data = bytes(source_data)
                source_manager_id = source_data.decode()

            self.tab_dropped.emit(tab_id, source_manager_id)
            event.acceptProposedAction()
            return

        event.ignore()

    def paintEvent(self, event: QPaintEvent) -> None:
        """Paint a highlight background when a drag is active over this widget."""
        super().paintEvent(event)
        if not self._drag_active:
            return

        painter = QPainter(self)
        color = self._style_manager.get_color(ColorRole.DROP_TARGET_HIGHLIGHT)
        painter.fillRect(self.rect(), color)
        painter.end()
