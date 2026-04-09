from PySide6.QtWidgets import QWidget
from PySide6.QtCore import Signal
from PySide6.QtGui import QDragEnterEvent, QDragMoveEvent, QDropEvent


class SpacerDropWidget(QWidget):
    """A spacer widget that accepts path drops."""

    path_dropped = Signal(str, str)  # source_type, path

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the spacer drop widget."""
        super().__init__(parent)
        self.setAcceptDrops(True)

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        """Accept path drops."""
        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            return

        event.ignore()

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        """Accept path drops during move."""
        if event.mimeData().hasFormat("application/x-humbug-path"):
            event.acceptProposedAction()
            return

        event.ignore()

    def dropEvent(self, event: QDropEvent) -> None:
        """Handle path drops by emitting signal."""
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

        event.ignore()
