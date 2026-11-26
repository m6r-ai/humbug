"""The row of files attached to a message that has not been sent yet, shared by every input that takes attachments."""

from PySide6.QtCore import QSize, Qt, Signal
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QHBoxLayout, QLabel, QToolButton, QWidget

from desktop.style_manager import StyleManager


_ICON_BASE_SIZE = 14


class AttachmentsBar(QWidget):
    """
    Shows each attached file as a chip with a button that removes it.

    The bar is hidden while nothing is attached.  It holds the attachments
    itself, so an input only adds files and takes them when its message is sent.
    The chips are named `_attachment_widget`, `_attachment_label` and
    `_attachment_remove` for the input's stylesheet.
    """

    # Emitted after a file is attached or removed.
    changed = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("_attachments_bar")
        self._style_manager = StyleManager()
        self._attachments: list[tuple[str, str]] = []
        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(0, 0, 0, 0)
        self._layout.setSpacing(4)
        self._layout.addStretch()
        self.hide()

    def add_attachment(self, filename: str, content: str) -> None:
        """Attach a file and show it."""
        self._attachments.append((filename, content))
        self._rebuild()

    def attachments(self) -> list[tuple[str, str]]:
        """Return the attached files as (filename, content) pairs."""
        return list(self._attachments)

    def clear(self) -> None:
        """Remove every attached file."""
        self._attachments.clear()
        self._rebuild()

    def apply_style(self) -> None:
        """Rescale the remove icons after a zoom change."""
        icon = QIcon(self._style_manager.scale_icon("close", _ICON_BASE_SIZE))
        size = self._icon_size()
        for button in self.findChildren(QToolButton, "_attachment_remove"):
            button.setIcon(icon)
            button.setIconSize(size)

    def _icon_size(self) -> QSize:
        """Return the size of a remove icon at the current zoom."""
        scaled = int(_ICON_BASE_SIZE * self._style_manager.zoom_factor())
        return QSize(scaled, scaled)

    def _remove_attachment(self, index: int) -> None:
        """Remove the attachment at the given index."""
        if 0 <= index < len(self._attachments):
            self._attachments.pop(index)
            self._rebuild()

    def _rebuild(self) -> None:
        """Show a chip for each attached file, and hide the bar when there are none."""
        # Remove all existing chips, leaving the trailing stretch.
        while self._layout.count() > 1:
            item = self._layout.takeAt(0)
            widget = item.widget() if item else None
            if widget:
                widget.deleteLater()

        icon = QIcon(self._style_manager.scale_icon("close", _ICON_BASE_SIZE))
        for index, (filename, _) in enumerate(self._attachments):
            chip = QWidget()
            chip.setObjectName("_attachment_widget")
            chip.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
            chip_layout = QHBoxLayout(chip)
            chip_layout.setContentsMargins(8, 4, 4, 4)
            chip_layout.setSpacing(4)

            name_label = QLabel(filename)
            name_label.setObjectName("_attachment_label")
            chip_layout.addWidget(name_label)

            remove_button = QToolButton()
            remove_button.setObjectName("_attachment_remove")
            remove_button.setIcon(icon)
            remove_button.setIconSize(self._icon_size())
            remove_button.clicked.connect(lambda _checked=False, i=index: self._remove_attachment(i))
            chip_layout.addWidget(remove_button)

            self._layout.insertWidget(index, chip)

        self.setVisible(bool(self._attachments))
        self.changed.emit()
