"""Settings action row widget with a button and status label."""

from PySide6.QtWidgets import QWidget, QHBoxLayout, QPushButton, QLabel
from PySide6.QtCore import Qt

from humbug.color_role import ColorRole
from humbug.settings.settings_item import SettingsItem


class SettingsActionRow(SettingsItem):
    """A settings item containing a button and a read-only status label."""

    def __init__(self, button_text: str, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        layout = QHBoxLayout()
        layout.setContentsMargins(0, 6, 0, 6)
        layout.setSpacing(12)

        self._button = QPushButton(button_text)
        self._button.setMinimumHeight(30)

        self._status_label = QLabel("")
        self._status_label.setAlignment(Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft)

        layout.addWidget(self._button)
        layout.addWidget(self._status_label, 1)
        self.setLayout(layout)

    @property
    def button(self) -> QPushButton:
        return self._button

    @property
    def status_label(self) -> QLabel:
        return self._status_label

    def set_button_text(self, text: str) -> None:
        self._button.setText(text)

    def set_status(self, text: str) -> None:
        """Show a normal (theme primary text) status message."""
        color = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        self._status_label.setStyleSheet(f"color: {color};")
        self._status_label.setText(text)

    def set_error(self, text: str) -> None:
        """Show a status message in the theme error colour."""
        color = self._style_manager.get_color_str(ColorRole.TEXT_ERROR)
        self._status_label.setStyleSheet(f"color: {color};")
        self._status_label.setText(text)

    def _on_style_changed(self) -> None:
        zoom = self._style_manager.zoom_factor()
        self._button.setMinimumHeight(int(30 * zoom))
