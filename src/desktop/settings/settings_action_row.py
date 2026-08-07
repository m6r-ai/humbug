"""Settings action row widget with a button and status label."""

from PySide6.QtWidgets import QWidget, QHBoxLayout, QPushButton, QLabel
from PySide6.QtCore import Qt

from desktop.color_role import ColorRole
from desktop.settings.settings_item import SettingsItem


class SettingsActionRow(SettingsItem):
    """A settings item containing one or two buttons and a shared read-only status label."""

    def __init__(
        self, button_text: str, second_button_text: str | None = None, parent: QWidget | None = None
    ) -> None:
        super().__init__(parent)

        layout = QHBoxLayout()
        layout.setContentsMargins(0, 6, 0, 6)
        layout.setSpacing(12)

        self._button = QPushButton(button_text)
        self._button.setMinimumHeight(30)
        layout.addWidget(self._button)

        self._second_button: QPushButton | None = None
        if second_button_text is not None:
            self._second_button = QPushButton(second_button_text)
            self._second_button.setMinimumHeight(30)
            layout.addWidget(self._second_button)

        self._status_label = QLabel("")
        self._status_label.setAlignment(Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft)

        layout.addWidget(self._status_label, 1)
        self.setLayout(layout)

    def button(self) -> QPushButton:
        """Return the primary action button."""
        return self._button

    def second_button(self) -> QPushButton:
        """Return the secondary action button (only valid if created with second_button_text)."""
        assert self._second_button is not None, "SettingsActionRow was created without a second button"
        return self._second_button

    def status_label(self) -> QLabel:
        """Return the status label."""
        return self._status_label

    def set_button_text(self, text: str) -> None:
        """Set the button label text."""
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

    def set_success(self, text: str) -> None:
        """Show a status message in the theme success (green) colour."""
        color = self._style_manager.get_color_str(ColorRole.TEXT_SUCCESS)
        self._status_label.setStyleSheet(f"color: {color};")
        self._status_label.setText(text)

    def _on_style_changed(self) -> None:
        zoom = self._style_manager.zoom_factor()
        self._button.setMinimumHeight(int(30 * zoom))
        if self._second_button is not None:
            self._second_button.setMinimumHeight(int(30 * zoom))
