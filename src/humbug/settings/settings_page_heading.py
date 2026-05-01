"""Page-level heading for settings pages."""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

from humbug.settings.settings_item import SettingsItem
from humbug.style_manager import ColorRole


class SettingsPageHeading(SettingsItem):
    """
    Page-level heading displayed at the top of each settings page.

    Styled larger than a section heading and using TEXT_HEADING colour.
    """

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """
        Initialize a page heading with the specified title.

        Args:
            title: Text to display as the page heading
            parent: Parent widget
        """
        super().__init__(parent)

        layout = QVBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)

        self._label = QLabel(title)
        self._label.setIndent(0)
        layout.addWidget(self._label)

        self.setLayout(layout)
        self._on_style_changed()

    def set_label(self, text: str) -> None:
        """Set the heading label text."""
        self._label.setText(text)

    def _on_style_changed(self) -> None:
        """Update heading styling."""
        font_size = self._style_manager.base_font_size()
        zoom_factor = self._style_manager.zoom_factor()
        scaled_font_size = font_size * zoom_factor * 1.3
        color = self._style_manager.get_color_str(ColorRole.TEXT_HEADING)
        self._label.setStyleSheet(f"""
            QLabel {{
                color: {color};
                font-size: {scaled_font_size:.1f}pt;
                font-weight: bold;
                border-radius: none;
                border: 0px;
                padding: 0px;
                margin: 0em 0em 0.75em 0em;
            }}
        """)
