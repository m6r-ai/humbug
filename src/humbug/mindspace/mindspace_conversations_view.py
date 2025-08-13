"""Conversations view widget for mindspace."""

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager


class MindspaceConversationsView(QWidget):
    """Placeholder widget for conversations view in mindspace."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the conversations view widget."""
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)

        # Create layout
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # Create header container
        self._header_widget = QWidget()
        header_layout = QVBoxLayout(self._header_widget)
        header_layout.setContentsMargins(0, 0, 0, 0)
        header_layout.setSpacing(0)

        # Create conversations label
        self._conversations_label = QLabel()
        self._conversations_label.setContentsMargins(0, 0, 0, 0)
        self._conversations_label.setText("Conversations")  # Will be updated by language change

        header_layout.addWidget(self._conversations_label)
        layout.addWidget(self._header_widget)

        # Add stretch to fill remaining space
        layout.addStretch()

        self._on_language_changed()

    def _on_language_changed(self) -> None:
        """Update when the language changes."""
        # For now, "Conversations" is not in the language strings, so we'll use English
        # This can be updated when conversation strings are added to the language files
        self._conversations_label.setText("Conversations")
        self.apply_style()

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        # Update font size for label
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._conversations_label.setFont(font)

        # Apply same styling as mindspace file tree header
        self._header_widget.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
                margin: 2px 0px 0px 0px;
            }}

            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: transparent;
                border: none;
                padding: 4px 0px 5px 7px;
            }}
        """)

        # Set background for the main widget
        self.setStyleSheet(f"""
            QWidget {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)};
            }}
        """)
