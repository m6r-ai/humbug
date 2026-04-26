"""Section header widget for mindspace panes."""

from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QFont
from PySide6.QtWidgets import QHBoxLayout, QLabel, QWidget

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


class MindspaceSectionHeader(QWidget):
    """A simple titled header for mindspace panes."""

    def __init__(self, title: str, parent: QWidget | None = None) -> None:
        """Initialize the section header."""
        super().__init__(parent)
        self.setObjectName("MindspaceSectionHeader")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._style_manager = StyleManager()

        layout = QHBoxLayout(self)
        spacing = 5
        layout.setSpacing(spacing)
        layout.setContentsMargins(spacing + 3, spacing, spacing, spacing)

        self._title_label = QLabel(title, self)
        self._title_label.setIndent(0)
        layout.addWidget(self._title_label)
        layout.addStretch()

    def set_title(self, title: str) -> None:
        """Set the header title text."""
        self._title_label.setText(title)

    def apply_style(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()
        background = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)

        font = self._title_label.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._title_label.setFont(font)

        self.setStyleSheet(f"""
            QWidget#MindspaceSectionHeader {{
                background-color: {background};
                border: none;
            }}
            QWidget#MindspaceSectionHeader QLabel {{
                color: {text};
                background: transparent;
            }}
        """)
