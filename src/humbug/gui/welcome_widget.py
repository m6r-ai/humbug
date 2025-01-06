"""Welcome message widget implementation."""

from PySide6.QtWidgets import QVBoxLayout, QLabel, QFrame
from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap

from humbug import format_version
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class WelcomeWidget(QFrame):
    """Widget showing welcome message when no tabs are open."""

    def __init__(self, parent=None):
        """Initialize welcome widget."""
        super().__init__(parent)

        # Create layout
        layout = QVBoxLayout()
        self.setLayout(layout)

        # Add application icon
        self._icon_label = QLabel()
        self._icon_label.setAlignment(Qt.AlignHCenter | Qt.AlignBottom)
        layout.addStretch()
        layout.addWidget(self._icon_label)
        layout.addSpacing(20)  # Space between icon and title

        # Application name
        self._title_label = QLabel(f"Humbug v{format_version()}")
        self._title_label.setAlignment(Qt.AlignHCenter | Qt.AlignBottom)

        # Add widgets to layout
        layout.addWidget(self._title_label)
        layout.addStretch()

        # Set margins for dialog-style spacing
        layout.setContentsMargins(20, 20, 20, 20)

        # Get style manager and connect to changes
        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

    def _handle_style_changed(self) -> None:
        """Update styling when application style changes."""
        zoom_factor = self._style_manager.zoom_factor
        base_font_size = self._style_manager.base_font_size

        # Update icon size
        icon_path = self._style_manager.get_icon_path("app-icon-disabled")
        icon_pixmap = QPixmap(icon_path)
        scaled_size = int(160 * zoom_factor)  # 160px base size for welcome screen
        self._icon_label.setPixmap(icon_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.KeepAspectRatio,
            Qt.SmoothTransformation
        ))

        # Update colors and frame style
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {self._style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)};
                border: none;
                border-radius: {4 * zoom_factor}px;
            }}
            QLabel {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
                background: none;
                font-size: {base_font_size * 1.5}pt;
                font-weight: bold;
            }}
        """)
