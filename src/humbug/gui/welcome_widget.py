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
        self._title_label = QLabel("Humbug")
        self._title_label.setAlignment(Qt.AlignHCenter | Qt.AlignBottom)

        # Version number
        self._version_label = QLabel(f"Version {format_version()}")
        self._version_label.setAlignment(Qt.AlignHCenter | Qt.AlignTop)

        # Add widgets to layout
        layout.addWidget(self._title_label)
        layout.addWidget(self._version_label)
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
        scaled_size = int(128 * zoom_factor)  # 128px base size for welcome screen
        self._icon_label.setPixmap(icon_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.KeepAspectRatio,
            Qt.SmoothTransformation
        ))

        # Update title font size (2x default)
        title_font = self._title_label.font()
        title_font.setPointSizeF(base_font_size * 2 * zoom_factor)
        self._title_label.setFont(title_font)

        # Update version font size (default size)
        version_font = self._version_label.font()
        version_font.setPointSizeF(base_font_size * zoom_factor)
        self._version_label.setFont(version_font)

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
            }}
        """)
