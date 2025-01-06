"""Dialog box for About Humbug."""

import os

from PySide6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton
from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap

from humbug import format_version
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class AboutDialog(QDialog):
    """About dialog for Humbug application."""

    def __init__(self, parent=None):
        """Initialize the About dialog."""
        super().__init__(parent)
        self.setWindowTitle("About Humbug")
        self.setMinimumWidth(400)
        self.setModal(True)

        style_manager = StyleManager()
        base_font_size = style_manager.base_font_size

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(8)
        layout.addSpacing(24)  # Space at the top

        # Add application icon
        icon_label = QLabel()
        icon_path = os.path.expanduser("~/.humbug/icons/app-icon.svg")
        icon_pixmap = QPixmap(icon_path)
        scaled_size = int(96 * style_manager.zoom_factor)  # 96px base size
        icon_label.setPixmap(icon_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.KeepAspectRatio,
            Qt.SmoothTransformation
        ))
        icon_label.setAlignment(Qt.AlignCenter)
        layout.addWidget(icon_label)
        layout.addSpacing(24)  # Space between icon and title

        # Title with version
        title_label = QLabel(f"Humbug v{format_version()}")
        title_label.setObjectName("titleLabel")
        title_label.setAlignment(Qt.AlignCenter)
        title_label.setMinimumHeight(40)
        layout.addWidget(title_label)

        # Description
        desc_label = QLabel(
            "Humbug is a GUI-based application that allows users to "
            "interact with AI backends through a simple chat interface."
        )
        desc_label.setWordWrap(True)
        desc_label.setAlignment(Qt.AlignCenter)
        desc_label.setMinimumHeight(40)
        layout.addWidget(desc_label)

        # Close button with proper styling and sizing
        close_button = QPushButton("Close")
        close_button.clicked.connect(self.accept)
        close_button.setMinimumWidth(80)
        close_button.setContentsMargins(6, 6, 6, 6)
        layout.addSpacing(16)  # Add spacing before button
        layout.addWidget(close_button, alignment=Qt.AlignCenter)
        layout.addSpacing(16)  # Space at the bottom

        self.setLayout(layout)

        # Apply dialog styling
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel#titleLabel {{
                font-size: {base_font_size * 1.5}pt;
                font-weight: bold;
                margin: 10px;
            }}
            QPushButton {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 6px;
                min-width: 80px;
            }}
            QPushButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
            }}
            QPushButton:disabled {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DISABLED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_DISABLED)};
            }}
        """)
