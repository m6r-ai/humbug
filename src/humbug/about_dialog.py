"""Dialog box for About Humbug."""

import os

from PySide6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QWidget
from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager


class AboutDialog(QDialog):
    """About dialog for Humbug application."""

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the About dialog."""
        super().__init__(parent)

        self._language_manager = LanguageManager()
        strings = self._language_manager.strings()
        self.setWindowTitle(strings.about_title)
        self.setMinimumWidth(400)
        self.setModal(True)

        style_manager = StyleManager()
        base_font_size = style_manager.base_font_size()
        zoom_factor = style_manager.zoom_factor()

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(8)
        layout.addSpacing(24)  # Space at the top

        # Add application icon
        icon_label = QLabel()
        icon_path = os.path.expanduser("~/.humbug/icons/app-icon.svg")
        icon_pixmap = QPixmap(icon_path)
        scaled_size = int(160 * zoom_factor)  # 160px base size
        icon_label.setPixmap(icon_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        ))
        icon_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(icon_label)
        layout.addSpacing(8)  # Space between icon and title

        # Title with version
        title_label = QLabel("Humbug v0.29")
        title_label.setObjectName("titleLabel")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setMinimumHeight(40)
        layout.addWidget(title_label)

        # Description with hyperlink
        desc_label = QLabel(
            "Visit the <a href='https://github.com/m6r-ai/humbug'>Humbug</a> project to learn more."
        )
        desc_label.setWordWrap(True)
        desc_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        desc_label.setMinimumHeight(40)
        desc_label.setOpenExternalLinks(True)
        layout.addWidget(desc_label)

        min_button_width = int(90 * zoom_factor)

        # Close button with proper styling and sizing
        close_button = QPushButton(strings.close_button)
        close_button.clicked.connect(self.accept)
        close_button.setMinimumWidth(min_button_width)
        close_button.setContentsMargins(8, 8, 8, 8)
        layout.addSpacing(24)  # Add spacing before button
        layout.addWidget(close_button, alignment=Qt.AlignmentFlag.AlignCenter)
        layout.addSpacing(16)  # Space at the bottom

        self.setLayout(layout)

        # Apply dialog styling
        link_color = style_manager.get_color_str(ColorRole.TEXT_LINK)
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
            QLabel a {{
                color: {link_color};
                text-decoration: none;
            }}
            QLabel a:hover {{
                text-decoration: underline;
            }}
            QPushButton {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 6px;
                min-width: 80px;
            }}
            QPushButton:hover {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)};
            }}
            QPushButton:pressed {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)};
            }}
        """)
