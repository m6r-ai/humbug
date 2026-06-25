"""Dialog box for About Humbug."""

from PySide6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QWidget, QSizePolicy
from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap

from desktop.color_role import ColorRole
from desktop.gradient_label import GradientBorderLabel, GradientLabel
from desktop.language.language_manager import LanguageManager
from desktop.style_manager import StyleManager
from desktop.url_opener import open_url
from desktop.version import CURRENT_VERSION


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
        link_color = style_manager.get_color_str(ColorRole.TEXT_LINK)

        # Main layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(8)
        layout.addSpacing(24)  # Space at the top

        # Add application logo with animated gradient border
        logo_label = GradientBorderLabel(
            style_manager.get_color_str(ColorRole.BRAND_GRADIENT_START),
            style_manager.get_color_str(ColorRole.BRAND_GRADIENT_END),
            radius=16.0,
            border_width=2.0,
            fill_color=style_manager.get_color_str(ColorRole.LOGO_BACKGROUND),
        )
        logo_pixmap = QPixmap(style_manager.get_app_logo_path())
        scaled_size = int(200 * zoom_factor)
        logo_label.setPixmap(logo_pixmap.scaled(
            scaled_size, scaled_size,
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        ))
        logo_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        logo_label.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        layout.addWidget(logo_label, alignment=Qt.AlignmentFlag.AlignHCenter)
        layout.addSpacing(4)

        # Title with version — gradient matches the logo colours
        title_label = GradientLabel(
            f"Humbug v{CURRENT_VERSION}",
            style_manager.get_color_str(ColorRole.BRAND_GRADIENT_START),
            style_manager.get_color_str(ColorRole.BRAND_GRADIENT_END),
        )
        title_label.setObjectName("titleLabel")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setMinimumHeight(40)
        layout.addWidget(title_label)

        # Description with hyperlink
        desc_label = QLabel(
            f"Visit the <a href='https://github.com/m6r-ai/humbug'"
            f" style='color: {link_color}; text-decoration: none;'>"
            f"Humbug</a> project website to learn more."
        )
        desc_label.setWordWrap(True)
        desc_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        desc_label.setMinimumHeight(40)
        desc_label.linkActivated.connect(open_url)
        layout.addWidget(desc_label)

        min_button_width = int(90 * zoom_factor)

        # Close button with proper styling and sizing
        close_button = QPushButton(strings.close_button)
        close_button.clicked.connect(self.accept)
        close_button.setMinimumWidth(min_button_width)
        close_button.setContentsMargins(8, 8, 8, 8)
        layout.addSpacing(30)  # Add spacing before button
        layout.addWidget(close_button, alignment=Qt.AlignmentFlag.AlignCenter)
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
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)};
                color: {style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)};
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
