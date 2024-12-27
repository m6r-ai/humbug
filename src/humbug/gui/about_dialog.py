"""Dialog box for About Humbug."""

from PySide6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton
from PySide6.QtCore import Qt

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
        layout.setContentsMargins(20, 20, 20, 20)

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
        layout.addSpacing(8)  # Add spacing before button
        layout.addWidget(close_button, alignment=Qt.AlignCenter)

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
