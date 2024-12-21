"""Menu implementation for Humbug application."""

from PySide6.QtWidgets import QDialog, QHBoxLayout, QVBoxLayout, QLabel, QPushButton, QSizePolicy
from PySide6.QtCore import Qt, QSize

from humbug import format_version


class AboutDialog(QDialog):
    """About dialog for Humbug application."""

    def __init__(self, parent=None):
        """Initialize the About dialog."""
        super().__init__(parent)
        self.setWindowTitle("About Humbug")
        self.setFixedSize(QSize(400, 250))
        self.setup_ui()

    def setup_ui(self):
        """Set up the About dialog UI."""
        layout = QVBoxLayout()

        # Title with version
        title_label = QLabel(f"Humbug v{format_version()}")
        title_label.setAlignment(Qt.AlignCenter)
        title_label.setStyleSheet("font-size: 18px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)

        # Description
        desc_label = QLabel(
            "Humbug is a GUI-based application that allows users to "
            "interact with AI backends through a simple chat interface."
        )
        desc_label.setWordWrap(True)
        desc_label.setAlignment(Qt.AlignCenter)
        desc_label.setStyleSheet("margin: 10px;")
        layout.addWidget(desc_label)

        # Close button
        button_layout = QHBoxLayout()
        close_button = QPushButton("Close")
        close_button.clicked.connect(self.accept)
        close_button.setStyleSheet("padding: 10px 15px;")
        button_layout.addStretch()
        button_layout.addWidget(close_button)
        button_layout.addStretch()

        layout.addLayout(button_layout)

        self.setLayout(layout)
