"""Menu implementation for Humbug application."""

from PySide6.QtWidgets import QDialog, QHBoxLayout, QVBoxLayout, QLabel, QPushButton
from PySide6.QtCore import Qt, QSize

from humbug import format_version


class TempToolTip(QDialog):
    """About dialog for Humbug application."""

    def __init__(self, parent=None):
        """Initialize the About dialog."""
        super().__init__(parent)
        self.setWindowTitle("About Humbug")
        self.setFixedSize(QSize(400, 250))

        layout = QVBoxLayout()

        # Title with version
        title_label = QLabel("What is Temperature?")
        title_label.setAlignment(Qt.AlignCenter)
        title_label.setStyleSheet("font-size: 18px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)

        # Description
        desc_label = QLabel(
            '''In the context of Large Language Models (LLMs), "temperature" is a parameter that controls the randomness of the model's output during text generation. A lower temperature (e.g., close to 0) makes the model more deterministic and conservative, leading to more predictable and repetitive responses, as it favors higher-probability tokens. Conversely, a higher temperature (e.g., above 1) increases randomness and creativity, allowing the model to explore a wider range of possibilities, which can result in more diverse but potentially less coherent outputs. Adjusting the temperature helps balance between creativity and coherence, depending on the desired outcome of the generated text.''')
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
