"""Tab label management for the Humbug application."""

from PySide6.QtWidgets import (
    QWidget, QLabel, QPushButton, QHBoxLayout, QSizePolicy
)
from PySide6.QtCore import Signal, QSize
from PySide6.QtGui import QIcon, QPixmap


class TabLabel(QWidget):
    """Custom widget for tab labels with close button."""

    _close_clicked = Signal()

    def __init__(self, text: str, parent=None):
        """Initialize the tab label widget.

        Args:
            text: The text to display in the tab
            parent: Optional parent widget
        """
        super().__init__(parent)
        self._label_text = text

        # Create SVG close icon
        self.close_icon_svg = """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16">
            <path fill="currentColor" d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/>
        </svg>"""

        # Set size policy to ensure proper expansion
        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Fixed)

        # Create layout with proper spacing
        layout = QHBoxLayout()
        layout.setSpacing(6)  # Increased spacing between items
        layout.setContentsMargins(8, 4, 8, 4)  # Added right margin

        # Add label with size policy
        self.label = QLabel(text)
        self.label.setStyleSheet("color: white;")
        self.label.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Preferred)
        self.label.setMinimumWidth(50)  # Ensure minimum space for text
        layout.addWidget(self.label)

        # Add stretching space to push close button right
        layout.addStretch(1)

        # Create close button with icon
        self.close_button = QPushButton(parent=self)
        self.close_button.setFixedSize(18, 18)
        self.close_button.clicked.connect(self._close_clicked)

        # Create and set the icon
        icon = QIcon()
        for mode, color in [("Normal", "#ffffff"), ("Selected", "#ffffff"), ("Active", "#ffffff")]:
            # Replace currentColor with specific color
            colored_svg = self.close_icon_svg.replace('currentColor', color)
            # Convert SVG string to bytes
            svg_bytes = colored_svg.encode('utf-8')
            # Create pixmap from SVG bytes
            pixmap = QPixmap()
            pixmap.loadFromData(svg_bytes)
            # Add to icon with specific mode
            icon.addPixmap(pixmap, getattr(QIcon, mode))

        self.close_button.setIcon(icon)
        self.close_button.setIconSize(QSize(18, 18))

        self.close_button.setStyleSheet("""
            QPushButton {
                color: white;
                border: none;
                border-radius: 2px;
                background: #404040;
                margin: 0px;
                padding: 0px;
            }
            QPushButton:hover {
                background: #ff4444;
            }
        """)

        # Set size policy for close button
        self.close_button.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Fixed)
        layout.addWidget(self.close_button)

        self.setLayout(layout)

        # Track states
        self.is_current = False
        self.is_hovered = False

        # Enable mouse tracking
        self.setMouseTracking(True)

    def sizeHint(self) -> QSize:
        """Provide size hint for the tab."""
        width = (
            self.label.sizeHint().width() +  # Label width
            self.close_button.sizeHint().width() +  # Button width
            20 +  # Layout spacing and margins
            16   # Extra padding
        )
        height = max(
            self.label.sizeHint().height(),
            self.close_button.sizeHint().height()
        ) + 8  # Vertical padding
        return QSize(width, height)

    def minimumSizeHint(self) -> QSize:
        """Provide minimum size hint for the tab."""
        return QSize(
            self.label.minimumWidth() + self.close_button.width() + 24,
            max(self.label.minimumHeight(), self.close_button.height()) + 8
        )

    def enterEvent(self, event):
        """Handle mouse entering the tab label."""
        super().enterEvent(event)
        self.is_hovered = True
        self._update_close_button()

    def leaveEvent(self, event):
        """Handle mouse leaving the tab label."""
        super().leaveEvent(event)
        self.is_hovered = False
        self._update_close_button()

    def _update_close_button(self):
        """Update close button visibility and style based on current state."""
        visible = self.is_current or self.is_hovered

        # Try both show/hide and setVisible
        if visible:
            self.close_button.show()
        else:
            self.close_button.hide()

        self.close_button.setVisible(visible)

        # Force immediate update
        self.close_button.update()

        style = """
            QPushButton {
                color: white;
                border: none;
                border-radius: 2px;
                background: #404040;
                margin: 0px;
                padding: 0px;
            }
            QPushButton:hover {
                background: #ff4444;
            }
        """
        self.close_button.setStyleSheet(style)

    def set_current(self, is_current: bool):
        """Update the current state of the tab."""
        self.is_current = is_current
        self._update_close_button()

    def setText(self, text: str):
        """Update the tab text.

        Args:
            text: The new text to display
        """
        self._label_text = text
        self.label.setText(text)