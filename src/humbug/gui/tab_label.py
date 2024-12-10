"""Tab label management for the Humbug application."""

from PySide6.QtWidgets import (
    QWidget, QLabel, QPushButton, QHBoxLayout, QSizePolicy
)
from PySide6.QtCore import Signal, QSize, Qt
from PySide6.QtGui import QIcon, QPixmap, QPainter, QColor

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


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
        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Fixed)

        layout = QHBoxLayout()
        layout.setSpacing(6)
        layout.setContentsMargins(8, 4, 8, 4)

        # Add label with size policy
        self.label = QLabel(text)
        self.label.setStyleSheet("color: white;")
        self.label.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Preferred)
        self.label.setMinimumWidth(50)
        layout.addWidget(self.label)

        # Add stretching space to push close button right
        layout.addStretch(1)

        self.close_button = QPushButton(parent=self)
        self.close_button.setFixedSize(18, 18)
        self.close_button.clicked.connect(self._close_clicked)

        # Create a close icon
        icon = QIcon()
        for state in ["normal", "hover"]:
            base_size = 64  # Work at 4x resolution
            pixmap = QPixmap(base_size, base_size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Set up pen
            pen = painter.pen()
            pen.setWidth(4)  # Thicker line for scaling
            pen.setColor(QColor("white"))
            painter.setPen(pen)

            # Draw at larger size
            margin = base_size // 4
            painter.drawLine(margin, margin, base_size-margin, base_size-margin)
            painter.drawLine(base_size-margin, margin, margin, base_size-margin)

            painter.end()

            # Scale down to target size for smoother lines
            scaled_pixmap = pixmap.scaled(16, 16, Qt.KeepAspectRatio, Qt.SmoothTransformation)
            icon.addPixmap(scaled_pixmap, QIcon.Normal if state == "normal" else QIcon.Active)

        self.close_button.setIcon(icon)
        self.close_button.setIconSize(QSize(16, 16))

        style_manager = StyleManager()
        self.close_button.setStyleSheet(f"""
            QPushButton {{
                color: white;
                border: none;
                border-radius: 2px;
                background: transparent;
                margin: 0px;
                padding: 0px;
            }}
            QPushButton:hover {{
                background: {style_manager.get_color_str(ColorRole.CLOSE_BUTTON_HOVER)};
            }}
        """)

        self.close_button.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Fixed)
        layout.addWidget(self.close_button)
        self.setLayout(layout)

        self.is_current = False
        self.is_hovered = False
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

        style_manager = StyleManager()
        style = f"""
            QPushButton {{
                color: white;
                border: none;
                border-radius: 2px;
                background: {style_manager.get_color_str(ColorRole.CLOSE_BUTTON_NORMAL)};
                margin: 0px;
                padding: 0px;
            }}
            QPushButton:hover {{
                background: {style_manager.get_color_str(ColorRole.CLOSE_BUTTON_HOVER)};
            }}
        """
        self.close_button.setStyleSheet(style)

    def set_current(self, is_current: bool):
        """Update the current state of the tab."""
        self.is_current = is_current
        self._update_close_button()
