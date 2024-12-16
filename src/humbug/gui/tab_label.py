"""Tab label management for the Humbug application."""

from PySide6.QtWidgets import (
    QWidget, QLabel, QToolButton, QHBoxLayout, QSizePolicy
)
from PySide6.QtCore import Signal, QSize, Qt
from PySide6.QtGui import QIcon, QPixmap, QPainter, QColor

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class TabLabel(QWidget):
    """Custom widget for tab labels with close button."""

    close_clicked = Signal()

    def __init__(self, text: str, parent=None):
        """Initialize the tab label widget.

        Args:
            text: The text to display in the tab
            parent: Optional parent widget
        """
        super().__init__(parent)

        self.is_current = False
        self.is_hovered = False

        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Fixed)

        layout = QHBoxLayout(self)
        self.setLayout(layout)
        layout.setSpacing(6)
        layout.setContentsMargins(8, 4, 8, 4)

        # Add label with size policy
        self.label = QLabel(text)
        self.label.setStyleSheet("color: white;")
        layout.addWidget(self.label)

        # Add stretching space to push close button right
        layout.addStretch()

        self._close_button = QToolButton(parent=self)
        self._close_button.setFixedSize(18, 18)
        self._close_button.setCursor(Qt.PointingHandCursor)
        self._close_button.clicked.connect(self.close_clicked)

        self._visible_close_icon = self._create_visible_close_icon()
        self._invisible_close_icon = self._create_invisible_close_icon()

        self._close_button.setIconSize(QSize(16, 16))
        self._close_button.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Fixed)
        self._update_close_button()
        layout.addWidget(self._close_button)

        self.setMouseTracking(True)

    def _create_visible_close_icon(self) -> QIcon:
        """Create and set the close icon for the button."""
        icon = QIcon()
        base_size = 64  # High resolution for better scaling
        pixmap = QPixmap(base_size, base_size)
        pixmap.fill(Qt.transparent)

        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.Antialiasing)
        pen = painter.pen()
        pen.setWidth(4)  # Thicker line for scaling
        pen.setColor(QColor("white"))
        painter.setPen(pen)

        margin = base_size // 4
        painter.drawLine(margin, margin, base_size - margin, base_size - margin)
        painter.drawLine(base_size - margin, margin, margin, base_size - margin)
        painter.end()

        # Scale down to target size
        scaled_pixmap = pixmap.scaled(16, 16, Qt.KeepAspectRatio, Qt.SmoothTransformation)
        icon.addPixmap(scaled_pixmap, QIcon.Normal, QIcon.Off)
        icon.addPixmap(scaled_pixmap, QIcon.Active, QIcon.Off)

        return icon

    def _create_invisible_close_icon(self) -> QIcon:
        transparent_pixmap = QPixmap(16, 16)
        transparent_pixmap.fill(Qt.transparent)
        transparent_icon = QIcon(transparent_pixmap)

        return transparent_icon

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
        """Update close button appearance based on current state."""
        visible = self.is_current or self.is_hovered

        # Update the stylesheet based on visibility
        style_manager = StyleManager()
        if visible:
            # Visible state - use different background colors based on tab state
            base_color = (ColorRole.TAB_ACTIVE if self.is_current
                        else ColorRole.TAB_HOVER)
            style = f"""
                QToolButton {{
                    border: none;
                    outline: none;
                    padding: 0px;
                    background: {style_manager.get_color_str(base_color)};
                }}
                QToolButton:hover {{
                    background: {style_manager.get_color_str(ColorRole.CLOSE_BUTTON_HOVER)};
                }}
            """
            self._close_button.setIcon(self._visible_close_icon)
            self._close_button.setCursor(Qt.PointingHandCursor)
            self._close_button.setToolTip("Close Tab")
        else:
            # Invisible state - hide the button
            style = f"""
                QToolButton {{
                    border: none;
                    outline: none;
                    padding: 0px;
                    background: {style_manager.get_color_str(ColorRole.TAB_INACTIVE)};
                }}
            """
            self._close_button.setIcon(self._invisible_close_icon)
            self._close_button.setCursor(Qt.ArrowCursor)
            self._close_button.setToolTip("")

        self._close_button.setStyleSheet(style)

    def set_current(self, is_current: bool):
        """Update the current state of the tab."""
        self.is_current = is_current
        self._update_close_button()
