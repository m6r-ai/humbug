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

        self._is_current = False
        self._is_hovered = False
        self._style_manager = StyleManager()

        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)

        self._layout = QHBoxLayout(self)
        self.setLayout(self._layout)

        # Add label with size policy
        self._label = QLabel(text)
        self._label.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        self._layout.addWidget(self._label)

        self._close_button = QToolButton()
        self._close_button.setCursor(Qt.PointingHandCursor)
        self._close_button.clicked.connect(self.close_clicked)
        self._close_button.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        self._layout.addWidget(self._close_button)

        self.handle_style_changed(self._style_manager.zoom_factor)

        self.setMouseTracking(True)

    def _create_visible_close_icon(self) -> QIcon:
        """Create and set the close icon for the button."""
        icon = QIcon()
        base_size = 64  # High resolution for scaling
        pixmap = QPixmap(base_size, base_size)
        pixmap.fill(Qt.transparent)

        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.Antialiasing)
        pen = painter.pen()
        pen.setWidth(4)
        pen.setColor(QColor(self._style_manager.get_color(ColorRole.TEXT_PRIMARY)))
        painter.setPen(pen)

        margin = base_size // 4
        painter.drawLine(margin, margin, base_size - margin, base_size - margin)
        painter.drawLine(base_size - margin, margin, margin, base_size - margin)
        painter.end()

        # Scale to current zoom level
        target_size = self._style_manager.get_scaled_size(16)
        scaled_pixmap = pixmap.scaled(target_size, target_size, Qt.KeepAspectRatio, Qt.SmoothTransformation)
        icon.addPixmap(scaled_pixmap, QIcon.Normal, QIcon.Off)
        icon.addPixmap(scaled_pixmap, QIcon.Active, QIcon.Off)

        return icon

    def _create_invisible_close_icon(self) -> QIcon:
        """Create a transparent icon for the inactive state."""
        size = self._style_manager.get_scaled_size(16)
        transparent_pixmap = QPixmap(size, size)
        transparent_pixmap.fill(Qt.transparent)
        return QIcon(transparent_pixmap)

    def handle_style_changed(self, factor: float):
        """Handle style changes from StyleManager.

        Args:
            factor: New zoom factor
        """
        self._label.setStyleSheet(f"color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)}")

        self._update_font_size()

        # Update close button size
        button_size = 18 * factor
        self._close_button.setFixedSize(button_size, button_size)

        # Update icon size
        icon_size = 16 * factor
        self._close_button.setIconSize(QSize(icon_size, icon_size))

        # Recreate icons at new size
        self._visible_close_icon = self._create_visible_close_icon()
        self._invisible_close_icon = self._create_invisible_close_icon()

        # Update layout margins and spacing
        self._layout.setSpacing(6 * factor)
        margins = 8 * factor
        v_margins = 4 * factor
        self._layout.setContentsMargins(margins, v_margins, margins, v_margins)

        self._update_close_button()

        self.adjustSize()

    def _update_font_size(self):
        """Update the label font size based on current zoom factor."""
        font = self._label.font()
        base_size = self._style_manager.base_font_size
        scaled_size = self._style_manager.get_scaled_size(base_size)
        font.setPointSize(scaled_size)
        self._label.setFont(font)

    def enterEvent(self, event):
        """Handle mouse entering the tab label."""
        super().enterEvent(event)
        self._is_hovered = True
        self._update_close_button()

    def leaveEvent(self, event):
        """Handle mouse leaving the tab label."""
        super().leaveEvent(event)
        self._is_hovered = False
        self._update_close_button()

    def _update_close_button(self):
        """Update close button appearance based on current state."""
        visible = self._is_current or self._is_hovered

        style_manager = StyleManager()
        if visible:
            base_color = (ColorRole.TAB_ACTIVE if self._is_current
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
        self._is_current = is_current
        self._update_close_button()
