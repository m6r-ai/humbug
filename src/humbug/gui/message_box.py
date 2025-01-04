"""Message box dialog with consistent styling.

Example usage:
    result = MessageBox.show_message(
        self,
        MessageBoxType.QUESTION,
        "Save Changes?",
        "Do you want to save your changes?",
        [MessageBoxButton.SAVE, MessageBoxButton.DISCARD, MessageBoxButton.CANCEL]
    )
    if result == MessageBoxButton.SAVE:
        self.save()
    elif result == MessageBoxButton.DISCARD:
        self.close()
"""

from enum import Enum, auto
from typing import Optional, List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton
)
from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QPixmap, QPainter

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class MessageBoxType(Enum):
    """Types of message box with corresponding icon and title."""
    INFORMATION = auto()
    WARNING = auto()
    CRITICAL = auto()
    QUESTION = auto()


class MessageBoxButton(Enum):
    """Standard message box buttons."""
    OK = auto()
    CANCEL = auto()
    YES = auto()
    NO = auto()
    SAVE = auto()
    DISCARD = auto()


class MessageBox(QDialog):
    """Custom message box dialog with consistent styling."""

    def __init__(self, msg_type: MessageBoxType, title: str, text: str,
                 buttons: List[MessageBoxButton], parent=None):
        """
        Initialize the message box.

        Args:
            msg_type: Type of message box (affects icon and style)
            title: Title for the dialog
            text: Main message text
            buttons: List of buttons to display
            parent: Optional parent widget
        """
        super().__init__(parent)
        self.setWindowTitle(title)
        self.setModal(True)
        self.setMinimumWidth(400)

        # Get style manager for consistent styling
        self._style_manager = StyleManager()

        # Create layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(12)
        layout.setContentsMargins(20, 20, 20, 20)

        # Create header with icon and text
        header_layout = QHBoxLayout()
        header_layout.setSpacing(12)

        # Add icon if appropriate
        icon = self._create_icon(msg_type)
        if icon:
            icon_label = QLabel()
            icon_label.setPixmap(icon)
            header_layout.addWidget(icon_label, alignment=Qt.AlignTop)

        # Add message text
        message_label = QLabel(text)
        message_label.setWordWrap(True)
        message_label.setMinimumHeight(40)
        header_layout.addWidget(message_label, stretch=1)

        layout.addLayout(header_layout)
        layout.addStretch()

        # Add buttons
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)

        self._button_results = {}
        default_button = None
        escape_button = None

        for button in buttons:
            btn = QPushButton(self._get_button_text(button))
            btn.setMinimumSize(QSize(90, 40))
            btn.setContentsMargins(8, 8, 8, 8)
            self._button_results[btn] = button
            btn.clicked.connect(self._handle_button)
            button_layout.addWidget(btn)

            # Set default and escape buttons appropriately
            if button == MessageBoxButton.OK or button == MessageBoxButton.YES:
                default_button = btn
            elif button == MessageBoxButton.CANCEL or button == MessageBoxButton.NO:
                escape_button = btn

        # If no default button set, use first button
        if not default_button and buttons:
            default_button = next(iter(self._button_results.keys()))

        if default_button:
            default_button.setDefault(True)

        # Store escape button for key handling
        self._escape_button = escape_button

        layout.addLayout(button_layout)
        self.setLayout(layout)

        # Apply styling
        self._handle_style_changed()

        # Store result
        self.result_button: Optional[MessageBoxButton] = None

    def _create_icon(self, msg_type: MessageBoxType) -> Optional[QPixmap]:
        """Create appropriate icon for message type."""
        size = int(32 * self._style_manager.zoom_factor)
        pixmap = QPixmap(size, size)
        pixmap.fill(Qt.transparent)

        painter = QPainter(pixmap)
        painter.setRenderHint(QPainter.Antialiasing)

        # Get appropriate color
        icon_color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
        pen = painter.pen()
        pen.setColor(icon_color)
        painter.setPen(pen)

        # Draw appropriate icon
        if msg_type == MessageBoxType.INFORMATION:
            # Draw info circle with 'i'
            pen.setWidth(2)
            painter.setPen(pen)
            painter.drawEllipse(4, 4, size-8, size-8)
            painter.drawText(size//2-2, size//2+4, "i")
        elif msg_type == MessageBoxType.WARNING:
            # Draw warning triangle
            pen.setWidth(2)
            painter.setPen(pen)
            points = [
                (size//2, 4),
                (size-4, size-4),
                (4, size-4)
            ]
            painter.drawPolygon(*points)
            painter.drawText(size//2-2, size-8, "!")
        elif msg_type == MessageBoxType.CRITICAL:
            # Draw X in circle
            pen.setWidth(2)
            painter.setPen(pen)
            painter.drawEllipse(4, 4, size-8, size-8)
            margin = size//4
            painter.drawLine(margin, margin, size-margin, size-margin)
            painter.drawLine(size-margin, margin, margin, size-margin)
        elif msg_type == MessageBoxType.QUESTION:
            # Draw question mark in circle
            pen.setWidth(2)
            painter.setPen(pen)
            painter.drawEllipse(4, 4, size-8, size-8)
            painter.drawText(size//2-4, size//2+4, "?")

        painter.end()
        return pixmap

    def _get_button_text(self, button: MessageBoxButton) -> str:
        """Get display text for button type."""
        return {
            MessageBoxButton.OK: "OK",
            MessageBoxButton.CANCEL: "Cancel",
            MessageBoxButton.YES: "Yes",
            MessageBoxButton.NO: "No",
            MessageBoxButton.SAVE: "Save",
            MessageBoxButton.DISCARD: "Discard"
        }[button]

    def _handle_button(self):
        """Handle button clicks."""
        button = self.sender()
        self.result_button = self._button_results[button]
        self.accept()

    def keyPressEvent(self, event):
        """Handle key events, specifically for Escape key."""
        if event.key() == Qt.Key_Escape and self._escape_button:
            self.result_button = self._button_results[self._escape_button]
            self.reject()
        else:
            super().keyPressEvent(event)

    def _handle_style_changed(self) -> None:
        """Update styling when application style changes."""
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor
        base_font_size = style_manager.base_font_size

        # Update font sizes
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

        # Apply consistent dialog styling
        self.setStyleSheet(f"""
            QDialog {{
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
            }}
            QLabel {{
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QPushButton {{
                background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                border: none;
                border-radius: 4px;
                padding: 8px;
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

    @classmethod
    def show_message(cls, parent, msg_type: MessageBoxType, title: str, text: str,
                    buttons: List[MessageBoxButton] = None) -> MessageBoxButton:
        """
        Show a message box and return the clicked button.

        Args:
            parent: Parent widget
            msg_type: Type of message box
            title: Dialog title
            text: Message text
            buttons: List of buttons (defaults to just OK)

        Returns:
            The MessageBoxButton that was clicked
        """
        if not buttons:
            buttons = [MessageBoxButton.OK]

        dialog = cls(msg_type, title, text, buttons, parent)
        dialog.exec()
        return dialog.result_button
