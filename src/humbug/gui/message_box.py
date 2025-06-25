"""Custom message box dialog with consistent styling and scrollable content."""

from enum import Enum, auto
from typing import List

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton, QPlainTextEdit, QWidget
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap, QKeyEvent

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.language.language_manager import LanguageManager


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
    """Custom message box dialog with consistent styling and scrollable content."""

    def __init__(
        self,
        msg_type: MessageBoxType,
        title: str,
        text: str,
        buttons: List[MessageBoxButton],
        parent: QWidget | None = None,
        destructive: bool = False
    ) -> None:
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

        self._style_manager = StyleManager()
        zoom_factor = self._style_manager.zoom_factor()

        self.setWindowTitle(title)
        self.setModal(True)
        min_widget_width = int(zoom_factor * 400)
        self.setMinimumWidth(min_widget_width)

        self._language_manager = LanguageManager()

        # Create layout with proper spacing
        layout = QVBoxLayout()
        layout.setSpacing(20)
        layout.setContentsMargins(20, 20, 20, 20)

        # Create header layout for icon and text
        header_layout = QHBoxLayout()
        header_layout.setSpacing(16)
        header_layout.setContentsMargins(0, 0, 0, 0)

        # Add icon if appropriate
        icon = self._create_icon(msg_type)
        if icon:
            icon_label = QLabel()
            icon_label.setPixmap(icon)
            header_layout.addWidget(icon_label, alignment=Qt.AlignmentFlag.AlignTop)

        # Add message text using QPlainTextEdit
        self._text_edit = QPlainTextEdit()
        self._text_edit.setPlainText(text)
        self._text_edit.setReadOnly(True)
        self._text_edit.setFrameStyle(0)  # No frame

        # Calculate size based on content
        doc = self._text_edit.document()
        margins = self._text_edit.contentsMargins()
        line_count = doc.lineCount()
        font_metrics = self._text_edit.fontMetrics()
        line_height = font_metrics.lineSpacing()

        # For short messages (<= 3 lines), use fixed height with small margin
        if line_count <= 3:
            content_height = ((line_count + 3) * line_height) + margins.top() + margins.bottom()
            self._text_edit.setFixedHeight(content_height)
            self._text_edit.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        else:
            # For longer messages, set reasonable max height with scrollbar
            content_height = min(30 * line_height, int(self.screen().geometry().height() * 0.8))
            self._text_edit.setMinimumHeight(40)  # Minimum 2 lines
            self._text_edit.setMaximumHeight(content_height)
            self._text_edit.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)

        header_layout.addWidget(self._text_edit, stretch=1)
        layout.addLayout(header_layout)

        # Add buttons
        button_layout = QHBoxLayout()
        button_layout.setSpacing(8)
        button_layout.addStretch()

        self._button_results = {}
        default_button = None
        escape_button = None
        recommended_button = None

        # Determine which button should be recommended (primary action)
        recommended_buttons = [MessageBoxButton.OK, MessageBoxButton.YES, MessageBoxButton.SAVE]

        # If we have multiple options, use the first recommended one that appears
        for button in buttons:
            if button in recommended_buttons:
                recommended_button = button
                break

        zoom_factor = self._style_manager.zoom_factor()
        min_button_width = int(90 * zoom_factor)
        min_button_height = 40
        for button in buttons:
            btn = QPushButton(self._get_button_text(button))
            btn.setMinimumWidth(min_button_width)
            btn.setMinimumHeight(min_button_height)
            self._button_results[btn] = button
            btn.clicked.connect(self._handle_button)

            # Apply recommended styling if this is our primary action button
            if button == recommended_button:
                btn.setProperty("recommended", not destructive)

            button_layout.addWidget(btn)

            # Set default and escape buttons appropriately
            if button in (MessageBoxButton.OK, MessageBoxButton.YES):
                default_button = btn

            elif button in (MessageBoxButton.CANCEL, MessageBoxButton.NO):
                escape_button = btn

        # If no default button set, use first button
        if not default_button and buttons:
            default_button = next(iter(self._button_results.keys()))

        if default_button:
            default_button.setDefault(True)

        # Store escape button for key handling
        self._escape_button = escape_button

        # Add spacing before buttons only if we have a scrollbar
        if line_count > 2:
            layout.addSpacing(12)

        button_layout.addStretch()
        layout.addLayout(button_layout)
        self.setLayout(layout)

        # Apply styling
        self._handle_style_changed()

        # Store result
        self.result_button: MessageBoxButton = MessageBoxButton.OK

    def _create_icon(self, msg_type: MessageBoxType) -> QPixmap | None:
        """Create appropriate icon for message type."""
        # Map message types to icon names
        icon_names = {
            MessageBoxType.INFORMATION: "info",
            MessageBoxType.WARNING: "warning",
            MessageBoxType.CRITICAL: "critical",
            MessageBoxType.QUESTION: "question"
        }

        icon_name = icon_names.get(msg_type)
        if not icon_name:
            return None

        icon_path = self._style_manager.get_icon_path(icon_name)
        return self._style_manager.scale_icon(icon_path, 64)

    def _get_button_text(self, button: MessageBoxButton) -> str:
        """Get display text for button type."""
        strings = self._language_manager.strings()
        return {
            MessageBoxButton.OK: strings.ok,
            MessageBoxButton.CANCEL: strings.cancel,
            MessageBoxButton.YES: strings.yes,
            MessageBoxButton.NO: strings.no,
            MessageBoxButton.SAVE: strings.save,
            MessageBoxButton.DISCARD: strings.discard
        }[button]

    def _handle_button(self) -> None:
        """Handle button clicks."""
        button = self.sender()
        if not isinstance(button, QPushButton):
            return

        self.result_button = self._button_results[button]
        self.accept()

    def keyPressEvent(self, arg__1: QKeyEvent) -> None:
        """Handle key events, specifically for Escape key."""
        if arg__1.key() == Qt.Key.Key_Escape and self._escape_button:
            self.result_button = self._button_results[self._escape_button]
            self.reject()

        return super().keyPressEvent(arg__1)

    def _handle_style_changed(self) -> None:
        """Update styling when application style changes."""
        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        base_font_size = style_manager.base_font_size()

        # Update font sizes
        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)

        # Apply scrollbar styling
        self._text_edit.setStyleSheet(f"""
            QPlainTextEdit {{
                border: none;
                background-color: {style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)};
                color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                font-size: {base_font_size * zoom_factor}pt;
            }}
            QScrollBar:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                width: 12px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-height: 20px;
            }}
            QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical {{
                background: none;
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)

        # Apply consistent dialog styling
        self.setStyleSheet(self._style_manager.get_dialog_stylesheet())

    @classmethod
    def show_message(
        cls,
        parent: QWidget | None,
        msg_type: MessageBoxType,
        title: str,
        text: str,
        buttons: List[MessageBoxButton] | None = None,
        destructive: bool = False
    ) -> MessageBoxButton:
        """
        Show a message box and return the clicked button.

        Args:
            parent: Parent widget
            msg_type: Type of message box
            title: Dialog title
            text: Message text
            buttons: List of buttons (defaults to just OK)
            dangerous: If True, use a destructive button style

        Returns:
            The MessageBoxButton that was clicked
        """
        if buttons is None:
            buttons = [MessageBoxButton.OK]

        dialog = cls(msg_type, title, text, buttons, parent, destructive)
        dialog.exec()
        return dialog.result_button
