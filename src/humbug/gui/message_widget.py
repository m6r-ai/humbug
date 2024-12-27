"""Widget for displaying individual chat messages."""

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QSizePolicy, QLabel
)
from PySide6.QtCore import Signal, Qt, QPoint
from PySide6.QtGui import QCursor

from humbug.gui.chat_highlighter import ChatHighlighter
from humbug.gui.chat_text_edit import ChatTextEdit
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class MessageWidget(QFrame):
    """Widget for displaying a single message in the chat history with header."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)
    mouseReleased = Signal()

    def __init__(self, parent=None, is_input=False):
        """Initialize the message widget.

        Args:
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Box | QFrame.Plain)
        self.setLineWidth(1)
        self._is_input = is_input

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(0)  # No spacing between widgets
        self._layout.setContentsMargins(0, 0, 0, 0)  # No margins around layout

        # Create header
        self._header = QLabel(self)
        self._header.setAutoFillBackground(True)
        self._header.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Minimum)
        self._header.setContentsMargins(8, 8, 8, 8)  # Keep some padding inside header for text

        # Create content area using custom ChatTextEdit
        self._text_area = ChatTextEdit()
        self._text_area.setReadOnly(not self._is_input)

        # Ensure text area takes up minimum space needed
        self._text_area.setAcceptRichText(False)
        self._text_area.setContentsMargins(8, 8, 8, 8)  # Keep some padding inside content for text

        # Explicitly disable scrollbars
        self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self._text_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Add widgets to layout in correct order
        self._layout.addWidget(self._header)
        self._layout.addWidget(self._text_area)

        # Connect selection change signal
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        # Add chat highlighter
        self._highlighter = ChatHighlighter(self._text_area.document())
        self._highlighter.codeBlockStateChanged.connect(self._on_code_block_state_changed)

        # Get style manager
        self._style_manager = StyleManager()

        # Track current message style
        self._current_style = None

        # Map message types to background color roles
        self.background_roles = {
            'user': ColorRole.MESSAGE_BACKGROUND_USER,
            'ai': ColorRole.MESSAGE_BACKGROUND_AI,
            'system': ColorRole.MESSAGE_BACKGROUND_SYSTEM,
            'error': ColorRole.MESSAGE_BACKGROUND_ERROR
        }

    def _on_mouse_released(self):
        """Handle mouse release from text area."""
        self.mouseReleased.emit()

    def _get_background_color(self, style: str) -> str:
        """Get the appropriate background color for the message style."""
        role = self.background_roles.get(style, ColorRole.MESSAGE_BACKGROUND_USER)
        return self._style_manager.get_color_str(role)

    def set_content(self, text: str, style: str):
        """Set content with style, handling incremental updates for AI responses."""
        if style != self._current_style:
            # Style changed - update header and styling
            header_text = {
                'user': "You",
                'ai': "Assistant",
                'system': "System Message",
                'error': "Error"
            }.get(style, "Unknown")

            self._current_style = style
            self._header.setText(header_text)
            self.handle_style_changed()

            # Full reset needed for style change
            self._text_area.clear()

        self._text_area.set_incremental_text(text)

    def _on_selection_changed(self):
        """Handle selection changes in the text area."""
        cursor = self._text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection:
            # Emit global mouse position for accurate scroll calculations
            self.scrollRequested.emit(QCursor.pos())

        self.selectionChanged.emit(has_selection)

    def has_selection(self) -> bool:
        """Check if text is selected in the text area."""
        return self._text_area.textCursor().hasSelection()

    def copy_selection(self):
        """Copy selected text to clipboard."""
        self._text_area.copy()

    def _on_code_block_state_changed(self, has_code_block: bool):
        """Handle changes in code block state."""
        self._text_area.set_has_code_block(has_code_block)
        # Ensure proper scroll behavior
        self.updateGeometry()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)

        # If we have code blocks, allow horizontal scrolling
        if self._text_area.has_code_block():
            self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        else:
            self._text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

    def clear_selection(self):
        """Clear any text selection in this message."""
        cursor = self._text_area.textCursor()
        cursor.clearSelection()
        self._text_area.setTextCursor(cursor)

    @property
    def is_ai(self) -> bool:
        """Check if this is an AI response message."""
        return self._current_style == 'ai'

    def handle_style_changed(self):
        """Handle the style changing"""
        self._header.setStyleSheet(f"""
            QLabel {{
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HEADER)};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
        """)

        content_color = self._get_background_color(self._current_style)
        self._text_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: {content_color};
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                padding: 8px;
            }}
            QScrollBar:horizontal {{
                height: 12px;
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            QScrollBar::handle:horizontal {{
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """)

        self.setStyleSheet(f"""
            QFrame {{
                border: 1px solid {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND_HEADER)};
                margin: 0;
            }}
        """)

        self._highlighter.rehighlight()
