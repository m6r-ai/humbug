"""Widget for displaying individual conversation messages."""

from datetime import datetime
from PySide6.QtWidgets import QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget
from PySide6.QtCore import Signal, Qt, QPoint
from PySide6.QtGui import QCursor

from humbug.conversation.message_source import MessageSource
from humbug.gui.conversation_highlighter import ConversationHighlighter
from humbug.gui.conversation_text_edit import ConversationTextEdit
from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class MessageWidget(QFrame):
    """Widget for displaying a single message in the conversation history with header."""

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
        self._is_input = is_input

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        self._layout.setSpacing(8)
        self._layout.setContentsMargins(8, 8, 8, 8)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(1, 1, 1, 1)
        self._header_layout.setSpacing(0)

        # Create role and timestamp labels
        self._role_label = QLabel(self)
        self._timestamp_label = QLabel(self)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addWidget(self._timestamp_label)
        self._header_layout.addStretch()

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Create content area using custom ConversationTextEdit
        self._text_area = self._create_text_area()
        self._text_area.setReadOnly(not self._is_input)

        # Add text area to main layout
        self._layout.addWidget(self._text_area)

        # Connect selection change signal
        self._text_area.selectionChanged.connect(self._on_selection_changed)
        self._text_area.mouseReleased.connect(self._on_mouse_released)

        # Add conversation highlighter
        self._highlighter = ConversationHighlighter(self._text_area.document())
        self._highlighter.codeBlockStateChanged.connect(self._on_code_block_state_changed)

        # Get style manager
        self._style_manager = StyleManager()

        # Track current message style
        self._current_style: MessageSource = None

        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed(self._style_manager.zoom_factor)

    def _create_text_area(self) -> ConversationTextEdit:
        """Create and configure the text area.
        
        Returns:
            Configured ConversationTextEdit instance
        """
        text_area = ConversationTextEdit()
        text_area.setAcceptRichText(False)
        text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        text_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        return text_area

    def _on_mouse_released(self):
        """Handle mouse release from text area."""
        self.mouseReleased.emit()

    def set_content(self, text: str, style: MessageSource, timestamp: datetime):
        """Set content with style, handling incremental updates for AI responses.
        
        Args:
            text: The message text content
            style: The style type ('user', 'ai', 'system', or 'error')
            timestamp: datetime object for the message timestamp
        """
        if style != self._current_style:
            # Style changed - update header and styling
            if self._is_input:
                self._timestamp_label.setText("")  # No timestamp for input
            else:
                # For history messages, show role with timestamp
                role_text = {
                    MessageSource.USER: "You",
                    MessageSource.AI: "Assistant",
                    MessageSource.SYSTEM: "System Message"
                }.get(style, "Unknown")

                self._role_label.setText(role_text)
                timestamp_str = timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
                self._timestamp_label.setText(f" @ {timestamp_str}")

            self._current_style = style
            self._handle_style_changed(self._style_manager.zoom_factor)

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

    def _handle_style_changed(self, factor: float):
        """Handle the style changing"""
        font = self.font()
        base_font_size = self._style_manager.base_font_size
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        # Map message types to role colors
        role_colours = {
            MessageSource.USER: ColorRole.MESSAGE_USER,
            MessageSource.AI: ColorRole.MESSAGE_AI,
            MessageSource.SYSTEM: ColorRole.MESSAGE_SYSTEM
        }

        role = role_colours.get(self._current_style, ColorRole.MESSAGE_USER)
        label_color = self._style_manager.get_color_str(role)

        # Role label styling (bold)
        self._role_label.setFont(font)
        self._role_label.setStyleSheet(f"""
            QLabel {{
                font-weight: bold;
                color: {label_color};
                margin: 0;
                padding: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

        # Timestamp label styling (normal weight)
        self._timestamp_label.setFont(font)
        self._timestamp_label.setStyleSheet(f"""
            QLabel {{
                font-weight: normal;
                color: {label_color};
                padding: 0;
                margin: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)


        # Header widget styling
        self._header.setStyleSheet(f"""
            QWidget {{
                border: none;
                border-radius: 0;
                padding: 1px;
                margin: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
        """)

        # Content area styling
        self._text_area.setFont(font)
        self._text_area.setStyleSheet(f"""
            QTextEdit {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {self._style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
                border: none;
                border-radius: 0;
                padding: 1px;
                margin: 0;
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            }}
            QScrollBar:horizontal {{
                height: 12px;
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
            }}
            QScrollBar::handle:horizontal {{
                background: {self._style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            QScrollBar::add-page:horizontal, QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """)

        # Main frame styling
        self.setStyleSheet(f"""
            QFrame {{
                background-color: {self._style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
                margin: 0;
                border-radius: 8px;
            }}
        """)
        self._highlighter.rehighlight()
