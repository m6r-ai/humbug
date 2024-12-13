"""Widget for displaying individual chat messages."""

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QSizePolicy, QLabel, QScrollArea
)
from PySide6.QtCore import Signal, QSize, Qt, QPoint
from PySide6.QtGui import QTextCharFormat, QCursor

from humbug.gui.color_role import ColorRole
from humbug.gui.dynamic_text_edit import DynamicTextEdit
from humbug.gui.markdown_highlighter import MarkdownHighlighter
from humbug.gui.style_manager import StyleManager


class MessageWidget(QFrame):
    """Widget for displaying a single message in the chat history with header."""

    selectionChanged = Signal(bool)
    scrollRequested = Signal(QPoint)

    def __init__(self, parent=None, is_input=False):
        """Initialize the message widget.

        Args:
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Box | QFrame.Plain)
        self.setLineWidth(1)
        self.is_input = is_input

        # Create layout
        self.layout = QVBoxLayout(self)
        self.layout.setSpacing(0)  # No spacing between widgets
        self.layout.setContentsMargins(0, 0, 0, 0)  # No margins around layout

        # Create header
        self.header = QLabel(self)
        self.header.setAutoFillBackground(True)
        self.header.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        self.header.setContentsMargins(8, 8, 8, 8)  # Keep some padding inside header for text

        # Create scroll area for content
        self.scroll_area = QScrollArea(self)
        self.scroll_area.setFrameStyle(QFrame.NoFrame)
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Create content area using custom DynamicTextEdit
        self.text_area = self._create_text_area()
        self.text_area.setContentsMargins(8, 8, 8, 8)  # Keep some padding inside content for text

        # Explicitly disable scrollbars
        self.text_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.text_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # Connect selection change signal
        self.text_area.selectionChanged.connect(self._on_selection_changed)

        # Add Markdown highlighter
        self.highlighter = MarkdownHighlighter(self.text_area.document())
        self.highlighter.codeBlockStateChanged.connect(self._on_code_block_state_changed)

        # Get style manager
        self.style_manager = StyleManager()

        # Track current message style
        self._current_style = None

        # Create formats for different message types - all using the primary text color
        self.formats = {
            'user': self._create_format(),
            'ai': self._create_format(),
            'system': self._create_format(),
            'error': self._create_format()
        }

        # Map message types to background color roles
        self.background_roles = {
            'user': ColorRole.MESSAGE_USER,
            'ai': ColorRole.MESSAGE_AI,
            'system': ColorRole.MESSAGE_SYSTEM,
            'error': ColorRole.MESSAGE_ERROR
        }

        self.layout.addWidget(self.header)
        self.layout.addWidget(self.text_area)

        # Set size policies that prevent shrinking
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)

        self.scroll_area.setStyleSheet(f"""
            QScrollBar:horizontal {{
                background: {self.style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND)};
                height: 12px;
            }}
            QScrollBar::handle:horizontal {{
                background: {self.style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE)};
                min-width: 20px;
            }}
            QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """)

    def _create_text_area(self) -> DynamicTextEdit:
        """Create and configure the text area widget.

        Can be overridden by subclasses to customize text area behavior.

        Returns:
            Configured DynamicTextEdit instance
        """
        text_area = DynamicTextEdit(self)
        text_area.setReadOnly(not self.is_input)

        # Ensure text area takes up minimum space needed
        text_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        text_area.setAcceptRichText(False)
        return text_area

    def _create_format(self) -> QTextCharFormat:
        """Create text format using primary text color from StyleManager."""
        fmt = QTextCharFormat()
        fmt.setForeground(self.style_manager.get_color(ColorRole.TEXT_PRIMARY))
        return fmt

    def _get_background_color(self, style: str) -> str:
        """Get the appropriate background color for the message style."""
        role = self.background_roles.get(style, ColorRole.MESSAGE_USER)
        return self.style_manager.get_color_str(role)

    def set_content(self, text: str, style: str):
        """Set content with style, handling incremental updates for AI responses."""
        if style != self._current_style:
            # Style changed - update header and styling
            self._setup_style(style)
            self._current_style = style

            # Full reset needed for style change
            self.text_area.clear()
            self.text_area.set_incremental_text(text, self.formats.get(style, self.formats['user']))
        else:
            # Same style - just update content incrementally
            self.text_area.set_incremental_text(text, self.formats.get(style, self.formats['user']))

    def _setup_style(self, style: str):
        """Set up styling for the message."""
        header_text = {
            'user': "You",
            'ai': "Assistant",
            'system': "System Message",
            'error': "Error"
        }.get(style, "Unknown")

        self.header.setText(header_text)

        # Style the header
        self.header.setStyleSheet(f"""
            QLabel {{
                background-color: {self.style_manager.get_color_str(ColorRole.MESSAGE_HEADER)};
                color: {self.style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
        """)

        # Style the content area
        content_color = self._get_background_color(style)
        self.text_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: {content_color};
                color: {self.style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {self.style_manager.get_color_str(ColorRole.SELECTED_TEXT)};
                border: none;
                padding: 8px;
            }}
        """)

        # Style the frame
        self.setStyleSheet(f"""
            QFrame {{
                border: 1px solid {self.style_manager.get_color_str(ColorRole.MESSAGE_HEADER)};
                margin: 0;
            }}
        """)

    def _on_selection_changed(self):
        """Handle selection changes in the text area."""
        cursor = self.text_area.textCursor()
        has_selection = cursor.hasSelection()

        if has_selection:
            # Emit global mouse position for accurate scroll calculations
            self.scrollRequested.emit(QCursor.pos())

        self.selectionChanged.emit(has_selection)

    def has_selection(self) -> bool:
        """Check if text is selected in the text area."""
        return self.text_area.textCursor().hasSelection()

    def copy_selection(self):
        """Copy selected text to clipboard."""
        self.text_area.copy()

    def _on_code_block_state_changed(self, has_code_block: bool):
        """Handle changes in code block state."""
        self.text_area.set_has_code_block(has_code_block)
        # Ensure proper scroll behavior
        self.updateGeometry()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)

        # Adjust scroll area width to match our width minus borders
        scroll_width = self.width() - 2
        self.scroll_area.setFixedWidth(scroll_width)

        # If we have code blocks, let the text area be as wide as needed
        if self.text_area.has_code_block():
            self.text_area.setMinimumWidth(0)
            self.text_area.setMaximumWidth(16777215)  # Qt's QWIDGETSIZE_MAX
        else:
            # Otherwise, match the scroll area width
            self.text_area.setFixedWidth(scroll_width)

        # Update size after resize
        self.updateGeometry()

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size including header and content."""
        header_height = self.header.sizeHint().height()

        # Get the document height when wrapped to current width
        self.text_area.document().setTextWidth(self.text_area.viewport().width())

        # Add 16 pixels for padding (8px top + 8px bottom)
        content_height = int(self.text_area.document().size().height()) + 16

        # Add 2 pixels for the frame border (1px top + 1px bottom)
        total_height = header_height + content_height + 2
        return QSize(self.width(), total_height)

    def sizeHint(self) -> QSize:
        """Size hint is same as minimum size hint."""
        return self.minimumSizeHint()
