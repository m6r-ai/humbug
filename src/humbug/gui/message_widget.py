"""Widget for displaying individual chat messages."""

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QSizePolicy, QLabel
)
from PySide6.QtCore import Signal, QSize, Qt
from PySide6.QtGui import QTextCharFormat

from humbug.gui.color_role import ColorRole
from humbug.gui.dynamic_text_edit import DynamicTextEdit
from humbug.gui.markdown_highlighter import MarkdownHighlighter
from humbug.gui.style_manager import StyleManager


class MessageWidget(QFrame):
    """Widget for displaying a single message in the chat history with header."""

    selectionChanged = Signal(bool)

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

        # Get style manager
        self.style_manager = StyleManager()

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
        text_area.setLineWrapMode(DynamicTextEdit.WidgetWidth)
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
        """Set the content and style of the message widget."""
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

        # Set content
        self.text_area.clear()
        cursor = self.text_area.textCursor()
        cursor.setCharFormat(self.formats.get(style, self.formats['user']))
        cursor.insertText(text)

        # Style the content area
        content_color = self._get_background_color(style)
        self.text_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: {content_color};
                color: {self.style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
                selection-background-color: {self.style_manager.get_color_str(ColorRole.SELECTED_TEXT)};
                border: none;
                padding: 8px 8px 8px 8px;
            }}
        """)

        # Style the frame
        self.setStyleSheet(f"""
            QFrame {{
                border: 1px solid {self.style_manager.get_color_str(ColorRole.MESSAGE_HEADER)};
                margin: 0;
                padding: 0;
            }}
        """)

        # Force immediate layout update
        self.updateGeometry()

    def _on_selection_changed(self):
        """Handle selection changes in the text area."""
        has_selection = self.text_area.textCursor().hasSelection()
        self.selectionChanged.emit(has_selection)

    def has_selection(self) -> bool:
        """Check if text is selected in the text area."""
        return self.text_area.textCursor().hasSelection()

    def copy_selection(self):
        """Copy selected text to clipboard."""
        self.text_area.copy()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)

        # Ensure text area width matches our width.  Subtract 2 for left and right borders
        text_area_width = self.width() - 2
        self.text_area.setFixedWidth(text_area_width)

        # Force document width to match the text area width
        self.text_area.document().setTextWidth(self.text_area.viewport().width())

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
