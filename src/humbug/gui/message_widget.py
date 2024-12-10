"""Widget for displaying individual chat messages."""

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QSizePolicy, QLabel
)
from PySide6.QtCore import Signal, QSize
from PySide6.QtGui import QTextCharFormat

from humbug.gui.color_role import ColorRole
from humbug.gui.dynamic_text_edit import DynamicTextEdit
from humbug.gui.markdown_highlighter import MarkdownHighlighter
from humbug.gui.style_manager import StyleManager


class MessageWidget(QFrame):
    """Widget for displaying a single message in the chat history with header."""

    selectionChanged = Signal(bool)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setFrameStyle(QFrame.Box | QFrame.Plain)
        self.setLineWidth(1)

        # Create layout
        self.layout = QVBoxLayout(self)
        self.layout.setSpacing(0)  # No spacing between widgets
        self.layout.setContentsMargins(0, 0, 0, 0)  # No margins around layout

        # Create header
        self.header = QLabel(self)
        self.header.setAutoFillBackground(True)
        self.header.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        self.header.setContentsMargins(8, 4, 8, 4)  # Keep some padding inside header for text

        # Create content area using custom DynamicTextEdit
        self.text_area = DynamicTextEdit(self)
        self.text_area.setReadOnly(True)
        self.text_area.setContentsMargins(8, 8, 8, 8)  # Keep some padding inside content for text

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
        self.text_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)

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
                font-weight: bold;
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
        has_selection = self.text_area.textCursor().hasSelection()
        self.selectionChanged.emit(has_selection)

    def has_selection(self) -> bool:
        return self.text_area.textCursor().hasSelection()

    def copy_selection(self):
        self.text_area.copy()

    def resizeEvent(self, event):
        """Handle resize events."""
        super().resizeEvent(event)
        # Ensure text area width matches our width
        self.text_area.setFixedWidth(self.width())
        # Update size after resize
        self.updateGeometry()

    def minimumSizeHint(self) -> QSize:
        """Calculate minimum size including header and content."""
        header_height = self.header.sizeHint().height()
        content_height = self.text_area.minimumSizeHint().height()
        return QSize(self.width(), header_height + content_height)

    def sizeHint(self) -> QSize:
        """Size hint is same as minimum size hint."""
        return self.minimumSizeHint()
