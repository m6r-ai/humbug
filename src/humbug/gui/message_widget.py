"""Widget for displaying individual chat messages."""

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QSizePolicy, QLabel
)
from PySide6.QtCore import Signal, QSize
from PySide6.QtGui import QTextCharFormat, QColor

from humbug.gui.dynamic_text_edit import DynamicTextEdit
from humbug.gui.markdown_highlighter import MarkdownHighlighter


class MessageWidget(QFrame):
    """Widget for displaying a single message in the chat history with header."""

    selectionChanged = Signal(bool)
    FRAME_COLOR = "#2a3544"  # Dark bluish-grey for frame and header

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

        # Style formats for different message types - all using white text
        self.formats = {
            'user': self._create_format('white'),
            'ai': self._create_format('white'),
            'system': self._create_format('white'),
            'error': self._create_format('white')
        }

        # Background colors for different message types (content area only)
        self.backgrounds = {
            'user': '#3c3c3c',    # Dark gray for user messages
            'ai': '#282828',      # Darker gray for AI messages
            'system': '#1a3a1a',  # Dark green for system messages
            'error': '#3a1a1a'    # Dark red for error messages
        }

        self.layout.addWidget(self.header)
        self.layout.addWidget(self.text_area)

        # Set size policies that prevent shrinking
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        self.text_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)

    def _create_format(self, color: str) -> QTextCharFormat:
        fmt = QTextCharFormat()
        fmt.setForeground(QColor(color))
        return fmt

    def set_content(self, text: str, style: str):
        # Split into source and content
        if text.startswith("You: "):
            header_text = "You"
            content = text[4:]
        elif text.startswith("AI: "):
            header_text = "Assistant"
            content = text[4:]
        else:
            header_text = "System Message" if style == 'system' else "Error"
            content = text

        # Set header text
        self.header.setText(header_text)

        # Style the header - using frame color
        self.header.setStyleSheet(f"""
            QLabel {{
                background-color: {self.FRAME_COLOR};
                color: white;
                font-weight: bold;
            }}
        """)

        # Set content
        self.text_area.clear()
        cursor = self.text_area.textCursor()
        cursor.setCharFormat(self.formats.get(style, self.formats['user']))
        cursor.insertText(content)

        # Style the content area
        content_color = self.backgrounds.get(style, self.backgrounds['user'])
        self.text_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: {content_color};
                color: white;
                selection-background-color: #606060;
                border: none;
            }}
        """)

        # Style the frame
        self.setStyleSheet(f"""
            QFrame {{
                border: 1px solid {self.FRAME_COLOR};
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
