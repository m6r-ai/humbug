"""Widget for displaying individual chat messages."""

import random
from PySide6.QtWidgets import QFrame, QVBoxLayout, QTextEdit, QSizePolicy
from PySide6.QtCore import Qt, Signal, QSize, QTimer
from PySide6.QtGui import QTextCharFormat, QColor

from humbug.gui.markdown_highlighter import MarkdownHighlighter

class DynamicTextEdit(QTextEdit):
    """QTextEdit that automatically adjusts its height to content."""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.document().documentLayout().documentSizeChanged.connect(self._on_content_changed)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        
    def _on_content_changed(self):
        """Update the widget size when content changes."""
        self.updateGeometry()

    def minimumSizeHint(self) -> QSize:
        """Minimum size is the same as size hint."""
        height = max(int(self.document().size().height()), 0)
        width = self.width() if self.width() > 0 else self.minimumWidth()
        return QSize(width, height)

    def sizeHint(self) -> QSize:
        return self.minimumSizeHint()

class MessageWidget(QFrame):
    """Widget for displaying a single message in the chat history."""
    
    selectionChanged = Signal(bool)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setFrameStyle(QFrame.NoFrame)
        
        # Create layout
        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setSpacing(0)

        # Create text display using custom DynamicTextEdit
        self.text_area = DynamicTextEdit(self)
        self.text_area.setReadOnly(True)
        self.text_area.setFrameStyle(QFrame.NoFrame)
        
        # Connect selection change signal
        self.text_area.selectionChanged.connect(self._on_selection_changed)
        
        # Add Markdown highlighter
        self.highlighter = MarkdownHighlighter(self.text_area.document())
        
        # Style formats for different message types
        self.formats = {
            'user': self._create_format('white'),
            'ai': self._create_format('yellow'),
            'system': self._create_format('green'),
            'error': self._create_format('red')
        }
        
        # Generate a random dark background color
        r = random.randint(20, 40)
        g = random.randint(20, 40)
        b = random.randint(20, 40)
        
        # Set styling
        self.text_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: rgb({r}, {g}, {b});
                color: white;
                selection-background-color: #606060;
                border: none;
            }}
        """)
        
        self.layout.addWidget(self.text_area)

        # Set size policies that prevent shrinking
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        self.text_area.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)

    def _create_format(self, color: str) -> QTextCharFormat:
        fmt = QTextCharFormat()
        fmt.setForeground(QColor(color))
        return fmt

    def set_content(self, text: str, style: str):
        self.text_area.clear()
        cursor = self.text_area.textCursor()
        cursor.setCharFormat(self.formats.get(style, self.formats['user']))
        cursor.insertText(text)

    def _on_selection_changed(self):
        has_selection = self.text_area.textCursor().hasSelection()
        self.selectionChanged.emit(has_selection)
        
    def has_selection(self) -> bool:
        return self.text_area.textCursor().hasSelection()
        
    def copy_selection(self):
        self.text_area.copy()
