"""
Markdown AST visitor to render the AST directly to a QTextDocument.
"""

from PySide6.QtGui import (
    QTextCursor, QTextDocument, QTextCharFormat, QTextBlockFormat, 
    QTextListFormat, QFont
)

from humbug.markdown.markdown_ast_node import MarkdownASTVisitor


class ConversationMarkdownRenderer(MarkdownASTVisitor):
    """Visitor that renders the AST directly to a QTextDocument."""
    
    def __init__(self, document: QTextDocument):
        """
        Initialize the document renderer.
        
        Args:
            document: The QTextDocument to render into
        """
        super().__init__()
        self.document = document
        self.cursor = QTextCursor(document)
        self.cursor.movePosition(QTextCursor.Start)
        
        # Maintain list state
        self.current_lists = []  # Stack of QTextList objects
        self.list_level = 0
        
        # Text formats for different elements
        self._init_formats()
    
    def _init_formats(self):
        """Initialize text formats for different markdown elements."""
        # Normal text format
        self.normal_format = QTextCharFormat()
        
        # Bold text format
        self.bold_format = QTextCharFormat()
        self.bold_format.setFontWeight(QFont.Bold)
        
        # Italic text format
        self.italic_format = QTextCharFormat()
        self.italic_format.setFontItalic(True)
        
        # Code format
        self.code_format = QTextCharFormat()
        self.code_format.setFontFamily("Courier")
        
        # Heading formats (h1 to h6)
        self.heading_formats = []
        for i in range(1, 7):
            heading_format = QTextBlockFormat()
            heading_format.setHeadingLevel(i)
            
            char_format = QTextCharFormat()
            # Adjust size based on heading level
            font_size = 20 - (i * 2)  # h1=18pt, h2=16pt, etc.
            char_format.setFontPointSize(font_size)
            char_format.setFontWeight(QFont.Bold)
            
            self.heading_formats.append((heading_format, char_format))
        
        # List formats
        self.ordered_list_format = QTextListFormat()
        self.ordered_list_format.setStyle(QTextListFormat.ListDecimal)
        
        self.unordered_list_format = QTextListFormat()
        self.unordered_list_format.setStyle(QTextListFormat.ListDisc)
    
    def visit_MarkdownDocumentNode(self, node):  # pylint: disable=invalid-name
        """
        Render a document node to the QTextDocument.
        
        Args:
            node: The document node to render
            
        Returns:
            None
        """
        # Clear the document first
        self.cursor.select(QTextCursor.Document)
        self.cursor.removeSelectedText()
        self.cursor.movePosition(QTextCursor.Start)
        
        # Process all children
        for child in node.children:
            self.visit(child)
    
    def visit_MarkdownParagraphNode(self, node):  # pylint: disable=invalid-name
        """
        Render a paragraph node to the QTextDocument.
        
        Args:
            node: The paragraph node to render
            
        Returns:
            None
        """
        # If not at beginning of document, add a new block
        if not (self.cursor.atStart() and self.cursor.atBlockStart()):
            self.cursor.insertBlock()
            
        # Process all inline content
        for child in node.children:
            self.visit(child)
        
        # End with a block if we don't have one yet
        if not self.cursor.atBlockEnd():
            self.cursor.insertBlock()
    
    def visit_MarkdownHeadingNode(self, node):  # pylint: disable=invalid-name
        """
        Render a heading node to the QTextDocument.
        
        Args:
            node: The heading node to render
            
        Returns:
            None
        """
        # Add a new block with heading format
        if not self.cursor.atStart():
            self.cursor.insertBlock()
        
        # Apply block format (heading level)
        level = min(node.level, 6) - 1  # Convert to 0-based index
        block_format, _char_format = self.heading_formats[level]
        self.cursor.setBlockFormat(block_format)

        print("heading")        
        # Apply character format and add text
        for child in node.children:
            self.visit(child)
        
        # End with a block break
        self.cursor.insertBlock()
    
    def visit_MarkdownTextNode(self, node):  # pylint: disable=invalid-name
        """
        Render a text node to the QTextDocument.
        
        Args:
            node: The text node to render
            
        Returns:
            None
        """
        self.cursor.insertText(node.content, self.normal_format)
    
    def visit_MarkdownBoldNode(self, node):  # pylint: disable=invalid-name
        """
        Render a bold node to the QTextDocument.
        
        Args:
            node: The bold node to render
            
        Returns:
            None
        """
        # Save current format, apply bold format, then process children
        saved_format = self.cursor.charFormat()
        self.cursor.setCharFormat(self.bold_format)
        
        for child in node.children:
            self.visit(child)
        
        # Restore previous format
        self.cursor.setCharFormat(saved_format)
    
    def visit_MarkdownEmphasisNode(self, node):  # pylint: disable=invalid-name
        """
        Render an emphasis node to the QTextDocument.
        
        Args:
            node: The emphasis node to render
            
        Returns:
            None
        """
        # Save current format, apply italic format, then process children
        saved_format = self.cursor.charFormat()
        self.cursor.setCharFormat(self.italic_format)
        
        for child in node.children:
            self.visit(child)
        
        # Restore previous format
        self.cursor.setCharFormat(saved_format)
    
    def visit_MarkdownInlineCodeNode(self, node):  # pylint: disable=invalid-name
        """
        Render an inline code node to the QTextDocument.
        
        Args:
            node: The inline code node to render
            
        Returns:
            None
        """
        # Save current format, apply code format, then insert text
        saved_format = self.cursor.charFormat()
        self.cursor.setCharFormat(self.code_format)
        
        self.cursor.insertText(node.content)
        
        # Restore previous format
        self.cursor.setCharFormat(saved_format)
    
    def visit_MarkdownCodeBlockNode(self, node):  # pylint: disable=invalid-name
        """
        Render a code block node to the QTextDocument.
        
        Args:
            node: The code block node to render
            
        Returns:
            None
        """
        # Insert a new block if needed
        if not self.cursor.atStart():
            self.cursor.insertBlock()
        
        # Create a code block format with monospace font and background
        block_format = QTextBlockFormat()
        block_format.setIndent(1)  # Indent the block
        
        # Apply the block format
        self.cursor.setBlockFormat(block_format)
        
        # Apply code formatting and insert text
        saved_format = self.cursor.charFormat()
        self.cursor.setCharFormat(self.code_format)
        
        # Split content by lines and add each in its own block
        lines = node.content.split('\n')
        
        for i, line in enumerate(lines):
            if i > 0:
                self.cursor.insertBlock(block_format)
            self.cursor.insertText(line)
        
        # Restore normal format and add a trailing block
        self.cursor.setCharFormat(saved_format)
        self.cursor.insertBlock()
    
    def visit_MarkdownOrderedListNode(self, node):
        """
        Render an ordered list node to the QTextDocument.
        
        Args:
            node: The ordered list node to render
        """
        # Create ordered list format with correct start number
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.ListDecimal)
        list_format.setStart(node.start)
        
        self._start_list(list_format)
        
        for child in node.children:
            self.visit(child)
            
        self._end_list()

    def visit_MarkdownUnorderedListNode(self, node):
        """
        Render an unordered list node to the QTextDocument.
        
        Args:
            node: The unordered list node to render
        """
        # Create unordered list format
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.ListDisc)
        
        self._start_list(list_format)
        
        for child in node.children:
            self.visit(child)
            
        self._end_list()
    
    def visit_MarkdownListItemNode(self, node):  # pylint: disable=invalid-name
        """
        Render a list item node to the QTextDocument.
        
        Args:
            node: The list item node to render
            
        Returns:
            None
        """
        # Start a new list item by creating a block
        if not self.cursor.atBlockStart():
            self.cursor.insertBlock()
            
        # Add the block to current list
        current_list = self.current_lists[-1]
        current_list.add(self.cursor.block())
        
        # Process all inline content
        for child in node.children:
            self.visit(child)
            
        # If we're the last list item, ensure we're at end of block
        if node == node.parent.children[-1]:
            if not self.cursor.atBlockEnd():
                self.cursor.insertBlock()
                current_list.add(self.cursor.block())  # Add empty block to list
    
    def visit_MarkdownLineBreakNode(self, _node):  # pylint: disable=invalid-name
        """
        Render a line break node to the QTextDocument.
        
        Args:
            node: The line break node to render
            
        Returns:
            None
        """
        # Insert line break character
        self.cursor.insertText("\n")
    
    def _start_list(self, list_format):
        """
        Start a new list with the given format.
        
        Args:
            list_format: The QTextListFormat to use
            
        Returns:
            None
        """
        # Increase the indent level for nested lists
        self.list_level += 1
        
        # Set indentation based on nesting level
        text_format = QTextListFormat(list_format)
        text_format.setIndent(self.list_level)
        
        # Create a new list
        current_list = self.cursor.createList(text_format)
        self.current_lists.append(current_list)
    
    def _end_list(self):
        """
        End the current list.
        
        Returns:
            None
        """
        if self.current_lists:
            self.current_lists.pop()
            self.list_level -= 1
