"""
Markdown AST visitor to render the AST directly to a QTextDocument.
"""

from PySide6.QtGui import (
    QTextCursor, QTextDocument, QTextCharFormat, QTextBlockFormat,
    QTextListFormat, QFont
)

from humbug.gui.style_manager import StyleManager
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
        self._lists = []
        self.list_level = 0

        self._style_manager = StyleManager()

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
        # If not at start of block, add a new block
        if not self.cursor.atBlockStart():
            print('para: new block')
            self.cursor.insertBlock()
        else:
            print('para: already at start')

        # Process all inline content
        for child in node.children:
            self.visit(child)

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
            print('heading: new block')
            self.cursor.insertBlock()
        else:
            print('heading: already at start')

        # Apply block format (heading level)
        level = min(node.level, 6) - 1  # Convert to 0-based index
        block_format = QTextBlockFormat()
        block_format.setHeadingLevel(level)
        self.cursor.setBlockFormat(block_format)

        saved_format = self.cursor.charFormat()
        char_format = QTextCharFormat(saved_format)
        font_size = (24 - (level * 2)) * self._style_manager.zoom_factor
        char_format.setFontPointSize(font_size)
        char_format.setFontWeight(QFont.Bold)
        self.cursor.setCharFormat(char_format)  # Apply heading character format

        # Process all inline content for the heading
        for child in node.children:
            self.visit(child)

        # Restore the previous format
        self.cursor.setCharFormat(saved_format)

    def visit_MarkdownTextNode(self, node):  # pylint: disable=invalid-name
        """
        Render a text node to the QTextDocument.

        Args:
            node: The text node to render

        Returns:
            None
        """
        self.cursor.insertText(node.content)  # Use the current cursor format

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

        # Create a new format based on the current one but with bold
        bold_format = QTextCharFormat(saved_format)
        bold_format.setFontWeight(QFont.Bold)
        self.cursor.setCharFormat(bold_format)

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

        # Create a new format based on the current one but with italic
        italic_format = QTextCharFormat(saved_format)
        italic_format.setFontItalic(True)
        self.cursor.setCharFormat(italic_format)

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

        # Create a new format based on the current one but with italic
        code_format = QTextCharFormat(saved_format)
        code_format.setFontFamilies(self._style_manager.monospace_font_families)
        self.cursor.setCharFormat(code_format)

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
            print('code: new block')
            self.cursor.insertBlock()
        else:
            print('code: already at start')

        # Create a code block format with monospace font and background
        block_format = QTextBlockFormat()
        block_format.setIndent(1)  # Indent the block

        # Apply the block format
        self.cursor.setBlockFormat(block_format)

        # Apply code formatting and insert text
        saved_format = self.cursor.charFormat()

        code_format = QTextCharFormat(saved_format)
        code_format.setFontFamilies(self._style_manager.monospace_font_families)
        self.cursor.setCharFormat(code_format)

        # Split content by lines and add each in its own block
        lines = node.content.split('\n')

        for i, line in enumerate(lines):
            if i > 0:
                self.cursor.insertBlock(block_format)

            self.cursor.insertText(line)

        # Restore normal format and add a trailing block
        self.cursor.setCharFormat(saved_format)

    def visit_MarkdownOrderedListNode(self, node):  # pylint: disable=invalid-name
        """
        Render an ordered list node to the QTextDocument.

        Args:
            node: The ordered list node to render
        """
        # Insert a new block if needed
        if not self.cursor.atStart():
            print('ol: new block')
            self.cursor.insertBlock()
        else:
            print('ol: already at start')

        orig_block_format = self.cursor.blockFormat()
        orig_char_format = self.cursor.charFormat()

        # Create ordered list format with correct start number
        self.list_level += 1

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.ListDecimal)
        list_format.setStart(node.start)
        list_format.setIndent(self.list_level)

        new_list = self.cursor.createList(list_format)
        self._lists.append(new_list)

        # Process the child nodes (list items)
        for child in node.children:
            self.visit(child)

        self._lists.pop()

        # Exit this list level
        self.list_level -= 1

        if not self.cursor.atBlockStart():
            self.cursor.insertBlock()

        self.cursor.setCharFormat(orig_char_format)
        self.cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownUnorderedListNode(self, node):  # pylint: disable=invalid-name
        """
        Render an unordered list node to the QTextDocument.

        Args:
            node: The unordered list node to render
        """
        # Insert a new block if needed
        if not self.cursor.atStart():
            print('ul: new block')
            self.cursor.insertBlock()
        else:
            print('ul: already at start')

        orig_block_format = self.cursor.blockFormat()
        orig_char_format = self.cursor.charFormat()

        # Create unordered list format
        self.list_level += 1

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.ListDisc)
        list_format.setIndent(self.list_level)

        new_list = self.cursor.createList(list_format)
        self._lists.append(new_list)

        # Process the child nodes (list items)
        for child in node.children:
            self.visit(child)

        self._lists.pop()

        # Exit this list level
        self.list_level -= 1

        if not self.cursor.atBlockStart():
            self.cursor.insertBlock()

        self.cursor.setCharFormat(orig_char_format)
        self.cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownListItemNode(self, node):  # pylint: disable=invalid-name
        """
        Render a list item node to the QTextDocument.

        Args:
            node: The list item node to render

        Returns:
            None
        """
        # Start a new list item by creating a block if not at start of a block
        if not self.cursor.atBlockStart():
            self.cursor.insertBlock()

        self._lists[-1].add(self.cursor.block())

        # Process all inline content for this list item
        for child in node.children:
            self.visit(child)

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
