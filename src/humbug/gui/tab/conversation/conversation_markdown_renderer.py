"""
Markdown AST visitor to render the AST directly to a QTextDocument.
"""

from PySide6.QtGui import (
    QTextCursor, QTextDocument, QTextCharFormat, QTextBlockFormat,
    QTextListFormat, QFont, QFontMetricsF
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.markdown.markdown_ast_node import (
    MarkdownASTVisitor, MarkdownOrderedListNode, MarkdownUnorderedListNode
)


class ConversationMarkdownRenderer(MarkdownASTVisitor):
    """Visitor that renders the AST directly to a QTextDocument."""

    def __init__(self, document: QTextDocument):
        """
        Initialize the document renderer.

        Args:
            document: The QTextDocument to render into
        """
        super().__init__()
        self._document = document
        self._cursor = QTextCursor(document)
        self._cursor.movePosition(QTextCursor.Start)
        self._orig_block_format = self._cursor.blockFormat()

        self._lists = []
        self._list_level = 0

        self._style_manager = StyleManager()

    def visit_MarkdownDocumentNode(self, node):  # pylint: disable=invalid-name
        """
        Render a document node to the QTextDocument.

        Args:
            node: The document node to render

        Returns:
            None
        """
        # Treat this entire operation as one "edit" so Qt doesn't attempt to
        # rendering as we're adding things.  It's *much* faster to do this as
        # one batch update.
        cursor = self._cursor
        cursor.beginEditBlock()

        # Clear the document.  At some point in the future we may want to do
        # incremental edits instead.
        self._document.clear()

        cursor.setBlockFormat(self._orig_block_format)

        # Process all children
        for child in node.children:
            self.visit(child)

        # If our last block is empty then delete it
        if self._cursor.block().text() == "":
            cursor.movePosition(QTextCursor.PreviousBlock)
            cursor.movePosition(QTextCursor.EndOfBlock)
            cursor.movePosition(QTextCursor.End, QTextCursor.KeepAnchor)
            cursor.removeSelectedText()

        # Enable all the changes to render
        cursor.endEditBlock()

    def follows_list_node(self, node):
        """
        Check if the given node follows a list node.

        Args:
            node: The node to check

        Returns:
            True if the node follows a list node, False otherwise
        """
        previous = node.previous_sibling()
        return previous is not None and isinstance(previous, (MarkdownOrderedListNode, MarkdownUnorderedListNode))

    def visit_MarkdownParagraphNode(self, node):  # pylint: disable=invalid-name
        """
        Render a paragraph node to the QTextDocument.

        Args:
            node: The paragraph node to render

        Returns:
            None
        """
        # If not at start of block, add a new block
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()

        # If we're in a list then ignore this.  We want our list formatting consistent.
        if not self._lists:
            font = QFont()
            font.setPointSizeF(self._style_manager.base_font_size)
            font_metrics = QFontMetricsF(font)
            block_format = QTextBlockFormat(orig_block_format)

            # Always set bottom margin
            block_format.setBottomMargin(font_metrics.height())

            # Set top margin if this node follows a list node
            if self.follows_list_node(node):
                block_format.setTopMargin(font_metrics.height())

            self._cursor.setBlockFormat(block_format)

        # Process all inline content
        for child in node.children:
            self.visit(child)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownHeadingNode(self, node):  # pylint: disable=invalid-name
        """
        Render a heading node to the QTextDocument.

        Args:
            node: The heading node to render

        Returns:
            None
        """
        # Add a new block with heading format
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()
        orig_char_format = self._cursor.charFormat()

        char_format = QTextCharFormat(orig_char_format)
        level = min(node.level, 6) - 1  # Convert to 0-based index
        font_size = (24 - (level * 2)) * self._style_manager.zoom_factor
        char_format.setFontPointSize(font_size)
        char_format.setFontWeight(QFont.Bold)
        self._cursor.setCharFormat(char_format)  # Apply heading character format

        # Apply block format (heading level)
        font = QFont()
        font.setPointSizeF(font_size)
        font_metrics = QFontMetricsF(font)
        block_format = QTextBlockFormat()
        block_format.setHeadingLevel(level)

        # Always set bottom margin
        block_format.setBottomMargin(font_metrics.height())

        # Set top margin if this node follows a list node
        if self.follows_list_node(node):
            block_format.setTopMargin(font_metrics.height())

        self._cursor.setBlockFormat(block_format)

        # Process all inline content for the heading
        for child in node.children:
            self.visit(child)

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownTextNode(self, node):  # pylint: disable=invalid-name
        """
        Render a text node to the QTextDocument.

        Args:
            node: The text node to render

        Returns:
            None
        """
        self._cursor.insertText(node.content)  # Use the current cursor format

    def visit_MarkdownBoldNode(self, node):  # pylint: disable=invalid-name
        """
        Render a bold node to the QTextDocument.

        Args:
            node: The bold node to render

        Returns:
            None
        """
        # Save current format, apply bold format, then process children
        orig_char_format = self._cursor.charFormat()

        # Create a new format based on the current one but with bold
        bold_format = QTextCharFormat(orig_char_format)
        bold_format.setFontWeight(QFont.Bold)
        self._cursor.setCharFormat(bold_format)

        for child in node.children:
            self.visit(child)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownEmphasisNode(self, node):  # pylint: disable=invalid-name
        """
        Render an emphasis node to the QTextDocument.

        Args:
            node: The emphasis node to render

        Returns:
            None
        """
        # Save current format, apply italic format, then process children
        orig_char_format = self._cursor.charFormat()

        # Create a new format based on the current one but with italic
        italic_format = QTextCharFormat(orig_char_format)
        italic_format.setFontItalic(True)
        self._cursor.setCharFormat(italic_format)

        for child in node.children:
            self.visit(child)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownInlineCodeNode(self, node):  # pylint: disable=invalid-name
        """
        Render an inline code node to the QTextDocument.

        Args:
            node: The inline code node to render

        Returns:
            None
        """
        # Save current format, apply code format, then insert text
        orig_char_format = self._cursor.charFormat()

        # Create a new format based on the current one but with a monospace font
        code_format = QTextCharFormat(orig_char_format)
        code_format.setFontFamilies(self._style_manager.monospace_font_families)
        code_format.setForeground(self._style_manager.get_color(ColorRole.SYNTAX_INLINE_CODE))
        self._cursor.setCharFormat(code_format)

        self._cursor.insertText(node.content)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownCodeBlockNode(self, node):  # pylint: disable=invalid-name
        """
        Render a code block node to the QTextDocument.

        Args:
            node: The code block node to render

        Returns:
            None
        """
        # Insert a new block if needed
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()
        orig_char_format = self._cursor.charFormat()

        # Create a code block format with monospace font and background
        block_format = QTextBlockFormat()
        block_format.setIndent(1)  # Indent the block

        # Set top margin if this node follows a list node
        if self.follows_list_node(node):
            font = QFont()
            font.setPointSizeF(self._style_manager.base_font_size)
            font_metrics = QFontMetricsF(font)
            block_format.setTopMargin(font_metrics.height())

        # Apply the block format
        self._cursor.setBlockFormat(block_format)

        # Apply code formatting and insert text
        code_format = QTextCharFormat(orig_char_format)
        code_format.setFontFamilies(self._style_manager.monospace_font_families)
        self._cursor.setCharFormat(code_format)

        # Split content by lines and add each in its own block
        lines = node.content.split('\n')

        for i, line in enumerate(lines):
            if i > 0:
                self._cursor.insertBlock(block_format)

            self._cursor.insertText(line)

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownOrderedListNode(self, node):  # pylint: disable=invalid-name
        """
        Render an ordered list node to the QTextDocument.

        Args:
            node: The ordered list node to render
        """
        # Insert a new block if needed
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()
        orig_char_format = self._cursor.charFormat()

        # Create ordered list format with correct start number
        self._list_level += 1

        block_format = QTextBlockFormat(orig_block_format)
        block_format.setBottomMargin(0)
        self._cursor.setBlockFormat(block_format)

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.ListDecimal)
        list_format.setStart(node.start)
        list_format.setIndent(self._list_level)

        new_list = self._cursor.createList(list_format)
        self._lists.append(new_list)

        # Process the child nodes (list items)
        for child in node.children:
            self.visit(child)

        self._lists.pop()

        # Exit this list level
        self._list_level -= 1

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownUnorderedListNode(self, node):  # pylint: disable=invalid-name
        """
        Render an unordered list node to the QTextDocument.

        Args:
            node: The unordered list node to render
        """
        # Insert a new block if needed
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()
        orig_char_format = self._cursor.charFormat()

        # Create unordered list format
        self._list_level += 1

        block_format = QTextBlockFormat(orig_block_format)
        block_format.setBottomMargin(0)
        self._cursor.setBlockFormat(block_format)

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.ListDisc)
        list_format.setIndent(self._list_level)

        new_list = self._cursor.createList(list_format)
        self._lists.append(new_list)

        # Process the child nodes (list items)
        for child in node.children:
            self.visit(child)

        self._lists.pop()

        # Exit this list level
        self._list_level -= 1

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownListItemNode(self, node):  # pylint: disable=invalid-name
        """
        Render a list item node to the QTextDocument.

        Args:
            node: The list item node to render

        Returns:
            None
        """
        # Start a new list item by creating a block if not at start of a block
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._lists[-1].add(self._cursor.block())

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
        self._cursor.insertText("\u2028")
