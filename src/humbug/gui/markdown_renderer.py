"""
Markdown AST visitor to render the AST directly to a QTextDocument.
"""

import logging
import os
import re
from typing import List, Tuple, cast

from PySide6.QtCore import Qt
from PySide6.QtGui import (
    QTextCursor, QTextDocument, QTextCharFormat, QTextBlockFormat,
    QTextListFormat, QFont, QFontMetricsF, QTextList, QTextTable,
    QTextTableFormat, QTextFrameFormat, QTextLength, QImage, QTextImageFormat
)

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager
from humbug.gui.markdown_block_data import HeadingBlockData
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.markdown.markdown_ast_node import (
    MarkdownASTVisitor, MarkdownDocumentNode, MarkdownParagraphNode, MarkdownHeadingNode,
    MarkdownTextNode, MarkdownBoldNode, MarkdownEmphasisNode, MarkdownInlineCodeNode,
    MarkdownCodeBlockNode, MarkdownListItemNode, MarkdownOrderedListNode,
    MarkdownUnorderedListNode, MarkdownLineBreakNode, MarkdownTableNode, MarkdownTableHeaderNode,
    MarkdownTableBodyNode, MarkdownTableRowNode, MarkdownTableCellNode, MarkdownHorizontalRuleNode,
    MarkdownLinkNode, MarkdownImageNode
)


class MarkdownRenderer(MarkdownASTVisitor):
    """Visitor that renders the AST directly to a QTextDocument."""

    def __init__(self, document: QTextDocument) -> None:
        """
        Initialize the document renderer.

        Args:
            document: The QTextDocument to render into
        """
        super().__init__()
        self._logger = logging.getLogger("MarkdownRenderer")

        self._document = document
        self._source_path: str | None = None
        self._cursor = QTextCursor(document)
        self._cursor.movePosition(QTextCursor.MoveOperation.Start)
        self._orig_block_format = self._cursor.blockFormat()

        self._lists: List[QTextList] = []
        self._list_level = 0

        self._default_font_height: float = 0

        self._mindspace_manager = MindspaceManager()

        self._style_manager = StyleManager()
        self._style_manager.style_changed.connect(self._handle_style_changed)
        self._handle_style_changed()

        # Table state variables
        self._current_table: QTextTable | None = None
        self._current_row: int = 0

    def _handle_style_changed(self) -> None:
        """Handle style changes."""
        # Add a pixel image resource to the document for horizontal rules.  Make the image
        # very wide so it will always fit the width of the viewport.
        pixel_image = QImage(8192, 1, QImage.Format.Format_ARGB32)
        pixel_image.fill(self._style_manager.get_color(ColorRole.TABLE_BORDER))
        self._document.addResource(QTextDocument.ResourceType.ImageResource, "pixel", pixel_image)

    def visit_MarkdownDocumentNode(self, node: MarkdownDocumentNode) -> None:  # pylint: disable=invalid-name
        """
        Render a document node to the QTextDocument.

        Args:
            node: The document node to render

        Returns:
            None
        """
        self._source_path = node.source_path

        # Treat this entire operation as one "edit" so Qt doesn't attempt to
        # rendering as we're adding things.  It's *much* faster to do this as
        # one batch update.
        cursor = self._cursor
        cursor.beginEditBlock()

        # Clear the document.  At some point in the future we may want to do
        # incremental edits instead.
        cursor.select(QTextCursor.SelectionType.Document)
        cursor.removeSelectedText()

        # Set up the default font size
        font = QFont()
        font.setPointSizeF(self._style_manager.base_font_size())
        font_metrics = QFontMetricsF(font)
        self._default_font_height = font_metrics.height()

        cursor.setBlockFormat(self._orig_block_format)

        # Process all children
        for child in node.children:
            self.visit(child)

        # If our last block is empty then delete it
        if self._cursor.block().text() == "":
            cursor.movePosition(QTextCursor.MoveOperation.PreviousBlock)
            cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock)
            cursor.movePosition(QTextCursor.MoveOperation.End, QTextCursor.MoveMode.KeepAnchor)
            cursor.removeSelectedText()

        # Enable all the changes to render
        cursor.endEditBlock()

    def visit_MarkdownParagraphNode(self, node: MarkdownParagraphNode) -> None:  # pylint: disable=invalid-name
        """
        Render a paragraph node to the QTextDocument.

        Args:
            node: The paragraph node to render

        Returns:
            None
        """
        orig_block_format = self._cursor.blockFormat()

        # We're in a normal paragraph, so format it normally
        block_format = QTextBlockFormat(orig_block_format)

        # Work out if we are in a tight list or not.  If we are, we're going to adjust margins accordingly.
        tight = False
        if node.parent:
            parent_list_node = node.parent.parent
            if isinstance(parent_list_node, (MarkdownOrderedListNode, MarkdownUnorderedListNode)):
                if parent_list_node.tight:
                    tight = True

        # If the previous sibling is a list, we need to add a top margin
        previous_sibling = node.previous_sibling()
        if previous_sibling and isinstance(previous_sibling, (MarkdownOrderedListNode, MarkdownUnorderedListNode)):
            block_format.setTopMargin(self._default_font_height)

        # If there is no previous sibling, check if our parent is a list item.  If it is, we also need to add a top margin.
        elif not previous_sibling and node.parent and isinstance(node.parent, MarkdownListItemNode):
            if not tight:
                block_format.setTopMargin(self._default_font_height)

        # If the next sibling is a horizontal rule, we don't need a bottom margin
        next_sibling = node.next_sibling()
        if next_sibling and isinstance(next_sibling, MarkdownHorizontalRuleNode):
            block_format.setBottomMargin(0)

        # If we are in a tight list, we don't need a bottom margin either
        elif tight:
            block_format.setBottomMargin(0)

        else:
            block_format.setBottomMargin(self._default_font_height)

        self._cursor.setBlockFormat(block_format)

        # Are we in a list?  If yes, we need to potentially do some tricks to ensure
        # paragraphs are rendered correctly.
        if self._lists:
            previous_sibling = node.previous_sibling()
            if previous_sibling:
                list_fmt = QTextListFormat(self._lists[-1].format())
                list_fmt.setStyle(QTextListFormat.Style.ListStyleUndefined)
                new_list = self._cursor.createList(list_fmt)
                new_list.setFormat(list_fmt)
                new_list.add(self._cursor.block())

        # Process all inline content
        for child in node.children:
            self.visit(child)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownHeadingNode(self, node: MarkdownHeadingNode) -> None:  # pylint: disable=invalid-name
        """
        Render a heading node to the QTextDocument.

        Args:
            node: The heading node to render

        Returns:
            None
        """
        orig_block_format = self._cursor.blockFormat()
        orig_char_format = self._cursor.charFormat()

        char_format = QTextCharFormat(orig_char_format)

        multipliers = [2.0, 1.782, 1.587, 1.414, 1.26, 1.122]
        level = min(node.level, 6) - 1  # Convert to 0-based index

        base_font_size = self._style_manager.base_font_size()
        font_size = base_font_size * multipliers[level] * self._style_manager.zoom_factor()
        char_format.setFontPointSize(font_size)
        char_format.setFontWeight(QFont.Weight.Bold)
        char_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_HEADING))
        self._cursor.setCharFormat(char_format)

        # Apply block format (heading level)
        block_format = QTextBlockFormat()
        block_format.setHeadingLevel(level)

        # We don't need a top margin if this is the first block in the document or
        # if the previous sibling is a horizontal rule
        previous_sibling = node.previous_sibling()
        if previous_sibling and not isinstance(previous_sibling, MarkdownHorizontalRuleNode):
            block_format.setTopMargin(self._default_font_height * multipliers[level])

        block_format.setBottomMargin(self._default_font_height)
        self._cursor.setBlockFormat(block_format)

        # Store the ID as block user data
        self._cursor.block().setUserData(HeadingBlockData(node.anchor_id))

        # Process all inline content for the heading
        for child in node.children:
            self.visit(child)

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownTextNode(self, node: MarkdownTextNode) -> None:  # pylint: disable=invalid-name
        """
        Render a text node to the QTextDocument with space normalization.

        Args:
            node: The text node to render
        """
        text = node.content

        # First: normalize multiple spaces within this text node to single spaces
        text = re.sub(r' {2,}', ' ', text)

        # Second: handle duplicate spaces between adjacent text nodes
        if node.parent and node.parent.children:
            current_index = node.parent.children.index(node)
            if current_index > 0:
                prev_sibling = node.parent.children[current_index - 1]

                # If previous sibling is a text node ending with space
                # and current text starts with space, remove the leading space
                if (isinstance(prev_sibling, MarkdownTextNode) and
                    prev_sibling.content.rstrip() != prev_sibling.content and  # ends with whitespace
                    text.startswith(' ')):
                    text = text.lstrip(' ')

        # If the text is empty after normalization, we don't need to insert anything
        if not text:
            return

        self._cursor.insertText(text)

    def visit_MarkdownBoldNode(self, node: MarkdownBoldNode) -> None:  # pylint: disable=invalid-name
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
        bold_format.setFontWeight(QFont.Weight.Bold)

        # Determine if this bold node is within a heading
        is_in_heading = False
        parent = node.parent
        while parent is not None:
            if isinstance(parent, MarkdownHeadingNode):
                is_in_heading = True
                break

            parent = parent.parent

        # Set foreground color based on context
        if is_in_heading:
            bold_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_HEADING_BRIGHT))
        else:
            bold_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_BRIGHT))

        self._cursor.setCharFormat(bold_format)

        for child in node.children:
            self.visit(child)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownEmphasisNode(self, node: MarkdownEmphasisNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownInlineCodeNode(self, node: MarkdownInlineCodeNode) -> None:  # pylint: disable=invalid-name
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
        code_format.setFontFamilies(self._style_manager.monospace_font_families())

        # If we are inside a link, then keep the link color, otherwise set the code color
        if not node.parent or not isinstance(node.parent, MarkdownLinkNode):
            code_format.setForeground(self._style_manager.get_color(ColorRole.SYNTAX_INLINE_CODE))

        self._cursor.setCharFormat(code_format)

        self._cursor.insertText(node.content)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownLinkNode(self, node: MarkdownLinkNode) -> None:  # pylint: disable=invalid-name
        """
        Render a link node to the QTextDocument.

        Args:
            node: The link node to render

        Returns:
            None
        """
        # Save current format
        orig_char_format = self._cursor.charFormat()

        # Create a new format for links
        link_format = QTextCharFormat(orig_char_format)
        link_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_LINK))
        link_format.setFontUnderline(True)
        link_format.setAnchor(True)

        # Process the URL to handle internal links
        url = node.url

        # Check if this is an internal link to a heading
        if self._is_internal_heading_link(url):
            # Convert from heading text to element ID format
            element_id = url.lstrip('#')
            url = f"#{element_id}"

        link_format.setAnchorHref(url)

        # Add the link title as a tooltip if present
        if node.title:
            link_format.setToolTip(node.title)

        else:
            tip_url = url
            if self._is_local_file(url):
                # If it's a local file, show the path as tooltip
                if url.startswith('file://'):
                    tip_url = url[7:]

                tip_url = self._mindspace_manager.get_relative_path(tip_url)

            link_format.setToolTip(tip_url)

        self._cursor.setCharFormat(link_format)

        # Process all content inside the link
        for child in node.children:
            self.visit(child)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def _is_internal_heading_link(self, url: str) -> bool:
        """
        Determine if a URL is an internal link to a heading.

        Args:
            url: The URL to check

        Returns:
            True if the URL is an internal heading link
        """
        # Direct anchor link
        if url.startswith('#'):
            return True

        # No protocol or path, might be an internal reference
        if '://' not in url and os.path.sep not in url and '.' not in url:
            return True

        return False

    def _is_local_file(self, url: str) -> bool:
        """
        Determine if a URL refers to a local file.

        Args:
            url: The URL to check

        Returns:
            True if the URL represents a local file, False otherwise
        """
        # Handle relative and absolute paths
        if url.startswith('/') or url.startswith('./') or url.startswith('../') or ':' not in url:
            return True

        # Handle file:// URLs
        if url.startswith('file://'):
            return True

        # Handle Windows-style paths (C:/, D:/, etc.)
        if len(url) > 1 and url[1] == ':' and url[0].isalpha() and url[2] == '/':
            return True

        return False

    def _load_local_image(self, path: str) -> Tuple[QImage, bool]:
        """
        Load an image from a local file path.

        Args:
            path: The path to the image file

        Returns:
            A tuple of (QImage, success_flag)
        """
        logger = logging.getLogger("MarkdownRenderer")

        # Convert file:// URLs to local paths
        if path.startswith('file://'):
            path = path[7:]

        # Normalize path (handle relative paths, etc.)
        try:
            # If the path is relative, resolve it against the source path
            if not os.path.isabs(path) and self._source_path:
                source_dir = os.path.dirname(self._source_path)
                path = os.path.normpath(os.path.join(source_dir, path))

            elif not os.path.isabs(path):
                # Fallback to current working directory if source path is unknown
                path = os.path.normpath(os.path.abspath(path))

            # Check if file exists
            if not os.path.isfile(path):
                logger.warning("Local image not found: %s", path)
                return self._create_placeholder_image(), False

            # Load the image
            image = QImage(path)
            if image.isNull():
                logger.warning("Failed to load image: %s", path)
                return self._create_placeholder_image(), False

            # Verify the image is valid
            if image.width() <= 0 or image.height() <= 0:
                logger.warning("Invalid image dimensions: %s", path)
                return self._create_placeholder_image(), False

            # Resize the image if it's too large for displaying in the document
#            max_width = 800  # Maximum width for displayed images
#            if image.width() > max_width:
#                image = image.scaledToWidth(max_width, Qt.TransformationMode.SmoothTransformation)
# TODO: HANDLE IMAGE SCALING

            return image, True

        except Exception:
            logger.exception("Error loading local image: %s", path)
            return self._create_placeholder_image(), False

    def _create_placeholder_image(self) -> QImage:
        """
        Create a placeholder image for when an image cannot be loaded.

        Returns:
            A QImage representing a placeholder
        """
        image = QImage(100, 100, QImage.Format.Format_ARGB32)
        image.fill(self._style_manager.get_color(ColorRole.BACKGROUND_SECONDARY))
        return image

    def visit_MarkdownImageNode(self, node: MarkdownImageNode) -> None:  # pylint: disable=invalid-name
        """
        Render an image node to the QTextDocument.

        Args:
            node: The image node to render

        Returns:
            None
        """
        # We may find we're in the middle of a paragraph block, in which case we need to ensure we start the
        # image at a new block.
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        image = self._create_placeholder_image()
        loaded_successfully = False

        # Attempt to load local images
        if self._is_local_file(node.url):
            image, loaded_successfully = self._load_local_image(node.url)

        # Create a resource for this image
        resource_name = f"image_{hash(node.url)}"
        self._document.addResource(QTextDocument.ResourceType.ImageResource, resource_name, image)

        # Create an image format
        img_format = QTextImageFormat()
        img_format.setName(resource_name)

        # If the image loaded successfully, preserve its aspect ratio
        if loaded_successfully:
            # Set the maximum width to 100% of the document width.  This lets the image
            # scale down to fit the document width but not scale up.
            img_format.setMaximumWidth(QTextLength(QTextLength.Type.PercentageLength, 100))

        # Add alt text and title if available
        tooltip = ""
        if node.alt_text:
            tooltip = node.alt_text

        if node.title:
            if tooltip:
                tooltip += f" - {node.title}"

            else:
                tooltip = node.title

        if tooltip:
            img_format.setToolTip(tooltip)

        # Insert the image
        self._cursor.insertImage(img_format)

    def visit_MarkdownCodeBlockNode(self, node: MarkdownCodeBlockNode) -> None:  # pylint: disable=invalid-name
        """
        Render a code block node to the QTextDocument.

        Args:
            node: The code block node to render

        Returns:
            None
        """
        orig_block_format = self._cursor.blockFormat()
        orig_char_format = self._cursor.charFormat()

        # Create a code block format with monospace font and background
        block_format = QTextBlockFormat()
        block_format.setIndent(1)  # Indent the block

        # Apply the block format
        self._cursor.setBlockFormat(block_format)

        # Apply code formatting and insert text
        code_format = QTextCharFormat(orig_char_format)
        code_format.setFontFamilies(self._style_manager.monospace_font_families())
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

    def _list_node_visitor(self, node: MarkdownOrderedListNode | MarkdownUnorderedListNode) -> None:
        """
        Handle the common list node visitor operations.

        Args:
            node: The ordered list node to render
        """
        orig_char_format = self._cursor.charFormat()

        # Process the child nodes (list items)
        for child in node.children:
            self.visit(child)

        self._lists.pop()

        # If we find ourselves in a new block then we need to look at its predecessor
        # as that's the one that actually has our last list item
        at_block_start = self._cursor.atBlockStart()
        if at_block_start:
            self._cursor.movePosition(QTextCursor.MoveOperation.PreviousBlock)

        block_format = self._cursor.blockFormat()
        block_format.setBottomMargin(0)
        self._cursor.setBlockFormat(block_format)

        if at_block_start:
            self._cursor.movePosition(QTextCursor.MoveOperation.NextBlock)

        # Exit this list level
        self._list_level -= 1

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

    def visit_MarkdownOrderedListNode(self, node: MarkdownOrderedListNode) -> None:  # pylint: disable=invalid-name
        """
        Render an ordered list node to the QTextDocument.

        Args:
            node: The ordered list node to render
        """
        # Insert a new block if needed
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()

        # Create ordered list format with correct start number
        self._list_level += 1

        block_format = QTextBlockFormat(orig_block_format)
        block_format.setBottomMargin(0)
        self._cursor.setBlockFormat(block_format)

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.Style.ListDecimal)
        list_format.setStart(node.start)
        list_format.setIndent(self._list_level)

        new_list = self._cursor.createList(list_format)
        self._lists.append(new_list)
        self._list_node_visitor(node)
        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownUnorderedListNode(self, node: MarkdownUnorderedListNode) -> None:  # pylint: disable=invalid-name
        """
        Render an unordered list node to the QTextDocument.

        Args:
            node: The unordered list node to render
        """
        # Insert a new block if needed
        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

        orig_block_format = self._cursor.blockFormat()

        # Create unordered list format
        self._list_level += 1

        block_format = QTextBlockFormat(orig_block_format)
        block_format.setBottomMargin(0)
        self._cursor.setBlockFormat(block_format)

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.Style.ListDisc)
        list_format.setIndent(self._list_level)

        new_list = self._cursor.createList(list_format)
        self._lists.append(new_list)
        self._list_node_visitor(node)
        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownListItemNode(self, node: MarkdownListItemNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownLineBreakNode(self, node: MarkdownLineBreakNode) -> None:  # pylint: disable=invalid-name
        """
        Render a line break node to the QTextDocument.

        Args:
            node: The line break node to render

        Returns:
            None
        """
        # If we don't have a sibling then we don't need to do anything
        if not node.next_sibling():
            return

        # Insert line break character
        self._cursor.insertText("\u2028")

    def visit_MarkdownTableNode(self, node: MarkdownTableNode) -> None:  # pylint: disable=invalid-name
        """
        Render a table node to the QTextDocument.

        Args:
            node: The table node to render

        Returns:
            None
        """
        # Insert a new block if needed
        orig_block_format = self._cursor.blockFormat()
        top_frame = self._cursor.currentFrame()

        # Verify that this table has at least one header row and one body row before rendering
        has_valid_structure = False

        for child in node.children:
            if isinstance(child, MarkdownTableHeaderNode) and child.children:
                for header_child in node.children:
                    if isinstance(header_child, MarkdownTableBodyNode) and header_child.children:
                        has_valid_structure = True
                        break

        if not has_valid_structure:
            # Fallback: render as text if the table structure is invalid
            self._render_table_as_text(node)
            return

        # Process all children (header and body sections)
        for child in node.children:
            self.visit(child)

        self._cursor.setPosition(top_frame.lastPosition())

        # Set spacing after table
        block_format = QTextBlockFormat(orig_block_format)
        block_format.setBottomMargin(self._default_font_height)
        self._cursor.setBlockFormat(block_format)

        # Add a new block after the table with proper spacing
        # Note: Qt needs a block after a table otherwise it segfaults!
        self._cursor.insertBlock()

    def _render_table_as_text(self, node: MarkdownTableNode) -> None:
        """
        Render a table as plain text when it has an invalid structure.

        Args:
            node: The table node to render

        Returns:
            None
        """
        # Create a paragraph for each row in the table
        for child in node.children:
            if isinstance(child, MarkdownTableHeaderNode):
                for row in child.children:
                    paragraph = MarkdownParagraphNode()

                    # Create a text representation of the row
                    row_text = "|"
                    for cell in row.children:
                        cell_text = ""
                        for content in cell.children:
                            if isinstance(content, MarkdownTextNode):
                                cell_text += content.content

                        row_text += f" {cell_text} |"

                    paragraph.add_child(MarkdownTextNode(row_text))
                    self.visit(paragraph)

            if isinstance(child, MarkdownTableBodyNode):
                for row in child.children:
                    paragraph = MarkdownParagraphNode()

                    # Create a text representation of the row
                    row_text = "|"
                    for cell in row.children:
                        cell_text = ""
                        for content in cell.children:
                            if isinstance(content, MarkdownTextNode):
                                cell_text += content.content

                        row_text += f" {cell_text} |"

                    paragraph.add_child(MarkdownTextNode(row_text))
                    self.visit(paragraph)

    def visit_MarkdownTableHeaderNode(self, node: MarkdownTableHeaderNode) -> None:  # pylint: disable=invalid-name
        """
        Render a table header node to the QTextDocument.

        Args:
            node: The table header node to render

        Returns:
            None
        """
        # The header node contains rows, which we'll process
        # in the row visitor. We just pass through to the children.
        for child in node.children:
            self.visit(child)

    def visit_MarkdownTableBodyNode(self, node: MarkdownTableBodyNode) -> None:  # pylint: disable=invalid-name
        """
        Render a table body node to the QTextDocument.

        Args:
            node: The table body node to render

        Returns:
            None
        """
        # The body node contains rows, which we'll process
        # in the row visitor. We just pass through to the children.
        for child in node.children:
            self.visit(child)

    def visit_MarkdownTableRowNode(self, node: MarkdownTableRowNode) -> None:  # pylint: disable=invalid-name
        """
        Render a table row node to the QTextDocument.

        Args:
            node: The table row node to render

        Returns:
            None
        """
        # Is this the first row of a table?
        is_first_row = isinstance(node.parent, MarkdownTableHeaderNode) and node is node.parent.children[0]

        # If this is the first row, we need to create the table
        if is_first_row:
            # Count the cells to determine column count
            column_count = len(node.children)
            if column_count == 0:
                # Can't create a table with no columns, fallback to text
                return

            # Count all rows in header and body to determine row count
            header = cast(MarkdownTableHeaderNode, node.parent)
            body = None

            # Find the table body (sibling of header)
            table_node = cast(MarkdownTableNode, header.parent)
            for sibling in table_node.children:
                if isinstance(sibling, MarkdownTableBodyNode):
                    body = sibling
                    break

            # Calculate total row count
            row_count = len(header.children)
            if body:
                row_count += len(body.children)

            # Ensure we have at least one body row
            if body is None or len(body.children) == 0:
                # Can't create a table with no body, fallback to text
                return

            # Create table format
            table_format = QTextTableFormat()
            table_format.setCellPadding(2)
            table_format.setCellSpacing(0)
            table_format.setBorderStyle(QTextFrameFormat.BorderStyle.BorderStyle_Solid)
            table_format.setBorder(1)

            # Get the width of the document
            doc_width = self._document.textWidth()
            if doc_width > 0:
                table_format.setWidth(QTextLength(QTextLength.Type.PercentageLength, 100))

            # Set uniform column widths
            col_width_percent = 100 / column_count
            col_widths = [QTextLength(QTextLength.Type.PercentageLength, col_width_percent) for _ in range(column_count)]
            table_format.setColumnWidthConstraints(col_widths)

            # Create the table
            self._current_table = self._cursor.insertTable(row_count, column_count, table_format)
            self._current_row = 0

        # Process all cells in this row
        for i, cell_node in enumerate(node.children):
            if self._current_table and i < self._current_table.columns():
                # Get the current table cell
                table_cell = self._current_table.cellAt(self._current_row, i)
                cell_cursor = table_cell.firstCursorPosition()

                # Apply cell format based on alignment
                if isinstance(cell_node, MarkdownTableCellNode):
                    # Create the cell format object
                    cell_format = table_cell.format().toTableCellFormat()
                    cell_format.setBorderStyle(QTextFrameFormat.BorderStyle.BorderStyle_Solid)
                    cell_format.setBorder(1)
                    cell_format.setBorderBrush(self._style_manager.get_color(ColorRole.TABLE_BORDER))
                    cell_format.setPadding(8)

                    # Apply header styling if needed
                    if cell_node.is_header:
                        cell_format.setBackground(self._style_manager.get_color(ColorRole.TABLE_HEADER_BACKGROUND))

                    # Apply the cell format
                    table_cell.setFormat(cell_format)

                    # Create character format for headers (bold text)
                    if cell_node.is_header:
                        cell_char_format = QTextCharFormat()
                        cell_char_format.setFontWeight(QFont.Weight.Bold)
                        cell_char_format.setForeground(self._style_manager.get_color(ColorRole.TEXT_BRIGHT))
                        cell_cursor.setCharFormat(cell_char_format)

                    # Set text alignment on the block format inside the cell
                    block_format = cell_cursor.blockFormat()

                    if cell_node.alignment == 'center':
                        block_format.setAlignment(Qt.AlignmentFlag.AlignCenter)

                    elif cell_node.alignment == 'right':
                        block_format.setAlignment(Qt.AlignmentFlag.AlignRight)

                    else:
                        block_format.setAlignment(Qt.AlignmentFlag.AlignLeft)

                    cell_cursor.setBlockFormat(block_format)

                # Store the current cursor position
                old_cursor = self._cursor
                self._cursor = cell_cursor

                # Visit the cell node to render its content
                self.visit(cell_node)

                # Restore cursor
                self._cursor = old_cursor

        # Increment row counter
        self._current_row += 1

        # Check if this is the last row in the table
        if ((isinstance(node.parent, MarkdownTableHeaderNode) and
                node is node.parent.children[-1] and
                not any(
                    isinstance(sibling, MarkdownTableBodyNode) for sibling in cast(MarkdownTableNode, node.parent.parent).children
                )
            ) or
            (isinstance(node.parent, MarkdownTableBodyNode) and node is node.parent.children[-1])
        ):
            # Reset table state
            self._current_table = None
            self._current_row = 0

    def visit_MarkdownTableCellNode(self, node: MarkdownTableCellNode) -> None:  # pylint: disable=invalid-name
        """
        Render a table cell node to the QTextDocument.

        Args:
            node: The table cell node to render

        Returns:
            None
        """
        # Process all inline content (text, bold, etc.)
        for child in node.children:
            self.visit(child)

    def visit_MarkdownHorizontalRuleNode(self, node: MarkdownHorizontalRuleNode) -> None:  # pylint: disable=invalid-name
        """
        Render a horizontal rule node to the QTextDocument.

        Args:
            node: The horizontal rule node to render

        Returns:
            None
        """
        # We need to add a new block before the horizontal rule to ensure proper spacing.
        # However, if the previous sibling is a table then we don't need to do this because
        # one already exists.
        previous_sibling = node.previous_sibling()
        if not previous_sibling or not isinstance(previous_sibling, MarkdownTableNode):
            self._cursor.insertBlock()

        # This is a workaround for the fact that QTextDocument doesn't support
        # horizontal rules directly.  We use an image resource instead.
        img_format = QTextImageFormat()
        img_format.setName("pixel")
        img_format.setMaximumWidth(QTextLength(QTextLength.Type.PercentageLength, 100))
        img_format.setHeight(1)
        self._cursor.insertImage(img_format)

        self._cursor.insertBlock()

        # In most cases we need to add 2 new blocks after the horizontal rule
        # to ensure proper spacing.  However, if the previous sibling is a
        # MarkdownTableNode then we only need to add one.
        if not previous_sibling or not isinstance(previous_sibling, MarkdownTableNode):
            self._cursor.insertBlock()
