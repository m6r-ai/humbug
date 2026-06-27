"""
Markdown AST visitor to render the AST directly to a QTextDocument.
"""

import logging
import os
import re
from typing import Callable, List, Tuple, cast

from PySide6.QtCore import Qt
from PySide6.QtGui import (
    QTextCursor, QTextDocument, QTextCharFormat, QTextBlockFormat,
    QTextListFormat, QFont, QFontMetricsF, QTextList, QTextTable,
    QTextTableFormat, QTextFrameFormat, QTextLength, QImage, QTextImageFormat,
    QTextDocumentFragment, QImageReader
)

from syntax import TokenType

from dmarkdown import (
    MarkdownASTNode, MarkdownASTVisitor, MarkdownASTDocumentNode, MarkdownASTParagraphNode, MarkdownASTHeadingNode,
    MarkdownASTTextNode, MarkdownASTBoldNode, MarkdownASTEmphasisNode, MarkdownASTInlineCodeNode,
    MarkdownASTCodeBlockNode, MarkdownASTListItemNode, MarkdownASTOrderedListNode,
    MarkdownASTUnorderedListNode, MarkdownASTLineBreakNode, MarkdownASTStrikethroughNode,
    MarkdownASTTableNode, MarkdownASTTableHeaderNode,
    MarkdownASTTableBodyNode, MarkdownASTTableRowNode, MarkdownASTTableCellNode, MarkdownASTHorizontalRuleNode,
    MarkdownASTLinkNode, MarkdownASTImageNode, MarkdownASTBlockquoteNode
)

from desktop.color_role import ColorRole
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.style_manager import StyleManager
from desktop.markdown.markdown_block_data import HeadingBlockData, MarkdownBlockData


class MarkdownRenderer(MarkdownASTVisitor):
    """Visitor that renders the AST directly to a QTextDocument."""

    def __init__(
        self,
        document: QTextDocument,
        animated_gif_callback: Callable[[str, str, int, int], None] | None = None
    ) -> None:
        """
        Initialize the document renderer.

        Args:
            document: The QTextDocument to render into
            animated_gif_callback: Optional callback invoked with
                (resource_name, path, display_width, display_height) for each
                animated GIF image encountered during rendering
        """
        super().__init__()
        self._logger = logging.getLogger("MarkdownRenderer")

        self._document = document
        self._animated_gif_callback = animated_gif_callback
        self._source_path: str | None = None
        self._cursor = QTextCursor(document)
        self._cursor.movePosition(QTextCursor.MoveOperation.Start)
        self._orig_block_format = self._cursor.blockFormat()

        self._lists: List[QTextList] = []
        self._list_level = 0

        self._default_font_height: float = 0

        self._mindspace_manager = MindspaceManager()

        self._style_manager = StyleManager()

        # Fingerprints of the top-level nodes in the last stable snapshot.
        self._stable_fingerprints: List[int] = []

        # Snapshot of the rendered document at the last stable point.  When all
        # current top-level nodes match the stable fingerprints, this is updated.
        # On the next call, if the last node has changed, we restore from this
        # snapshot and re-render only the changed suffix cleanly.
        self._snapshot: QTextDocument | None = None
        self.apply_style()

        # Table state variables
        self._current_table: QTextTable | None = None
        self._current_row: int = 0

        # Per-level list-indent at each blockquote nesting depth (empty = not in a blockquote)
        self._blockquote_bar_offsets: list[int] = []

    def apply_style(self) -> None:
        """Apply style changes."""
        self._stable_fingerprints = []
        self._snapshot = None
        # Add a pixel image resource to the document for horizontal rules.  Make the image
        # very wide so it will always fit the width of the viewport.
        pixel_image = QImage(8192, 1, QImage.Format.Format_ARGB32)
        pixel_image.fill(self._style_manager.get_color(ColorRole.TABLE_BORDER))
        self._document.addResource(QTextDocument.ResourceType.ImageResource, "pixel", pixel_image)

    @staticmethod
    def _node_fingerprint(node: MarkdownASTNode) -> int:
        """
        Compute a cheap structural fingerprint for a top-level AST node.

        Two nodes with identical fingerprints produce identical rendered output.
        line_start/line_end alone are insufficient for single-line nodes that grow
        by token appending, so child count and last child content are also included.

        Args:
            node: A top-level child of the document node.

        Returns:
            An integer hash suitable for equality comparison.
        """
        language = ""
        if isinstance(node, MarkdownASTCodeBlockNode):
            language = node.language_name

        child_count = len(node.children)
        last_content = ""
        if node.children:
            last = node.children[-1]
            if isinstance(last, MarkdownASTTextNode):
                last_content = last.content

        return hash((type(node).__name__, node.line_start, node.line_end, language,
                     child_count, last_content))

    def _first_changed_index(self, new_children: List[MarkdownASTNode]) -> int:
        """
        Return the index of the first top-level child that differs from the
        stable snapshot fingerprints.

        Args:
            new_children: The new document node's children list.

        Returns:
            The index of the first changed child, or len(new_children) if all
            children match the stable snapshot.
        """
        for i, child in enumerate(new_children):
            if i >= len(self._stable_fingerprints):
                return i

            if self._node_fingerprint(child) != self._stable_fingerprints[i]:
                return i

        return len(new_children)

    def visit_MarkdownASTDocumentNode(self, node: MarkdownASTDocumentNode) -> None:  # pylint: disable=invalid-name
        """
        Render a document node to the QTextDocument.

        Args:
            node: The document node to render

        Returns:
            None
        """
        self._source_path = node.source_path

        # Set up the default font size
        style_manager = self._style_manager
        font = QFont()
        font.setPointSizeF(style_manager.base_font_size() * style_manager.zoom_factor())
        font_metrics = QFontMetricsF(font)
        self._default_font_height = font_metrics.height()

        first_changed = self._first_changed_index(node.children)

        # Treat this entire operation as one "edit" so Qt doesn't attempt to
        # render as we're adding things.  It's *much* faster to do this as
        # one batch update.
        cursor = self._cursor
        cursor.beginEditBlock()

        if first_changed == 0 or self._snapshot is None:
            # Nothing is reusable — full clear and re-render.
            cursor.select(QTextCursor.SelectionType.Document)
            cursor.removeSelectedText()
            cursor.setBlockFormat(self._orig_block_format)

        else:
            # Restore the snapshot, then append the changed suffix.  The snapshot
            # contains the cleanly rendered stable prefix, so the cursor state and
            # document content are exactly as they were after the stable render.
            cursor.select(QTextCursor.SelectionType.Document)
            cursor.removeSelectedText()
            cursor.setBlockFormat(self._orig_block_format)
            cursor.insertFragment(QTextDocumentFragment(self._snapshot))
            cursor.movePosition(QTextCursor.MoveOperation.End)

        children = node.children
        stable_end = len(children) - 1 if children else 0

        for i, child in enumerate(children[first_changed:], start=first_changed):
            # Before rendering the last child, snapshot the document if all
            # preceding nodes are stable.  This gives us a clean restore point
            # that contains exactly nodes 0..N-2, with no partial last-node content.
            if i == stable_end and first_changed >= stable_end:
                self._stable_fingerprints = [self._node_fingerprint(c) for c in children[:stable_end]]
                self._snapshot = QTextDocument()
                self._snapshot.setDefaultFont(self._document.defaultFont())
                snapshot_cursor = QTextCursor(self._snapshot)
                snapshot_cursor.insertFragment(QTextDocumentFragment(self._document))

            self.visit(child)

        # Enable all the changes to render
        cursor.endEditBlock()

        # Trim the trailing empty block that Qt always leaves.
        self._trim_empty_tail()

    def _trim_empty_tail(self) -> None:
        """
        Remove the trailing empty block Qt appends after every edit.
        """
        cursor = self._cursor
        cursor.beginEditBlock()

        if cursor.block().text() == "":
            cursor.movePosition(QTextCursor.MoveOperation.PreviousBlock)
            cursor.movePosition(QTextCursor.MoveOperation.EndOfBlock)
            cursor.movePosition(QTextCursor.MoveOperation.End, QTextCursor.MoveMode.KeepAnchor)
            cursor.removeSelectedText()

        cursor.endEditBlock()

    def visit_MarkdownASTParagraphNode(self, node: MarkdownASTParagraphNode) -> None:  # pylint: disable=invalid-name
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
            ancestor = node.parent.parent
            while isinstance(ancestor, (MarkdownASTBlockquoteNode, MarkdownASTListItemNode)):
                ancestor = ancestor.parent

            if isinstance(ancestor, (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode)):
                if ancestor.tight:
                    tight = True

        # If the previous sibling is a list or code block, we need to add a top margin
        previous_sibling = node.previous_sibling()
        if previous_sibling and isinstance(
            previous_sibling, (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode,
                               MarkdownASTCodeBlockNode, MarkdownASTBlockquoteNode)
        ):
            block_format.setTopMargin(self._default_font_height)

        # If this is the first child of a blockquote whose previous sibling is a list,
        # we also need a top margin to produce spacing after the list.
        elif (not previous_sibling and
                node.parent and isinstance(node.parent, MarkdownASTBlockquoteNode) and
                node.parent.previous_sibling() and isinstance(
                    node.parent.previous_sibling(),
                    (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode))):
            block_format.setTopMargin(self._default_font_height)

        # If there is no previous sibling, check if our parent is a list item.  If it is, we also need to add a top margin.
        elif not previous_sibling and node.parent and isinstance(node.parent, MarkdownASTListItemNode):
            if not tight:
                if node.parent.previous_sibling() is not None:
                    block_format.setTopMargin(self._default_font_height)

            elif node.parent.previous_sibling() is not None:
                # This is a tight list item that is not the first in its list.  The topMargin
                # on orig_block_format was inherited from the first block of the list (where it
                # correctly spaces the list away from a preceding sibling).  Clear it here so
                # it does not add spurious inter-item spacing.
                block_format.setTopMargin(0)

        # If the next sibling is a horizontal rule, we don't need a bottom margin
        next_sibling = node.next_sibling()
        if next_sibling and isinstance(next_sibling, MarkdownASTHorizontalRuleNode):
            block_format.setBottomMargin(0)

        # If there is no next sibling, check if our parent is a blockquote whose next sibling
        # is a horizontal rule.  If so, suppress the bottom margin for the same reason.
        elif (not next_sibling and
                node.parent and isinstance(node.parent, MarkdownASTBlockquoteNode) and
                isinstance(node.parent.next_sibling(), MarkdownASTHorizontalRuleNode)):
            block_format.setBottomMargin(0)

        # If we are in a tight list, we don't need a bottom margin either
        elif tight:
            block_format.setBottomMargin(0)

        else:
            block_format.setBottomMargin(self._default_font_height)

        block_format.setIndent(len(self._blockquote_bar_offsets))
        self._cursor.setBlockFormat(block_format)
        self._stamp_blockquote_data()

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
            self._stamp_blockquote_data()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownASTHeadingNode(self, node: MarkdownASTHeadingNode) -> None:  # pylint: disable=invalid-name
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
        if previous_sibling and not isinstance(previous_sibling, MarkdownASTHorizontalRuleNode):
            block_format.setTopMargin(self._default_font_height * multipliers[level])

        block_format.setBottomMargin(self._default_font_height)
        block_format.setIndent(len(self._blockquote_bar_offsets))
        self._cursor.setBlockFormat(block_format)

        # Store the ID as block user data
        heading_data = HeadingBlockData(node.anchor_id)
        heading_data.blockquote_bar_offsets = list(self._blockquote_bar_offsets)
        self._cursor.block().setUserData(heading_data)

        # Process all inline content for the heading
        for child in node.children:
            self.visit(child)

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()
            self._stamp_blockquote_data()

        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownASTTextNode(self, node: MarkdownASTTextNode) -> None:  # pylint: disable=invalid-name
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
                if (isinstance(prev_sibling, MarkdownASTTextNode) and
                    prev_sibling.content.rstrip() != prev_sibling.content and  # ends with whitespace
                    text.startswith(' ')):
                    text = text.lstrip(' ')

        # If the text is empty after normalization, we don't need to insert anything
        if not text:
            return

        self._cursor.insertText(text)

    def visit_MarkdownASTBoldNode(self, node: MarkdownASTBoldNode) -> None:  # pylint: disable=invalid-name
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
            if isinstance(parent, MarkdownASTHeadingNode):
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

    def visit_MarkdownASTEmphasisNode(self, node: MarkdownASTEmphasisNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownASTStrikethroughNode(self, node: MarkdownASTStrikethroughNode) -> None:  # pylint: disable=invalid-name
        """
        Render a strikethrough node to the QTextDocument.

        Args:
            node: The strikethrough node to render

        Returns:
            None
        """
        # Save current format, apply strikethrough format, then process children
        orig_char_format = self._cursor.charFormat()

        # Create a new format based on the current one but with strikethrough
        strikethrough_format = QTextCharFormat(orig_char_format)
        strikethrough_format.setFontStrikeOut(True)
        self._cursor.setCharFormat(strikethrough_format)

        for child in node.children:
            self.visit(child)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownASTInlineCodeNode(self, node: MarkdownASTInlineCodeNode) -> None:  # pylint: disable=invalid-name
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
        code_format.setFontFixedPitch(True)
        if not self._style_manager.font_ligatures():
            code_format.setFontStyleStrategy(QFont.StyleStrategy.PreferNoShaping)

        # If we are inside a link, then keep the link color, otherwise set the code color
        if not node.parent or not isinstance(node.parent, MarkdownASTLinkNode):
            code_format.setForeground(self._style_manager.get_color(ColorRole.SYNTAX_12))

        self._cursor.setCharFormat(code_format)

        self._cursor.insertText(node.content)

        # Restore previous format
        self._cursor.setCharFormat(orig_char_format)

    def visit_MarkdownASTLinkNode(self, node: MarkdownASTLinkNode) -> None:  # pylint: disable=invalid-name
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
        if len(url) > 2 and url[0].isalpha() and url[1] == ':' and url[2] in ('/', '\\'):
            return True

        return False

    def _load_local_image(self, path: str) -> Tuple[QImage, bool, bool]:
        """
        Load an image from a local file path.

        Args:
            path: The path to the image file

        Returns:
            A tuple of (QImage, success_flag, is_animated)
        """
        logger = logging.getLogger("MarkdownRenderer")

        # Convert file:// URLs to local paths
        if path.startswith('file://'):
            path = path[7:]

        # Normalize path (handle relative paths, etc.)
        try:
            # If the path is relative, resolve it against the source path
            if not os.path.isabs(path) and self._source_path:
                if os.path.isdir(self._source_path):
                    source_dir = self._source_path

                else:
                    source_dir = os.path.dirname(self._source_path)

                path = os.path.normpath(os.path.join(source_dir, path))

            elif not os.path.isabs(path):
                # Fallback to current working directory if source path is unknown
                path = os.path.normpath(os.path.abspath(path))

            # Check if file exists
            if not os.path.isfile(path):
                logger.warning("Local image not found: %s", path)
                return self._create_placeholder_image(), False, False

            # Load the image
            image = QImage(path)
            if image.isNull():
                logger.warning("Failed to load image: %s", path)
                return self._create_placeholder_image(), False, False

            # Verify the image is valid
            if image.width() <= 0 or image.height() <= 0:
                logger.warning("Invalid image dimensions: %s", path)
                return self._create_placeholder_image(), False, False

            reader = QImageReader(path)
            is_animated = reader.supportsAnimation() and reader.imageCount() > 1

            return image, True, is_animated

        except Exception:
            logger.exception("Error loading local image: %s", path)
            return self._create_placeholder_image(), False, False

    def _create_placeholder_image(self) -> QImage:
        """
        Create a placeholder image for when an image cannot be loaded.

        Returns:
            A QImage representing a placeholder
        """
        image = QImage(100, 100, QImage.Format.Format_ARGB32)
        image.fill(self._style_manager.get_color(ColorRole.BACKGROUND_SECONDARY))
        return image

    def visit_MarkdownASTImageNode(self, node: MarkdownASTImageNode) -> None:  # pylint: disable=invalid-name
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
        is_animated = False
        if self._is_local_file(node.url):
            image, loaded_successfully, is_animated = self._load_local_image(node.url)

        # Compute display dimensions and pre-scale static images so Qt doesn't have to
        # rescale on every layout pass.  When a PercentageLength is used Qt must recompute
        # the scaled dimensions from the full-resolution source on each layout, which is
        # very expensive for large images during scrolling.  Animated GIFs are not
        # pre-scaled here — the QMovie handles frame scaling via setScaledSize instead.
        display_width = 0
        display_height = 0
        if loaded_successfully:
            doc_width = self._document.textWidth()
            if 0 < doc_width < image.width():
                scale = doc_width / image.width()
                display_width = int(doc_width)
                display_height = int(image.height() * scale)
                if not is_animated:
                    scaled = image.scaled(
                        display_width, display_height,
                        Qt.AspectRatioMode.IgnoreAspectRatio,
                        Qt.TransformationMode.SmoothTransformation
                    )
                    if not scaled.isNull():
                        image = scaled

            else:
                display_width = image.width()
                display_height = image.height()

        # Create a resource for this image.  We always update the resource (even on
        # re-renders) because the image may have been pre-scaled to a different document width.
        resource_name = f"image_{hash(node.url)}"
        existing = self._document.resource(QTextDocument.ResourceType.ImageResource, resource_name)
        needs_update = existing is None
        if not needs_update and loaded_successfully and isinstance(existing, QImage):
            needs_update = existing.width() != image.width() or existing.height() != image.height()

        if needs_update:
            self._document.addResource(QTextDocument.ResourceType.ImageResource, resource_name, image)

        if is_animated and self._animated_gif_callback is not None:
            self._animated_gif_callback(resource_name, node.url, display_width, display_height)

        # Create an image format
        img_format = QTextImageFormat()
        img_format.setName(resource_name)

        # Set explicit pixel dimensions for the pre-scaled image so Qt can use
        # cached layout metrics instead of recomputing on every layout pass.
        if loaded_successfully:
            if display_width > 0:
                img_format.setWidth(display_width)

            if display_height > 0:
                img_format.setHeight(display_height)

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

    def visit_MarkdownASTCodeBlockNode(self, node: MarkdownASTCodeBlockNode) -> None:  # pylint: disable=invalid-name
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
        block_format.setIndent(len(self._blockquote_bar_offsets))

        # Add a top margin when following a list or a blockquote.
        if isinstance(node.previous_sibling(), (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode,
                                                MarkdownASTBlockquoteNode)):
            block_format.setTopMargin(self._default_font_height)

        # Apply the block format
        self._cursor.setBlockFormat(block_format)
        self._stamp_blockquote_data()

        # If we're inside a list, attach each code block line to a ListStyleUndefined
        # list at the same indent level so it aligns with the surrounding list content.
        # A code block inside a list item is always a non-first sibling, so we always
        # need this indentation.
        list_for_code: QTextList | None = None
        if self._lists:
            list_fmt = QTextListFormat(self._lists[-1].format())
            list_fmt.setStyle(QTextListFormat.Style.ListStyleUndefined)
            list_for_code = self._cursor.createList(list_fmt)
            list_for_code.setFormat(list_fmt)
            list_for_code.add(self._cursor.block())

        # Apply code formatting and insert text
        code_format = QTextCharFormat(orig_char_format)
        code_format.setFontFamilies(self._style_manager.monospace_font_families())
        code_format.setFontFixedPitch(True)
        if not self._style_manager.font_ligatures():
            code_format.setFontStyleStrategy(QFont.StyleStrategy.PreferNoShaping)

        # Split content by lines and add each in its own block
        lines = node.content.split('\n')

        for i, line in enumerate(lines):
            if i > 0:
                self._cursor.insertBlock(block_format)
                self._stamp_blockquote_data()
                if list_for_code:
                    list_for_code.add(self._cursor.block())

            # Apply syntax highlighting if we have pre-computed tokens
            if i < len(node.tokens_by_line) and node.tokens_by_line[i]:
                tokens = node.tokens_by_line[i]
                last_token_pos = 0

                for token in tokens:
                    # Create a format for this token with syntax highlighting
                    token_format = QTextCharFormat(code_format)
                    token_format.setForeground(self._style_manager.get_highlight(token.type).foreground())

                    # Insert any whitespace before this token
                    if token.start > last_token_pos:
                        whitespace_format = QTextCharFormat(code_format)
                        whitespace_format.setForeground(self._style_manager.get_highlight(TokenType.TEXT).foreground())
                        self._cursor.setCharFormat(whitespace_format)
                        self._cursor.insertText(line[last_token_pos:token.start])

                    # Insert the token with its highlighting
                    self._cursor.setCharFormat(token_format)
                    self._cursor.insertText(token.value)
                    last_token_pos = token.start + len(token.value)

                # Insert any trailing whitespace
                if last_token_pos < len(line):
                    whitespace_format = QTextCharFormat(code_format)
                    whitespace_format.setForeground(self._style_manager.get_highlight(TokenType.TEXT).foreground())
                    self._cursor.setCharFormat(whitespace_format)
                    self._cursor.insertText(line[last_token_pos:])

            else:
                # No tokens available, insert plain text
                self._cursor.setCharFormat(code_format)
                self._cursor.insertText(line)

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()
            self._stamp_blockquote_data()

        self._cursor.setBlockFormat(orig_block_format)

    def _list_node_visitor(self, node: MarkdownASTOrderedListNode | MarkdownASTUnorderedListNode) -> None:
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

        # Add a bottom margin when the next sibling is another list or a table,
        # to visually separate them.  Otherwise suppress the margin so list items
        # don't have extra space below the last one.
        # If the last block belongs to a blockquote, preserve the half-row padding
        # that the blockquote visitor added so the background and bar cover it.
        blockquote_extra = 0.0
        last_block_data = self._cursor.block().userData()
        if isinstance(last_block_data, MarkdownBlockData) and last_block_data.blockquote_bar_offsets:
            blockquote_extra = self._default_font_height * 0.5

        next_sib = node.next_sibling()
        if isinstance(next_sib, (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode, MarkdownASTTableNode)):
            block_format.setBottomMargin(self._default_font_height)

        else:
            block_format.setBottomMargin(blockquote_extra)

        self._cursor.setBlockFormat(block_format)

        if at_block_start:
            self._cursor.movePosition(QTextCursor.MoveOperation.NextBlock)

        # Exit this list level
        self._list_level -= 1

        self._cursor.setCharFormat(orig_char_format)

        if not self._cursor.atBlockStart():
            self._cursor.insertBlock()

    def visit_MarkdownASTOrderedListNode(self, node: MarkdownASTOrderedListNode) -> None:  # pylint: disable=invalid-name
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
        # Add a top margin when following another list or a blockquote.
        if isinstance(node.previous_sibling(), (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode,
                                                MarkdownASTBlockquoteNode)):
            block_format.setTopMargin(self._default_font_height)

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

    def visit_MarkdownASTUnorderedListNode(self, node: MarkdownASTUnorderedListNode) -> None:  # pylint: disable=invalid-name
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
        # Add a top margin when following another list or a blockquote.
        if isinstance(node.previous_sibling(), (MarkdownASTOrderedListNode, MarkdownASTUnorderedListNode,
                                                MarkdownASTBlockquoteNode)):
            block_format.setTopMargin(self._default_font_height)

        self._cursor.setBlockFormat(block_format)

        # Set indentation based on nesting level
        list_format = QTextListFormat()
        list_format.setStyle(QTextListFormat.Style.ListDisc)
        list_format.setIndent(self._list_level)

        new_list = self._cursor.createList(list_format)
        self._lists.append(new_list)
        self._list_node_visitor(node)
        self._cursor.setBlockFormat(orig_block_format)

    def visit_MarkdownASTListItemNode(self, node: MarkdownASTListItemNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownASTLineBreakNode(self, node: MarkdownASTLineBreakNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownASTTableNode(self, node: MarkdownASTTableNode) -> None:  # pylint: disable=invalid-name
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

        # If we're inside a list, insert a clean block attached to a ListStyleUndefined
        # list before the table frame.  Without this Qt associates the frame's anchor
        # block with the active list and renders a spurious bullet to the left of the
        # table.  This mirrors the same technique used for paragraphs and code blocks.
        if self._lists:
            if not self._cursor.atBlockStart():
                self._cursor.insertBlock()

            list_fmt = QTextListFormat(self._lists[-1].format())
            list_fmt.setStyle(QTextListFormat.Style.ListStyleUndefined)
            pre_table_list = self._cursor.createList(list_fmt)
            pre_table_list.add(self._cursor.block())

        # Add a top margin when following a blockquote.
        if isinstance(node.previous_sibling(), MarkdownASTBlockquoteNode):
            sep_format = QTextBlockFormat(orig_block_format)
            sep_format.setTopMargin(self._default_font_height)
            self._cursor.setBlockFormat(sep_format)
            self._stamp_blockquote_data()

        # Verify that this table has at least one header row and one body row before rendering
        has_valid_structure = False

        for child in node.children:
            if isinstance(child, MarkdownASTTableHeaderNode) and child.children:
                for header_child in node.children:
                    if isinstance(header_child, MarkdownASTTableBodyNode) and header_child.children:
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

    def _render_table_as_text(self, node: MarkdownASTTableNode) -> None:
        """
        Render a table as plain text when it has an invalid structure.

        Args:
            node: The table node to render

        Returns:
            None
        """
        # Create a paragraph for each row in the table
        for child in node.children:
            if isinstance(child, MarkdownASTTableHeaderNode):
                for row in child.children:
                    paragraph = MarkdownASTParagraphNode()

                    # Create a text representation of the row
                    row_text = "|"
                    for cell in row.children:
                        cell_text = ""
                        for content in cell.children:
                            if isinstance(content, MarkdownASTTextNode):
                                cell_text += content.content

                        row_text += f" {cell_text} |"

                    paragraph.add_child(MarkdownASTTextNode(row_text))
                    self.visit(paragraph)

            if isinstance(child, MarkdownASTTableBodyNode):
                for row in child.children:
                    paragraph = MarkdownASTParagraphNode()

                    # Create a text representation of the row
                    row_text = "|"
                    for cell in row.children:
                        cell_text = ""
                        for content in cell.children:
                            if isinstance(content, MarkdownASTTextNode):
                                cell_text += content.content

                        row_text += f" {cell_text} |"

                    paragraph.add_child(MarkdownASTTextNode(row_text))
                    self.visit(paragraph)

    def visit_MarkdownASTTableHeaderNode(self, node: MarkdownASTTableHeaderNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownASTTableBodyNode(self, node: MarkdownASTTableBodyNode) -> None:  # pylint: disable=invalid-name
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

    def visit_MarkdownASTTableRowNode(self, node: MarkdownASTTableRowNode) -> None:  # pylint: disable=invalid-name
        """
        Render a table row node to the QTextDocument.

        Args:
            node: The table row node to render

        Returns:
            None
        """
        # Is this the first row of a table?
        is_first_row = isinstance(node.parent, MarkdownASTTableHeaderNode) and node is node.parent.children[0]

        # If this is the first row, we need to create the table
        if is_first_row:
            # Count the cells to determine column count
            column_count = len(node.children)
            if column_count == 0:
                # Can't create a table with no columns, fallback to text
                return

            # Count all rows in header and body to determine row count
            header = cast(MarkdownASTTableHeaderNode, node.parent)
            body = None

            # Find the table body (sibling of header)
            table_node = cast(MarkdownASTTableNode, header.parent)
            for sibling in table_node.children:
                if isinstance(sibling, MarkdownASTTableBodyNode):
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

            # When inside a list or blockquote, offset the table's left edge to align with the list
            # indent, and reduce the width by the same amount so it doesn't overflow.
            # QTextTable is a QTextFrame and its percentage width is always relative to
            # the full document text width, so we must use a fixed pixel width here.
            doc_width = self._document.textWidth()
            indent_depth = self._list_level + len(self._blockquote_bar_offsets)
            if indent_depth > 0 and doc_width > 0:
                left_margin = indent_depth * self._document.indentWidth()
                table_format.setLeftMargin(left_margin)
                table_format.setWidth(QTextLength(QTextLength.Type.FixedLength, doc_width))

            elif doc_width > 0:
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
                if isinstance(cell_node, MarkdownASTTableCellNode):
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
        if ((isinstance(node.parent, MarkdownASTTableHeaderNode) and
                node is node.parent.children[-1] and
                not any(
                    isinstance(sibling, MarkdownASTTableBodyNode)
                    for sibling in cast(MarkdownASTTableNode, node.parent.parent).children
                )
            ) or
            (isinstance(node.parent, MarkdownASTTableBodyNode) and node is node.parent.children[-1])
        ):
            # Reset table state
            self._current_table = None
            self._current_row = 0

    def visit_MarkdownASTTableCellNode(self, node: MarkdownASTTableCellNode) -> None:  # pylint: disable=invalid-name
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

    def _stamp_blockquote_data(self) -> None:
        """
        Stamp blockquote_bar_offsets onto the current block's MarkdownBlockData.

        If the block already has a MarkdownBlockData (e.g. HeadingBlockData), the offsets
        are written onto it directly.  Otherwise a plain MarkdownBlockData is created.
        Always writes the offsets, including empty, so that blocks pre-stamped by a
        previous blockquote visitor are correctly cleared when rendered outside one.
        """
        existing = self._cursor.block().userData()
        if isinstance(existing, MarkdownBlockData):
            existing.blockquote_bar_offsets = list(self._blockquote_bar_offsets)

        else:
            if not self._blockquote_bar_offsets:
                return

            data = MarkdownBlockData()
            data.blockquote_bar_offsets = list(self._blockquote_bar_offsets)
            self._cursor.block().setUserData(data)

    def visit_MarkdownASTBlockquoteNode(self, node: MarkdownASTBlockquoteNode) -> None:  # pylint: disable=invalid-name
        """
        Render a blockquote node to the QTextDocument.

        Args:
            node: The blockquote node to render

        Returns:
            None
        """
        self._blockquote_bar_offsets.append(self._list_level)

        if self._lists:
            list_fmt = QTextListFormat(self._lists[-1].format())
            list_fmt.setStyle(QTextListFormat.Style.ListStyleUndefined)
            list_for_blockquote = self._cursor.createList(list_fmt)
            list_for_blockquote.setFormat(list_fmt)
            list_for_blockquote.add(self._cursor.block())

        for child in node.children:
            self.visit(child)

        self._blockquote_bar_offsets.pop()

    def visit_MarkdownASTHorizontalRuleNode(self, node: MarkdownASTHorizontalRuleNode) -> None:  # pylint: disable=invalid-name
        """
        Render a horizontal rule node to the QTextDocument.

        Args:
            node: The horizontal rule node to render

        Returns:
            None
        """
        orig_block_format = self._cursor.blockFormat()

        # Insert blocks after the HR with clean formatting (no margins)
        clean_block_format = QTextBlockFormat()
        clean_block_format.setTopMargin(0)
        clean_block_format.setBottomMargin(0)
        self._cursor.setBlockFormat(clean_block_format)

        # We need to add a new block before the horizontal rule to ensure proper spacing.
        # However, if the previous sibling is a table then we don't need to do this because
        # one already exists.
        previous_sibling = node.previous_sibling()
        if not previous_sibling or not isinstance(previous_sibling, MarkdownASTTableNode):
            self._cursor.insertBlock()

        # This is a workaround for the fact that QTextDocument doesn't support
        # horizontal rules directly.  We use an image resource instead.
        img_format = QTextImageFormat()
        img_format.setName("pixel")
        img_format.setMaximumWidth(QTextLength(QTextLength.Type.PercentageLength, 100))
        img_format.setHeight(1)
        self._cursor.insertImage(img_format)

        # We neeed to add 2 new blocks after the horizontal rule to ensure proper spacing.
        self._cursor.insertBlock()
        self._cursor.insertBlock()

        # Restore the original block format
        self._cursor.setBlockFormat(orig_block_format)
