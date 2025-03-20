"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

import logging
import re
from typing import Dict, List, Tuple, Any, Set, Optional

from humbug.gui.tab.conversation.conversation_markdown_ast_node import (
    ASTNode, Document, Text, Emphasis, Bold, Heading, Paragraph,
    OrderedList, UnorderedList, ListItem, MarkdownParseError, CodeBlock
)


class ASTBuilder:
    """
    Builder class for constructing an AST from markdown text.

    This class handles the incremental parsing of markdown text into an
    Abstract Syntax Tree (AST) representation.
    """

    def __init__(self):
        """Initialize the AST builder with regex patterns for markdown elements."""
        # Regular expressions for markdown elements
        self._heading_pattern = re.compile(r'^(#{1,6})\s+(.*?)(?:\s+#{1,6})?$', re.MULTILINE)
        self._bold_pattern = re.compile(r'\*\*(.*?)\*\*|\b__(.*?)__\b')
        self._italic_pattern = re.compile(r'\*([^*]+)\*|\b_([^_]+)_\b')
        self._unordered_list_pattern = re.compile(r'^(\s*)([*+-])\s+(.*?)$', re.MULTILINE)
        self._ordered_list_pattern = re.compile(r'^(\s*)(\d+)\.[ \t]+(.*?)$', re.MULTILINE)
        self._code_block_pattern = re.compile(r'^```(?:(\w+))?$')

        self._logger = logging.getLogger("ASTBuilder")

        # Initialize an empty document
        self.document = Document()

        # Mapping from line numbers to nodes for incremental updates
        self.line_to_node_map: Dict[int, List[ASTNode]] = {}

        # List state tracking
        self.active_lists: List[Tuple[ASTNode, int]] = []  # (list_node, indent)
        self.list_contains_blank_line: Set[ASTNode] = set()  # Lists that have blank lines

        # Text continuation tracking
        self.last_paragraph: Optional[Paragraph] = None
        self.last_list_item: Optional[ListItem] = None
        self.last_processed_line_type: str = ""
        self.blank_line_count: int = 0

        # Code block state tracking
        self.in_code_block = False
        self.code_block_language = ""
        self.code_block_content = []
        self.code_block_start_line = -1

    def escape_html(self, text: str) -> str:
        """
        Escape special HTML characters in text.

        Args:
            text: The text to escape

        Returns:
            The escaped text

        Raises:
            None
        """
        text = text.replace('&', '&amp;')  # Must come first to avoid double-escaping
        text = text.replace('<', '&lt;')
        text = text.replace('>', '&gt;')
        return text

    def identify_line_type(self, line: str) -> Tuple[str, Any]:
        """
        Identify the type of a markdown line.

        Args:
            line: The line to identify

        Returns:
            A tuple of (line_type, content) where line_type is one of:
            'heading', 'unordered_list_item', 'ordered_list_item', 'blank', 'text',
            'code_block_start', 'code_block_end', 'code_block_content'

        Raises:
            None
        """
        # Handle code block state
        if self.in_code_block:
            # Check for code block end
            if line.strip() == '```':
                return 'code_block_end', None
            return 'code_block_content', line

        # Check for code block start
        code_block_match = self._code_block_pattern.match(line.strip())
        if code_block_match:
            language = code_block_match.group(1) or ""
            return 'code_block_start', language

        if not line.strip():
            return 'blank', None

        # Check for heading
        heading_match = self._heading_pattern.match(line)
        if heading_match:
            level = len(heading_match.group(1))
            content = heading_match.group(2).strip()
            return 'heading', (level, content)

        # Check for unordered list item
        unordered_match = self._unordered_list_pattern.match(line)
        if unordered_match:
            indent = len(unordered_match.group(1))
            marker = unordered_match.group(2)
            content = unordered_match.group(3)
            return 'unordered_list_item', (indent, marker, content)

        # Check for ordered list item
        ordered_match = self._ordered_list_pattern.match(line)
        if ordered_match:
            indent = len(ordered_match.group(1))
            number = ordered_match.group(2)
            content = ordered_match.group(3)
            return 'ordered_list_item', (indent, number, content)

        # Default to regular text
        return 'text', line

    def parse_inline_formatting(self, text: str) -> List[ASTNode]:
        """
        Parse inline formatting (bold, italic) in text and create appropriate AST nodes.

        Args:
            text: The text to parse

        Returns:
            A list of AST nodes representing the formatted text

        Raises:
            None
        """
        # Escape HTML characters first
        text = self.escape_html(text)
        nodes = []

        # Process bold formatting
        bold_segments = []
        last_end = 0

        for match in self._bold_pattern.finditer(text):
            start, end = match.span()
            # Add text before this match
            if start > last_end:
                bold_segments.append(('text', text[last_end:start]))

            # Add the bold content
            content = match.group(1) or match.group(2)
            if content:
                bold_segments.append(('bold', content))
            else:
                bold_segments.append(('text', '**'))

            last_end = end

        # Add any remaining text
        if last_end < len(text):
            bold_segments.append(('text', text[last_end:]))

        # Process italic formatting in each segment
        for segment_type, segment_text in bold_segments:
            if segment_type == 'text':
                # Process italics in regular text
                italic_segments = []
                last_end = 0

                for match in self._italic_pattern.finditer(segment_text):
                    start, end = match.span()
                    # Add text before this match
                    if start > last_end:
                        italic_segments.append(Text(segment_text[last_end:start]))

                    # Add the italic content
                    content = match.group(1) or match.group(2)
                    if content:
                        emphasis = Emphasis()
                        emphasis.add_child(Text(content))
                        italic_segments.append(emphasis)
                    else:
                        italic_segments.append(Text('*'))

                    last_end = end

                # Add any remaining text
                if last_end < len(segment_text):
                    italic_segments.append(Text(segment_text[last_end:]))

                nodes.extend(italic_segments)
            elif segment_type == 'bold':
                # Create a bold node with its text content
                bold = Bold()

                # Process italics within the bold text
                italic_segments = []
                last_end = 0

                for match in self._italic_pattern.finditer(segment_text):
                    start, end = match.span()
                    # Add text before this match
                    if start > last_end:
                        italic_segments.append(Text(segment_text[last_end:start]))

                    # Add the italic content
                    content = match.group(1) or match.group(2)
                    if content:
                        emphasis = Emphasis()
                        emphasis.add_child(Text(content))
                        italic_segments.append(emphasis)
                    else:
                        italic_segments.append(Text('*'))

                    last_end = end

                # Add any remaining text
                if last_end < len(segment_text):
                    italic_segments.append(Text(segment_text[last_end:]))

                # Add all italic segments to the bold node
                for node in italic_segments:
                    bold.add_child(node)

                nodes.append(bold)

        return nodes

    def handle_line_breaks(self, text: str) -> str:
        """
        Process text to convert trailing spaces to HTML line breaks.

        Args:
            text: The text to process

        Returns:
            Text with appropriate line breaks

        Raises:
            None
        """
        if text.endswith('  '):  # Line ends with exactly two spaces
            return text.rstrip() + '<br />'
        return text

    def parse_heading(self, level: int, content: str, line_num: int) -> Heading:
        """
        Parse a heading line and create a heading node.

        Args:
            level: The heading level (1-6)
            content: The heading content
            line_num: The line number

        Returns:
            A heading node

        Raises:
            None
        """
        heading = Heading(level)
        formatted_text = self.handle_line_breaks(content)
        for node in self.parse_inline_formatting(formatted_text):
            heading.add_child(node)

        heading.line_start = line_num
        heading.line_end = line_num
        self.register_node_line(heading, line_num)

        return heading

    def parse_text(self, text: str, line_num: int) -> Paragraph:
        """
        Parse a text line and create a paragraph node.

        Args:
            text: The text content
            line_num: The line number

        Returns:
            A paragraph node

        Raises:
            None
        """
        paragraph = Paragraph()
        formatted_text = self.handle_line_breaks(text)
        for node in self.parse_inline_formatting(formatted_text):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        self.register_node_line(paragraph, line_num)

        return paragraph

    def find_or_create_list(self, indent: int, is_ordered: bool) -> ASTNode:
        """
        Find an existing list at the given indent level or create a new one.

        Args:
            indent: The indentation level
            is_ordered: Whether this is an ordered list

        Returns:
            A list node (either OrderedList or UnorderedList)

        Raises:
            None
        """
        # Close deeper lists
        while self.active_lists and self.active_lists[-1][1] > indent:
            self.active_lists.pop()

        # Check if we have a list at this level
        if self.active_lists and self.active_lists[-1][1] == indent:
            list_node, list_indent = self.active_lists[-1]

            # If list type matches, use it
            if (isinstance(list_node, OrderedList) and is_ordered) or \
               (isinstance(list_node, UnorderedList) and not is_ordered):
                return list_node

            # Otherwise, close this list and create a new one
            self.active_lists.pop()

        # Create a new list
        parent = self.document
        if self.active_lists:
            # Find the closest parent - either a list item or the document
            for i in range(len(self.active_lists) - 1, -1, -1):
                list_node, _ = self.active_lists[i]
                if list_node.children:
                    parent = list_node.children[-1]  # Last list item
                    break

        # Create the appropriate list type
        if is_ordered:
            new_list = OrderedList(indent)
        else:
            new_list = UnorderedList(indent)

        parent.add_child(new_list)
        self.active_lists.append((new_list, indent))

        return new_list

    def _add_paragraph_to_list_item(self, list_item: ListItem, content: str, line_num: int) -> None:
        """
        Add a paragraph to a list item, respecting the list's formatting style.

        Args:
            list_item: The list item to add content to
            content: The text content to add
            line_num: The line number

        Returns:
            None
        """
        paragraph = Paragraph()
        formatted_text = self.handle_line_breaks(content)
        for node in self.parse_inline_formatting(formatted_text):
            paragraph.add_child(node)

        paragraph.line_start = line_num
        paragraph.line_end = line_num
        list_item.add_child(paragraph)
        self.register_node_line(paragraph, line_num)

    def parse_list_item(self, indent: int, marker: str, content: str, line_num: int, is_ordered: bool) -> ListItem:
        """
        Parse a list item and create a list item node.

        Args:
            indent: The indentation level
            marker: The list marker (bullet or number)
            content: The item content
            line_num: The line number
            is_ordered: Whether this is an ordered list item

        Returns:
            A list item node

        Raises:
            None
        """
        # Find the appropriate list to add this item to
        list_node = self.find_or_create_list(indent, is_ordered)

        # Create the list item
        item = ListItem()
        list_node.add_child(item)

        # Check if this list has blank lines, which means we need to use paragraphs for content
        if list_node in self.list_contains_blank_line:
            self._add_paragraph_to_list_item(item, content, line_num)
        else:
            # Process the content with inline formatting
            formatted_text = self.handle_line_breaks(content)
            for node in self.parse_inline_formatting(formatted_text):
                item.add_child(node)

        item.line_start = line_num
        item.line_end = line_num
        self.register_node_line(item, line_num)

        # Update tracking variables
        self.last_list_item = item

        return item

    def register_node_line(self, node: ASTNode, line_num: int) -> None:
        """
        Register a node with a line number for later reference.

        Args:
            node: The AST node
            line_num: The line number

        Returns:
            None

        Raises:
            None
        """
        if line_num not in self.line_to_node_map:
            self.line_to_node_map[line_num] = []
        self.line_to_node_map[line_num].append(node)

    def _finalize_code_block(self, end_line: int) -> None:
        """
        Finalize a code block that might be unclosed at document end.

        Args:
            end_line: The last line number in the document

        Returns:
            None
        """
        # Create a code block node for the unclosed block
        code_block = CodeBlock(
            language=self.code_block_language,
            content=self.escape_html('\n'.join(self.code_block_content))
        )
        code_block.line_start = self.code_block_start_line
        code_block.line_end = end_line

        # Add to document
        self.document.add_child(code_block)

        # Register code block with all lines it spans
        for i in range(self.code_block_start_line, end_line + 1):
            self.register_node_line(code_block, i)

        # Reset code block state
        self.in_code_block = False
        self.code_block_language = ""
        self.code_block_content = []
        self.code_block_start_line = -1

    def _handle_text_continuation(self, text: str, line_num: int) -> bool:
        """
        Handle text as a continuation of the previous paragraph or list item.

        Args:
            text: The text content
            line_num: The line number

        Returns:
            True if handled as a continuation, False otherwise
        """
        # Can only continue if no blank lines were encountered
        if self.blank_line_count > 0:
            return False

        # Case 1: Continue a paragraph
        if self.last_paragraph and self.last_processed_line_type == 'text':
            formatted_text = self.handle_line_breaks(text)
            # Add a space between the continued text
            self.last_paragraph.add_child(Text(" "))
            for node in self.parse_inline_formatting(formatted_text):
                self.last_paragraph.add_child(node)
            self.last_paragraph.line_end = line_num
            self.register_node_line(self.last_paragraph, line_num)
            return True

        # Case 2: Continue a list item
        elif self.last_list_item and self.last_processed_line_type in ('unordered_list_item', 'ordered_list_item'):
            formatted_text = self.handle_line_breaks(text)

            # Check if the list has blank lines (uses paragraph formatting)
            for list_node, _ in self.active_lists:
                if list_node in self.list_contains_blank_line:
                    # Create a new paragraph for this continuation
                    self._add_paragraph_to_list_item(self.last_list_item, text, line_num)
                    return True

            # Otherwise continue inline
            self.last_list_item.add_child(Text(" "))
            for node in self.parse_inline_formatting(formatted_text):
                self.last_list_item.add_child(node)
            self.last_list_item.line_end = line_num
            self.register_node_line(self.last_list_item, line_num)
            return True

        return False

    def _handle_blank_line_in_list(self) -> None:
        """
        Mark all active lists as containing blank lines, which affects their formatting.

        Returns:
            None
        """
        for list_node, _ in self.active_lists:
            self.list_contains_blank_line.add(list_node)

    def parse_line(self, line: str, line_num: int) -> None:
        """
        Parse a single line and add the resulting nodes to the AST.

        Args:
            line: The line to parse
            line_num: The line number

        Returns:
            None

        Raises:
            MarkdownParseError: If there's an error parsing the line
        """
        try:
            line_type, content = self.identify_line_type(line)

            # Reset paragraph tracking if not continuing text
            if line_type != 'text' and line_type != 'blank':
                self.last_paragraph = None

            # Handle blank lines for list state
            if line_type == 'blank':
                self.blank_line_count += 1
                # If we're in a list, mark it as having blank lines
                if self.active_lists:
                    self._handle_blank_line_in_list()
            else:
                self.blank_line_count = 0

            # Handle code blocks
            if line_type == 'code_block_start':
                self.in_code_block = True
                self.code_block_language = content
                self.code_block_content = []
                self.code_block_start_line = line_num
                self.last_processed_line_type = line_type
                return

            if line_type == 'code_block_content':
                self.code_block_content.append(content)
                self.last_processed_line_type = line_type
                return

            if line_type == 'code_block_end':
                # Create a code block node
                code_block = CodeBlock(
                    language=self.code_block_language,
                    content=self.escape_html('\n'.join(self.code_block_content))
                )
                code_block.line_start = self.code_block_start_line
                code_block.line_end = line_num

                # Add to document
                self.document.add_child(code_block)

                # Register code block with all lines it spans
                for i in range(self.code_block_start_line, line_num + 1):
                    self.register_node_line(code_block, i)

                # Reset code block state
                self.in_code_block = False
                self.code_block_language = ""
                self.code_block_content = []
                self.code_block_start_line = -1

                # Reset list tracking and other state after a code block
                self.active_lists = []
                self.list_contains_blank_line = set()
                self.last_paragraph = None
                self.last_list_item = None
                self.last_processed_line_type = line_type
                return

            if line_type == 'heading':
                level, heading_text = content
                heading = self.parse_heading(level, heading_text, line_num)
                self.document.add_child(heading)
                # Reset list tracking after a heading
                self.active_lists = []
                self.list_contains_blank_line = set()
                self.last_list_item = None

            elif line_type == 'unordered_list_item':
                indent, marker, text = content
                self.last_list_item = self.parse_list_item(indent, marker, text, line_num, False)

            elif line_type == 'ordered_list_item':
                indent, number, text = content
                self.last_list_item = self.parse_list_item(indent, number, text, line_num, True)

            elif line_type == 'blank':
                # Blank lines are handled above for list state
                pass

            elif line_type == 'text':
                # Try to handle as a continuation first
                if not self._handle_text_continuation(content, line_num):
                    # Regular paragraph
                    paragraph = self.parse_text(content, line_num)
                    self.document.add_child(paragraph)
                    self.last_paragraph = paragraph
                    # Reset list tracking after a paragraph
                    self.active_lists = []
                    self.list_contains_blank_line = set()
                    self.last_list_item = None

            # Update the last processed line type
            self.last_processed_line_type = line_type

        except Exception as e:
            self._logger.exception("Error parsing line %d: %s", line_num, line)
            raise MarkdownParseError(f"Failed to parse line {line_num}: {e}") from e

    def build_ast(self, text: str) -> Document:
        """
        Build a complete AST from the given text.

        Args:
            text: The markdown text to parse

        Returns:
            The document root node

        Raises:
            MarkdownParseError: If there's an error parsing the text
        """
        self.document = Document()
        self.line_to_node_map = {}
        self.active_lists = []
        self.list_contains_blank_line = set()
        self.last_paragraph = None
        self.last_list_item = None
        self.last_processed_line_type = ""
        self.blank_line_count = 0
        self.in_code_block = False
        self.code_block_language = ""
        self.code_block_content = []
        self.code_block_start_line = -1

        lines = text.split('\n')
        for i, line in enumerate(lines):
            self.parse_line(line, i)

        # Handle case where document ends while still in a code block
        if self.in_code_block:
            self._finalize_code_block(len(lines) - 1)

        return self.document

    def update_ast(self, text: str, previous_text: str = None) -> Document:
        """
        Update the AST incrementally based on changes between previous_text and text.

        Args:
            text: The new markdown text
            previous_text: The previous markdown text, or None if this is the first update

        Returns:
            The updated document root node

        Raises:
            MarkdownParseError: If there's an error updating the AST
        """
        if previous_text is None or not self.document.children:
            # First update or empty document, build from scratch
            return self.build_ast(text)

        # Split into lines
        new_lines = text.split('\n')
        old_lines = previous_text.split('\n')

        # Find common prefix length (unchanged lines)
        common_prefix_len = 0
        for i, (old, new) in enumerate(zip(old_lines, new_lines)):
            if old == new:
                common_prefix_len = i + 1
            else:
                break

        # Find common suffix length (unchanged lines from the end)
        common_suffix_len = 0
        for i, (old, new) in enumerate(zip(reversed(old_lines), reversed(new_lines))):
            if old == new and common_prefix_len + common_suffix_len < min(len(old_lines), len(new_lines)):
                common_suffix_len = i + 1
            else:
                break

        # Calculate changed region
        old_start = common_prefix_len
        old_end = len(old_lines) - common_suffix_len
        new_start = common_prefix_len
        new_end = len(new_lines) - common_suffix_len

        # If nothing changed, return existing document
        if old_start >= old_end and new_start >= new_end:
            return self.document

        # For efficiency in highly incremental scenarios (e.g., typing at the end),
        # handle the common case of appending to the document
        if old_start == len(old_lines) and new_start == old_start:
            # We're just appending lines - parse only the new lines
            # Save existing state
            saved_document = self.document
            saved_line_map = self.line_to_node_map.copy()

            # Parse just the new lines
            try:
                for i, line in enumerate(new_lines[old_start:], start=old_start):
                    self.parse_line(line, i)

                # Handle case where document ends while still in a code block
                if self.in_code_block:
                    self._finalize_code_block(len(new_lines) - 1)

                return self.document
            except Exception:
                # If incremental update fails, fall back to full rebuild
                self._logger.exception("Incremental append failed, falling back to full rebuild")
                self.document = saved_document
                self.line_to_node_map = saved_line_map

        # For more complex edits, do a full rebuild
        return self.build_ast(text)
