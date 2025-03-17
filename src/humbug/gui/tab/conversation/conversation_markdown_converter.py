"""
Utility for converting simplified markdown text to HTML.

This module provides functionality to incrementally convert simplified markdown
to HTML while preserving code blocks and handling streaming text updates.
"""

import re
from typing import List, Tuple, Dict, Optional


class ConversationMarkdownConverter:
    """
    Converts simplified markdown to HTML.

    This class handles the incremental conversion of simplified markdown to HTML,
    focusing on headings, unordered lists, ordered lists, bold, and italic formatting
    with proper handling of block-level vs inline elements.
    """

    def __init__(self):
        """Initialize the markdown converter."""
        # Regular expressions for markdown elements
        self._heading_pattern = re.compile(r'^(#{1,6})\s+(.*?)(?:\s+#{1,6})?$', re.MULTILINE)
        self._bold_pattern = re.compile(r'\*\*(.*?)\*\*|\b__(.*?)__\b')
        self._italic_pattern = re.compile(r'\*([^*]+)\*|\b_([^_]+)_\b')

        # List item patterns - capture leading spaces, list marker, and content
        self._unordered_list_pattern = re.compile(r'^(\s*)([*+-])\s+(.*?)$', re.MULTILINE)
        self._ordered_list_pattern = re.compile(r'^(\s*)(\d+)\.[ \t]+(.*?)$', re.MULTILINE)

        # Track block state for incremental conversion
        self._current_blocks: List[str] = []
        self._block_converted: List[bool] = []
        self._block_html: List[str] = []

        # Track the part of the last block we've processed so far
        self._partial_last_block = ""

    def _split_into_blocks(self, text: str) -> List[str]:
        """
        Split text into logical blocks for processing.

        This method separates text at blank lines, and ensures
        headings start new blocks even without blank lines.

        Args:
            text: The text to split

        Returns:
            List of text blocks
        """
        blocks = []
        current_block = []

        lines = text.split('\n')
        for i, line in enumerate(lines):
            # Check if line is a heading
            is_heading = self._heading_pattern.match(line)

            # Start a new block if:
            # 1. We encounter a heading (and it's not the first line/block is not empty)
            # 2. We encounter a blank line (and current block is not empty)
            if is_heading and current_block:
                blocks.append('\n'.join(current_block))
                current_block = [line]
            elif line.strip() == "" and current_block:
                blocks.append('\n'.join(current_block))
                current_block = []
            # Otherwise add to the current block
            elif line.strip() != "" or current_block:  # Skip consecutive blank lines
                current_block.append(line)

        # Add the last block if there is one
        if current_block:
            blocks.append('\n'.join(current_block))

        return blocks

    def _apply_inline_formatting(self, text: str) -> str:
        """
        Apply inline formatting (bold, italic) to text.

        Args:
            text: The text to format

        Returns:
            Text with inline formatting applied
        """
        # First, escape special HTML characters
        # This needs to be done before applying formatting
        def escape_html(text):
            # Replace < with &lt; and > with &gt;
            text = text.replace('&', '&amp;')  # Must come first to avoid double-escaping
            text = text.replace('<', '&lt;')
            text = text.replace('>', '&gt;')
            return text

        # Escape the text first
        text = escape_html(text)

        # Apply bold formatting
        def replace_bold(match):
            content = match.group(1) or match.group(2)
            return f"<strong>{content}</strong>" if content else "**"

        # Apply italic formatting
        def replace_italic(match):
            content = match.group(1) or match.group(2)
            return f"<em>{content}</em>" if content else "*"

        formatted = self._bold_pattern.sub(replace_bold, text)
        return self._italic_pattern.sub(replace_italic, formatted)

    def _handle_line_breaks(self, text: str) -> str:
        """
        Process text to convert lines ending with two spaces to line breaks.

        Args:
            text: The text to process

        Returns:
            Text with appropriate line breaks added
        """
        lines = text.split('\n')
        processed_lines = []

        for line in lines:
            if line.endswith('  '):  # Line ends with exactly two spaces
                processed_lines.append(line.rstrip() + '<br />')
            else:
                processed_lines.append(line)

        return '\n'.join(processed_lines)

    def _identify_line_type(self, line: str) -> Tuple[str, any]:
        """
        Identify the type of a single line of text.

        Args:
            line: The line to identify

        Returns:
            Tuple of (line_type, content) where line_type is one of:
            'heading', 'unordered_list_item', 'ordered_list_item', 'blank', 'text'
        """
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

    def _classify_block(self, block: str) -> List[Tuple[str, any]]:
        """
        Analyze a block and break it down into its component elements.

        This approach processes the block line by line, identifying each line type
        and returning a sequence of elements that should be converted to HTML.

        Args:
            block: Block text to classify

        Returns:
            List of (element_type, content) tuples representing the parsed structure
        """
        lines = block.split('\n')
        elements = []

        # List item detection
        list_items = []
        current_list_type = None  # Either 'ordered' or 'unordered'
        prev_list_item_indent = -1

        # Paragraph tracking
        current_paragraph_lines = []

        def finalize_paragraph():
            """Helper to finalize the current paragraph"""
            nonlocal current_paragraph_lines
            if current_paragraph_lines:
                paragraph_text = '\n'.join(current_paragraph_lines)
                elements.append(('paragraph', paragraph_text))
                current_paragraph_lines = []

        def process_list_items():
            """Helper to process accumulated list items"""
            nonlocal list_items, current_list_type
            if list_items:
                # Determine if this is an ordered or unordered list
                is_ordered = current_list_type == 'ordered'
                elements.append((current_list_type + '_list', list_items))
                list_items = []
                current_list_type = None
                prev_list_item_indent = -1

        i = 0
        while i < len(lines):
            line = lines[i]
            line_type, content = self._identify_line_type(line)

            if line_type == 'heading':
                # Finalize any open structures
                process_list_items()
                finalize_paragraph()

                # Add the heading
                elements.append(('heading', content))

            elif line_type in ('unordered_list_item', 'ordered_list_item'):
                # Finalize any open paragraph
                finalize_paragraph()

                # Determine list type
                list_type = 'ordered' if line_type == 'ordered_list_item' else 'unordered'

                # If we're starting a new list or switching types at the same level
                indent, marker, item_content = content

                # If this is the first list item or if we're switching list types at the base level
                if not list_items or (indent == 0 and current_list_type != list_type):
                    # Process any previous list
                    process_list_items()
                    current_list_type = list_type

                # Add this item to our list
                list_items.append(content)
                prev_list_item_indent = indent

            elif line_type == 'blank':
                # Blank line in the middle of a list doesn't break the list
                # But it does end paragraphs
                finalize_paragraph()

            elif line_type == 'text':
                # If we have list items, we need to determine if this text
                # is part of the previous list item or a new paragraph
                if list_items and i > 0:
                    prev_line_type, _ = self._identify_line_type(lines[i-1])
                    if prev_line_type in ('unordered_list_item', 'ordered_list_item'):
                        # This text might be a continuation of the previous list item
                        # We'll add it to the content of the last list item
                        if list_items:
                            indent, marker, item_content = list_items[-1]
                            list_items[-1] = (indent, marker, item_content + "\n" + line)
                            i += 1
                            continue

                # Not continuation of a list item - process any lists we were building
                process_list_items()

                # Add to current paragraph
                current_paragraph_lines.append(line)

            i += 1

        # Don't forget any remaining structures
        process_list_items()
        finalize_paragraph()

        return elements

    def _convert_heading(self, level: int, content: str) -> str:
        """
        Convert a heading to HTML.

        Args:
            level: Heading level (1-6)
            content: Heading content

        Returns:
            HTML heading
        """
        # Apply inline formatting within the heading
        formatted_content = self._apply_inline_formatting(content)
        return f"<h{level}>{formatted_content}</h{level}>"

    def _process_list_items(self, items: List[Tuple[int, str, str]], ordered: bool = False) -> str:
        """
        Process a sequence of list items and generate HTML.

        Handles nested lists based on relative indentation rather than fixed boundaries.
        Supports both ordered and unordered lists, including mixed nested list types.

        Args:
            items: List of (indent, marker, content) tuples from _identify_line_type
            ordered: Whether the root list is ordered (ol) or unordered (ul)

        Returns:
            HTML for the list structure
        """
        if not items:
            return ""

        # Map indentation levels to list item information
        # Store items in a structure that preserves their hierarchy
        # Structure: [indent_level, marker_type, content, [children]]
        # where children is a list that will contain nested items

        # First determine the base indentation level (from the first item)
        base_indent = items[0][0]

        # Helper function to determine if a marker indicates an ordered list
        def is_ordered_marker(marker):
            if isinstance(marker, int) or (isinstance(marker, str) and marker.isdigit()):
                return True
            if isinstance(marker, str) and marker.strip().endswith('.') and marker.strip()[:-1].isdigit():
                return True
            return False

        # Create a hierarchical structure of the list items
        root_items = []
        item_stack = []  # Stack of (indent, items_list) to track hierarchy

        for indent, marker, content in items:
            # Determine if this is an ordered list item
            is_ordered_item = is_ordered_marker(marker)

            # Format the content
            formatted_content = self._apply_inline_formatting(content)

            # Create item entry [indent, is_ordered, content, children]
            item = [indent, is_ordered_item, formatted_content, []]

            # Find the appropriate parent for this item based on indentation
            while item_stack and item_stack[-1][0] >= indent:
                item_stack.pop()

            if not item_stack:  # This is a top-level item
                root_items.append(item)
                item_stack.append((indent, root_items))
            else:  # This is a child of the last item on the stack
                parent_indent, parent_items = item_stack[-1]
                parent_items[-1][3].append(item)  # Add to children of the last item
                item_stack.append((indent, item[3]))  # Push this item's children list

        # Generate HTML from the hierarchical structure
        def generate_html(items, parent_ordered=None):
            if not items:
                return ""

            # Determine if this list should be ordered or unordered
            # If all items are ordered markers, create an ordered list
            # Otherwise, create an unordered list (or use parent_ordered for the first level)
            if parent_ordered is not None:
                is_ordered = parent_ordered
            else:
                is_ordered = all(item[1] for item in items)

            # Create the list
            list_tag = "ol" if is_ordered else "ul"
            html = [f"<{list_tag}>"]

            # Add each item
            for _, item_is_ordered, content, children in items:
                html.append(f"<li>{content}")

                # Add nested list if there are children
                if children:
                    # Determine children's list type - if mixed, default based on first child
                    children_html = generate_html(children)
                    if children_html:
                        html.append(children_html)

                html.append("</li>")

            html.append(f"</{list_tag}>")
            return "\n".join(html)

        # Generate HTML for the root items, using the specified 'ordered' parameter
        return generate_html(root_items, ordered)

    def _convert_block(self, block: str) -> str:
        """
        Convert a single block of markdown to HTML.

        Args:
            block: The block of text to convert

        Returns:
            HTML converted text
        """
        # Get the sequence of elements in this block
        elements = self._classify_block(block)
        html_parts = []

        for element_type, content in elements:
            if element_type == 'heading':
                level, heading_text = content
                html_parts.append(self._convert_heading(level, heading_text))
            elif element_type == 'unordered_list':
                html_parts.append(self._process_list_items(content, ordered=False))
            elif element_type == 'ordered_list':
                html_parts.append(self._process_list_items(content, ordered=True))
            elif element_type == 'paragraph':
                # Process line breaks (lines ending with two spaces)
                content_with_breaks = self._handle_line_breaks(content)

                # Apply inline formatting
                formatted = self._apply_inline_formatting(content_with_breaks)

                # Wrap in paragraph tags
                html_parts.append(f"<p>{formatted}</p>")

        return "\n".join(html_parts)

    def convert_incremental(self, new_text: str) -> str:
        """
        Convert markdown to HTML incrementally as text is being added.

        Args:
            new_text: The updated text to convert

        Returns:
            HTML converted text
        """
        # Split into blocks
        new_blocks = self._split_into_blocks(new_text)

        # Handle empty input
        if not new_blocks:
            return ""

        # Initialize if this is the first conversion
        if not self._current_blocks:
            self._current_blocks = new_blocks
            self._block_converted = [False] * len(new_blocks)
            self._block_html = [""] * len(new_blocks)
            self._partial_last_block = new_blocks[-1]
        else:
            # Find how many blocks match from the beginning
            common_prefix_len = 0
            for i, (old, new) in enumerate(zip(self._current_blocks, new_blocks)):
                if old == new:
                    common_prefix_len = i + 1
                else:
                    break

            # Update our tracking arrays
            if len(new_blocks) > len(self._current_blocks):
                # New blocks added
                self._current_blocks = new_blocks
                # Keep converted status for existing blocks
                self._block_converted = (
                    self._block_converted +
                    [False] * (len(new_blocks) - len(self._block_converted))
                )
                # Keep HTML for existing blocks
                self._block_html = (
                    self._block_html +
                    [""] * (len(new_blocks) - len(self._block_html))
                )
                # Always consider the last block as "in progress"
                self._block_converted[-1] = False
            elif common_prefix_len < len(new_blocks):
                # Update with changed blocks
                self._current_blocks = new_blocks
                # Mark changed blocks as not converted
                for i in range(common_prefix_len, len(new_blocks)):
                    self._block_converted[i] = False
                    self._block_html[i] = ""
                # Trim arrays if blocks were removed
                if len(new_blocks) < len(self._block_converted):
                    self._block_converted = self._block_converted[:len(new_blocks)]
                    self._block_html = self._block_html[:len(new_blocks)]

            self._partial_last_block = new_blocks[-1]

        # Process all unconverted blocks
        for i, block in enumerate(self._current_blocks):
            if not self._block_converted[i]:
                self._block_html[i] = self._convert_block(block)
                # Mark all blocks except the last one as converted
                if i < len(self._current_blocks) - 1:
                    self._block_converted[i] = True

        # Combine all processed blocks
        print(f"\n".join(self._block_html))
        return "\n".join(self._block_html)

    def reset(self):
        """Reset the converter state."""
        self._current_blocks = []
        self._block_converted = []
        self._block_html = []
        self._partial_last_block = ""
