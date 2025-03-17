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

        # List tracking
        current_list_items = []
        current_list_type = None  # Either 'ordered' or 'unordered'
        in_list = False

        # Paragraph tracking
        current_paragraph_lines = []

        for i, line in enumerate(lines):
            line_type, content = self._identify_line_type(line)

            # Finish current paragraph if necessary
            def finalize_paragraph():
                nonlocal current_paragraph_lines
                if current_paragraph_lines:
                    paragraph_text = '\n'.join(current_paragraph_lines)
                    elements.append(('paragraph', paragraph_text))
                    current_paragraph_lines = []

            # Finish current list if necessary
            def finalize_list():
                nonlocal current_list_items, current_list_type, in_list
                if in_list:
                    elements.append((current_list_type + '_list', current_list_items))
                    current_list_items = []
                    in_list = False

            if line_type == 'heading':
                # Finalize any open structures
                finalize_list()
                finalize_paragraph()

                # Add the heading
                elements.append(('heading', content))

            elif line_type in ('unordered_list_item', 'ordered_list_item'):
                # Finalize any open paragraph
                finalize_paragraph()

                # If we're switching list types, finalize the current list
                list_type = 'ordered' if line_type == 'ordered_list_item' else 'unordered'

                if in_list and current_list_type != list_type:
                    elements.append((current_list_type + '_list', current_list_items))
                    current_list_items = []

                # Start or continue a list
                in_list = True
                current_list_type = list_type
                current_list_items.append(content)

            elif line_type == 'blank':
                # Blank line - end current paragraph
                finalize_paragraph()
                # We don't finalize lists on blank lines - they can contain blank lines

            elif line_type == 'text':
                # If we were in a list, finalize it
                finalize_list()

                # Add to current paragraph
                current_paragraph_lines.append(content)

                # If this is the last line, finalize the paragraph
                if i == len(lines) - 1:
                    finalize_paragraph()

        # Don't forget any remaining structures
        finalize_list()
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
        Supports both ordered and unordered lists.

        Args:
            items: List of (indent, marker, content) tuples from _identify_line_type
            ordered: Whether to generate an ordered (ol) or unordered (ul) list

        Returns:
            HTML for the list structure
        """
        if not items:
            return ""

        # Track list structure
        html_parts = []
        list_stack = []  # Stack of tuples: (indentation level, list type)

        # Track indentation of the first list item to determine relative indentation
        base_indent = items[0][0]
        current_levels = {base_indent: 0}  # Maps indentation to nesting level

        for indent, marker, content in items:
            # Apply inline formatting to content
            formatted_content = self._apply_inline_formatting(content)

            # Determine the nesting level based on relative indentation
            if indent not in current_levels:
                # Find the closest lower indentation level
                prev_indents = [i for i in current_levels.keys() if i < indent]
                if prev_indents:
                    closest_indent = max(prev_indents)
                    # This is a new level one deeper than the closest lower level
                    current_levels[indent] = current_levels[closest_indent] + 1
                else:
                    # If no lower indent exists, this must be a new root level
                    current_levels[indent] = 0

            level = current_levels[indent]

            # Adjust list stack if needed
            current_stack_level = len(list_stack) - 1 if list_stack else -1

            if current_stack_level < level:
                # Need to open new lists
                while current_stack_level < level:
                    # Determine if this should be an ordered or unordered list
                    # For now, we'll use the same type as the parent list
                    # This could be enhanced to detect type changes
                    list_tag = "ol" if ordered else "ul"
                    html_parts.append(f"<{list_tag}>")
                    list_stack.append((level, ordered))
                    current_stack_level += 1
            elif current_stack_level > level:
                # Need to close lists
                while list_stack and current_stack_level > level:
                    _, is_ordered = list_stack[-1]
                    list_tag = "ol" if is_ordered else "ul"
                    html_parts.append(f"</{list_tag}>")
                    list_stack.pop()
                    current_stack_level -= 1

            # Add the list item
            html_parts.append(f"<li>{formatted_content}</li>")

        # Close any remaining open lists
        while list_stack:
            _, is_ordered = list_stack[-1]
            list_tag = "ol" if is_ordered else "ul"
            html_parts.append(f"</{list_tag}>")
            list_stack.pop()

        return "\n".join(html_parts)

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
        return "\n".join(self._block_html)

    def reset(self):
        """Reset the converter state."""
        self._current_blocks = []
        self._block_converted = []
        self._block_html = []
        self._partial_last_block = ""
