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
    focusing on headings, unordered lists, bold, and italic formatting with proper
    handling of block-level vs inline elements.
    """

    def __init__(self):
        """Initialize the markdown converter."""
        # Regular expressions for markdown elements
        self._heading_pattern = re.compile(r'^(#{1,6})\s+(.*?)(?:\s+#{1,6})?$', re.MULTILINE)
        self._bold_pattern = re.compile(r'\*\*(.*?)\*\*|\b__(.*?)__\b')
        self._italic_pattern = re.compile(r'\*([^*]+)\*|\b_([^_]+)_\b')

        # List item pattern - captures leading spaces, list marker, and content
        self._list_item_pattern = re.compile(r'^(\s*)([*+-])\s+(.*?)$', re.MULTILINE)

        # Track block state for incremental conversion
        self._current_blocks: List[str] = []
        self._block_converted: List[bool] = []
        self._block_html: List[str] = []

        # Track list state between block processing
        self._list_states: List[Dict] = []

        # Track the part of the last block we've processed so far
        self._partial_last_block = ""

    def _split_into_blocks(self, text: str) -> List[str]:
        """
        Split text into blocks based on blank lines.

        Args:
            text: The text to split

        Returns:
            List of text blocks
        """
        blocks = []
        current_block = []

        lines = text.split('\n')
        for line in lines:
            if line.strip() == "":
                if current_block:
                    blocks.append('\n'.join(current_block))
                    current_block = []
            else:
                current_block.append(line)

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

    def _classify_block(self, block: str) -> Tuple[str, str]:
        """
        Determine the type of a block and extract its content.

        Args:
            block: Block text to classify

        Returns:
            Tuple of (block_type, content) where block_type is one of:
            'heading', 'list_item', 'paragraph'
        """
        # Check for headings
        heading_match = self._heading_pattern.match(block)
        if heading_match:
            level = len(heading_match.group(1))
            content = heading_match.group(2).strip()
            return 'heading', (level, content)

        # Check if any line is a list item
        lines = block.split('\n')
        is_list = any(self._list_item_pattern.match(line) for line in lines)
        if is_list:
            return 'list', lines

        # Default to paragraph
        return 'paragraph', block

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

    def _process_list_items(self, lines: List[str]) -> str:
        """
        Process multiple lines with potential list items.

        Args:
            lines: List of text lines that may contain list items

        Returns:
            HTML for the list structure
        """
        # Track list structure
        html_parts = []
        list_stack = []

        for line in lines:
            match = self._list_item_pattern.match(line)
            if not match:
                # Handle non-list lines within a list block
                if list_stack:
                    # This is content for the previous list item (indented content)
                    html_parts.append(line)
                else:
                    # This shouldn't normally happen in a 'list' block
                    formatted_line = self._apply_inline_formatting(line)
                    html_parts.append(formatted_line)
                continue

            # Extract information from the match
            indent = len(match.group(1))
            marker = match.group(2)
            content = match.group(3)

            # Apply inline formatting to content
            formatted_content = self._apply_inline_formatting(content)

            # Calculate list level based on indentation
            level = indent // 2

            # Adjust list stack if needed
            if not list_stack:
                # Start first list
                html_parts.append("<ul>")
                list_stack.append(level)
            elif level > list_stack[-1]:
                # Start a nested list
                html_parts.append("<ul>")
                list_stack.append(level)
            elif level < list_stack[-1]:
                # Close deeper lists
                while list_stack and level < list_stack[-1]:
                    html_parts.append("</ul>")
                    list_stack.pop()

            # Add the list item
            html_parts.append(f"<li>{formatted_content}</li>")

        # Close any remaining open lists
        while list_stack:
            html_parts.append("</ul>")
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
        block_type, content = self._classify_block(block)

        if block_type == 'heading':
            level, heading_text = content
            return self._convert_heading(level, heading_text)
        elif block_type == 'list':
            return self._process_list_items(content)
        else:  # paragraph
            # Apply inline formatting
            formatted = self._apply_inline_formatting(content)
            # Wrap in paragraph tags if it's not already a HTML tag
            if not formatted.strip().startswith('<'):
                return f"<p>{formatted}</p>"
            return formatted

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
        self._list_states = []
